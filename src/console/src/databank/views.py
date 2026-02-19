import logging
import csv

# Create your views here.
from django.shortcuts import get_object_or_404, redirect, render

from .forms import DatabaseConfigurationForm, ETLConfigurationForm
from .models import DatabaseConfiguration, ETLConfiguration
from .tasks import delete_etl_graph, recreate_etl_task
from django.contrib.auth.decorators import login_not_required
from django.http import HttpResponse

logger = logging.getLogger(__name__)


# Home
@login_not_required
def home(request):
    return render(request, "databank/home.html")

# Dashboard
def dashboard(request):
    databases = DatabaseConfiguration.objects.all()
    etls = ETLConfiguration.objects.all()

    database_count = databases.count()
    etl_count = etls.count()

    source_count = databases.filter(etl_type="source").count()
    target_count = databases.filter(etl_type="target").count()

    recent_etls = ETLConfiguration.objects.select_related(
        "source_database", "target_database"
    ).order_by("-id")[:5]

    return render(
        request,
        "databank/dashboard.html",
        {
            "database_count": database_count,
            "etl_count": etl_count,
            "source_count": source_count,
            "target_count": target_count,
            "recent_etls": recent_etls,
        },
    )

# DATABASE CONFIG UI
def database_list(request):
    # Fetch all configured databases to show in UI
    databases = DatabaseConfiguration.objects.all()
    return render(request, "databank/database_list.html", {"databases": databases})


def database_create(request):
    if request.method == "POST":
        form = DatabaseConfigurationForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect("database_list")
    else:
        form = DatabaseConfigurationForm()
    return render(request, "databank/database_form.html", {"form": form})


def database_edit(request, pk):
    # Load database config or return 404 if not found
    db = get_object_or_404(DatabaseConfiguration, pk=pk)
    if request.method == "POST":
        form = DatabaseConfigurationForm(request.POST, instance=db)
        if form.is_valid():
            form.save()
            return redirect("database_list")
    else:
        form = DatabaseConfigurationForm(instance=db)
    return render(request, "databank/database_form.html", {"form": form})


def database_delete(request, pk):
    # Delete the selected database config immediately
    db = get_object_or_404(DatabaseConfiguration, pk=pk)
    db.delete()
    return redirect("database_list")


# ETL CONFIG UI
def etl_list(request):
    # Show all ETL pipelines configured in the system
    etls = ETLConfiguration.objects.all()
    return render(request, "databank/etl_list.html", {"etls": etls})


def etl_create(request):
    if request.method == "POST":
        form = ETLConfigurationForm(request.POST)
        if form.is_valid():
            etl = form.save()
            recreate_etl_task(etl_id=etl.id)
            return redirect("etl_list")
    else:
        form = ETLConfigurationForm()
    return render(request, "databank/etl_form.html", {"form": form})


def etl_edit(request, pk):
    # Load ETL config for editing
    etl = get_object_or_404(ETLConfiguration, pk=pk)
    if request.method == "POST":
        form = ETLConfigurationForm(request.POST, instance=etl)
        if form.is_valid():
            etl = form.save()
            recreate_etl_task(etl_id=etl.id)
            return redirect("etl_list")
    else:
        form = ETLConfigurationForm(instance=etl)
    return render(request, "databank/etl_form.html", {"form": form})


def etl_delete(request, pk):
    # Remove ETL pipeline definition
    delete_etl_graph(pk)
    etl = get_object_or_404(ETLConfiguration, pk=pk)
    etl.delete()
    return redirect("etl_list")


# DOWNLOAD DATABASES
def download_databases(request):
    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="databases.csv"'

    writer = csv.writer(response)

    fields = [field.name for field in DatabaseConfiguration._meta.fields]
    writer.writerow(fields)

    for obj in DatabaseConfiguration.objects.all():
        writer.writerow([getattr(obj, field) for field in fields])

    return response


# DOWNLOAD ETLS
def download_etls(request):
    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="etls.csv"'

    writer = csv.writer(response)

    fields = [field.name for field in ETLConfiguration._meta.fields]
    writer.writerow(fields)

    for obj in ETLConfiguration.objects.all():
        writer.writerow([getattr(obj, field) for field in fields])

    return response


# DOWNLOAD SOURCES
def download_sources(request):
    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="sources.csv"'

    writer = csv.writer(response)

    qs = DatabaseConfiguration.objects.filter(etl_type="source")
    fields = [field.name for field in DatabaseConfiguration._meta.fields]

    writer.writerow(fields)

    for obj in qs:
        writer.writerow([getattr(obj, field) for field in fields])

    return response


# DOWNLOAD TARGETS
def download_targets(request):
    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="targets.csv"'

    writer = csv.writer(response)

    qs = DatabaseConfiguration.objects.filter(etl_type="target")
    fields = [field.name for field in DatabaseConfiguration._meta.fields]

    writer.writerow(fields)

    for obj in qs:
        writer.writerow([getattr(obj, field) for field in fields])

    return response
