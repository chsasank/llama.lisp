from django.shortcuts import render

# Create your views here.
from django.shortcuts import render, redirect, get_object_or_404
from django.http import JsonResponse, HttpResponse
from .models import DatabaseConfiguration, ETLConfiguration
from .forms import DatabaseConfigurationForm, ETLConfigurationForm
from .tasks import run_etl

# Home
def home(request):
    return render(request, "databank/home.html")


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
            form.save()
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
            form.save()
            return redirect("etl_list")
    else:
        form = ETLConfigurationForm(instance=etl)
    return render(request, "databank/etl_form.html", {"form": form})

def etl_delete(request, pk):
    # Remove ETL pipeline definition
    etl = get_object_or_404(ETLConfiguration, pk=pk)
    etl.delete()
    return redirect("etl_list")


# RUN ETL FROM UI
def run_single_etl(request, etl_id):
    # execute one ETL pipeline manually from the UI
    try:
        run_etl(etl_id)
        status = "success"
        message = f"ETL {etl_id} completed successfully."
    except Exception as e:
        status = "error"
        message = f"ETL {etl_id} failed: {str(e)}"

    return render(request, "databank/etl_result.html", {
        "status": status,
        "message": message,
        "etl_id": etl_id,
    })


def run_all_etls(request):
    # execute every ETL config in sequence and returns a JSON response so UI or API can consume the results
    etls = ETLConfiguration.objects.all()
    results = []
    for e in etls:
        try:
            run_etl(e.id)
            results.append({"id": e.id, "status": "ok"})
        except Exception as ex:
            results.append({"id": e.id, "status": "error", "error": str(ex)})
    return JsonResponse({"results": results})
