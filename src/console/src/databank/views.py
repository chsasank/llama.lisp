import logging
from django.http import JsonResponse

from django.shortcuts import get_object_or_404, redirect, render

from .forms import DatabaseConfigurationForm, ETLConfigurationForm
from .models import DatabaseConfiguration, ETLConfiguration
from .tasks import delete_etl_graph, recreate_etl_task
from etl.sources import MssqlSource, OracleSource, PostgresSource
from etl.targets import ClickhouseTarget, PostgresTarget

logger = logging.getLogger(__name__)


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

def test_database(request, pk):
    db = get_object_or_404(DatabaseConfiguration, pk=pk)

    cfg = {
        "connection": db.connection_config,   # driver expects this format
        "table": "dummy.schema",              # required but not used
    }

    try:
        # Picks correct driver automatically
        driver = None

        if db.etl_type == "source":
            if db.database_type == "postgres":
                driver = PostgresSource(cfg)
            elif db.database_type == "mssql":
                driver = MssqlSource(cfg)
            elif db.database_type == "oracle":
                driver = OracleSource(cfg)
        elif db.etl_type == "target":
            if db.database_type == "postgres":
                driver = PostgresTarget(cfg)
            elif db.database_type == "clickhouse":
                driver = ClickhouseTarget(cfg)

        if driver is None:
            return JsonResponse({
                "success": False,
                "message": f"Unsupported database type: {db.database_type}"
            })

        driver._connect()

        return JsonResponse({
            "success": True,
            "message": f"{db.database_type.upper()} connection OK"
        })

    except Exception as ex:
        return JsonResponse({
            "success": False,
            "message": str(ex)
        })