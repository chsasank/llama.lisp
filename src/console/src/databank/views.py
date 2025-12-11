import logging
from django.http import JsonResponse
import json

# Create your views here.
from django.shortcuts import get_object_or_404, redirect, render

from .forms import DatabaseConfigurationForm, ETLConfigurationForm
from .models import DatabaseConfiguration, ETLConfiguration
from .tasks import delete_etl_graph, recreate_etl_task

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
    cfg = db.connection_config or {}
    host = cfg.get("host")
    port = cfg.get("port")

    if not host or not port:
        return JsonResponse({"success": False, "message": "Missing host or port"})

    try:
        # POSTGRES (psycopg)
        if db.database_type == "postgres":
            try:
                import psycopg
            except ImportError:
                return JsonResponse({"success": False, "message": "psycopg is not installed"})

            conn = psycopg.connect(
                host=host,
                port=port,
                user=cfg.get("user"),
                password=cfg.get("password"),
                dbname=cfg.get("database"),
                connect_timeout=5
            )
            conn.close()
            return JsonResponse({"success": True, "message": "POSTGRES connection OK"})

        # CLICKHOUSE (clickhouse-connect)
        elif db.database_type == "clickhouse":
            try:
                from clickhouse_connect import get_client
            except ImportError:
                return JsonResponse({"success": False, "message": "clickhouse-connect is not installed"})

            client = get_client(
                host=host,
                port=int(port),
                username=cfg.get("user") or "default",
                password=cfg.get("password") or "",
                database=cfg.get("database") or "default",
            )
            client.query("SELECT 1")
            return JsonResponse({"success": True, "message": "ClickHouse connection OK"})

        # MSSQL (pyodbc)
        elif db.database_type == "mssql":
            try:
                import pyodbc
            except ImportError:
                return JsonResponse({"success": False, "message": "pyodbc is not installed"})

            conn_str = (
                f"DRIVER={{ODBC Driver 18 for SQL Server}};"
                f"SERVER={host},{port};"
                f"UID={cfg.get('user')};"
                f"PWD={cfg.get('password')};"
                f"DATABASE={cfg.get('database')};"
                "Encrypt=no;"
                "TrustServerCertificate=yes;"
            )
            conn = pyodbc.connect(conn_str, timeout=5)
            conn.close()
            return JsonResponse({"success": True, "message": "MSSQL connection OK"})

        # ORACLE (oracledb)
        elif db.database_type == "oracle":
            try:
                import oracledb
            except ImportError:
                return JsonResponse({"success": False, "message": "oracledb is not installed"})

            conn = oracledb.connect(
                user=cfg["user"],
                password=cfg["password"],
                host=cfg["host"],
                port=cfg.get("port", 1521),
                service_name=cfg["service"],
            )
            conn.close()
            return JsonResponse({"success": True, "message": "ORACLE connection OK"})

        # UNSUPPORTED DB TYPE
        else:
            return JsonResponse({"success": False, "message": f"Unsupported DB type: {db.database_type}"})

    except Exception as ex:
        return JsonResponse({"success": False, "message": str(ex)})
