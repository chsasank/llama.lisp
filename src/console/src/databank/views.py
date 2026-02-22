import csv
import json
import logging

import clickhouse_connect
import oracledb
import psycopg
import pyodbc
from django.contrib.auth.decorators import login_not_required
from django.http import HttpResponse, JsonResponse

# Create your views here.
from django.shortcuts import get_object_or_404, redirect, render
from django.views.decorators.csrf import csrf_exempt
from etl.sources import MssqlSource, OracleSource, PostgresSource

from .forms import DatabaseConfigurationForm, ETLConfigurationForm
from .models import DatabaseConfiguration, ETLConfiguration
from .tasks import delete_etl_graph, recreate_etl_task

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
    databases = DatabaseConfiguration.objects.all().order_by("-id")
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


# DOWNLOAD ETLS
def download_etls(request):
    response = HttpResponse(content_type="text/csv")
    response["Content-Disposition"] = 'attachment; filename="etls.csv"'

    writer = csv.writer(response)

    fields = [field.name for field in ETLConfiguration._meta.fields]
    writer.writerow(fields)

    for obj in ETLConfiguration.objects.all():
        writer.writerow([getattr(obj, field) for field in fields])

    return response


@csrf_exempt
def test_connection(request):
    if request.method == "POST":
        try:
            data = json.loads(request.body)

            db_type = data.get("database_type")
            etl_type = data.get("etl_type")

            try:
                config = json.loads(data.get("connection_config"))
            except Exception:
                return JsonResponse({"success": False, "error": "Invalid JSON format"})

            # Validation rules
            if etl_type == "source" and db_type == "clickhouse":
                return JsonResponse(
                    {"success": False, "error": "ClickHouse cannot be used as SOURCE"}
                )

            if etl_type == "target" and db_type in ["mssql", "oracle"]:
                return JsonResponse(
                    {
                        "success": False,
                        "error": f"{db_type.upper()} cannot be used as TARGET",
                    }
                )

            # Connections
            if db_type == "postgres":
                conn = psycopg.connect(
                    host=config["host"],
                    port=config["port"],
                    user=config["user"],
                    password=config["password"],
                    dbname=config["database"],
                    connect_timeout=3,
                )
                conn.close()

            elif db_type == "clickhouse":
                client = clickhouse_connect.get_client(
                    host=config["host"],
                    port=config["port"],
                    user=config["user"],
                    password=config["password"],
                    database=config["database"],
                )
                client.query("SELECT 1")

            elif db_type == "mssql":
                conn_str = (
                    f"DRIVER={{ODBC Driver 18 for SQL Server}};"
                    f"SERVER={config['host']},{config['port']};"
                    f"DATABASE={config['database']};"
                    f"UID={config['user']};"
                    f"PWD={config['password']};"
                    f"Encrypt=yes;"
                    f"TrustServerCertificate=yes;"
                )
                conn = pyodbc.connect(conn_str, timeout=3)
                conn.close()

            elif db_type == "oracle":
                dsn = oracledb.makedsn(
                    config["host"], int(config["port"]), service_name=config["service"]
                )
                conn = oracledb.connect(
                    user=config["user"], password=config["password"], dsn=dsn
                )
                conn.close()

            return JsonResponse({"success": True})

        except Exception as e:
            return JsonResponse({"success": False, "error": str(e).split("\n")[0]})

    return JsonResponse({"success": False, "error": "Invalid request"})


def toggle_etl_status(request, pk):
    etl = get_object_or_404(ETLConfiguration, pk=pk)

    if etl.status == ETLConfiguration.Status.ACTIVE:
        etl.status = ETLConfiguration.Status.PAUSED
        etl.save()
        delete_etl_graph(pk)

    else:
        etl.status = ETLConfiguration.Status.ACTIVE
        etl.save()
        recreate_etl_task(etl_id=pk)

    return redirect("etl_list")


def get_tables(request, db_id):
    db = DatabaseConfiguration.objects.get(id=db_id)
    config = db.connection_config

    if db.database_type == "postgres":
        source = PostgresSource({"connection": config})
    elif db.database_type == "mssql":
        source = MssqlSource({"connection": config})
    elif db.database_type == "oracle":
        source = OracleSource({"connection": config})
    else:
        return JsonResponse({"error": "Unsupported DB"}, status=400)

    tables = source.get_all_tables()

    return JsonResponse({"tables": tables})
