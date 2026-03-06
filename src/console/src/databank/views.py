import csv
import json
import logging
from urllib.parse import urlencode

import clickhouse_connect
import oracledb
import psycopg
import pyodbc
from django.conf import settings
from django.contrib.auth.decorators import login_not_required
from django.http import HttpResponse, JsonResponse

# Create your views here.
from django.shortcuts import get_object_or_404, redirect, render
from django.views.decorators.csrf import csrf_exempt
from etl.sources import MssqlSource, OracleSource, PostgresSource

from task_manager.models import Graph, Task

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

    running_tasks = 0
    failed_tasks = 0
    success_tasks = 0
    queued_tasks = 0
    not_scheduled_tasks = 0

    for etl in ETLConfiguration.objects.all():

        graph_name = f"etl_{etl.id}"

        try:
            graph = Graph.objects.get(name=graph_name)
            latest_task = graph.tasks.order_by("-updated_at").first()

            if not latest_task:
                not_scheduled_tasks += 1
                continue

            if latest_task.state == Task.TaskState.RUNNING:
                running_tasks += 1

            elif latest_task.state == Task.TaskState.FAILED:
                failed_tasks += 1

            elif latest_task.state == Task.TaskState.SUCCESS:
                success_tasks += 1

            elif latest_task.state == Task.TaskState.QUEUED:
                queued_tasks += 1

        except Graph.DoesNotExist:
            continue

    # Recently executed ETLs
    recent_tasks = Task.objects.filter(args__has_key="etl_config_id").order_by(
        "-updated_at"
    )[:5]

    recent_etls = []

    for task in recent_tasks:
        etl_id = task.args.get("etl_config_id")

        try:
            etl = ETLConfiguration.objects.select_related(
                "source_database", "target_database"
            ).get(id=etl_id)

            etl.last_state = task.state
            etl.last_run = task.updated_at

            recent_etls.append(etl)

        except ETLConfiguration.DoesNotExist:
            continue

    # Active scheduled ETLs
    scheduled_tasks = Task.objects.filter(periodic_interval__isnull=False).order_by(
        "next_run_at"
    )[:5]

    return render(
        request,
        "databank/dashboard.html",
        {
            "database_count": database_count,
            "etl_count": etl_count,
            "source_count": source_count,
            "target_count": target_count,
            "running_tasks": running_tasks,
            "failed_tasks": failed_tasks,
            "success_tasks": success_tasks,
            "queued_tasks": queued_tasks,
            "recent_etls": recent_etls,
            "scheduled_tasks": scheduled_tasks,
            "not_scheduled_tasks": not_scheduled_tasks,
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
    for e in etls:
        # Logs URL
        query = f"LogAttributes['source_table'] IN ('{e.source_table}')"

        params = {
            "source": settings.LOGS_SOURCE_ID,
            "filters": f'[{{"type":"sql","condition":"{query}"}}]',
            "from": "now-60m",
            "to": "now",
            "isLive": "true",
        }

        e.logs_url = f"{settings.LOGS_BASE_URL}/search?{urlencode(params)}"

        graph_name = f"etl_{e.id}"
        try:
            graph = Graph.objects.get(name=graph_name)
            latest_task = graph.tasks.order_by("-updated_at").first()

            if latest_task:
                e.current_status = latest_task.state
                e.last_run = latest_task.updated_at
                e.next_run = latest_task.next_run_at
            else:
                e.current_status = "no_task"
        except Graph.DoesNotExist:
            e.current_status = "not_scheduled"
    return render(request, "databank/etl_list.html", {"etls": etls})


def etl_create(request):
    if request.method == "POST":
        form = ETLConfigurationForm(request.POST)
        if form.is_valid():
            etl = form.save(commit=False)
            mode = form.cleaned_data.get("replication_mode")
            if mode == "full_refresh":
                etl.replication_key = None
                etl.replication_state = None
            elif mode == "one_time":
                etl.replication_key = None
                etl.replication_state = None
                etl.run_interval = None
            etl.save()
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
            etl = form.save(commit=False)
            mode = form.cleaned_data.get("replication_mode")
            if mode == "full_refresh":
                etl.replication_key = None
                etl.replication_state = None
            elif mode == "one_time":
                etl.replication_key = None
                etl.replication_state = None
                etl.run_interval = None
            etl.save()
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


def etl_status_api(request):

    data = []

    for etl in ETLConfiguration.objects.all():

        graph_name = f"etl_{etl.id}"

        try:
            graph = Graph.objects.get(name=graph_name)
            latest_task = graph.tasks.order_by("-updated_at").first()

            state = latest_task.state if latest_task else "idle"

        except Graph.DoesNotExist:
            state = "not_scheduled"

        data.append({"id": etl.id, "state": state})

    return JsonResponse(data, safe=False)


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
