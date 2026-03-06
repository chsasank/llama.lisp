import json
import logging
from datetime import timedelta

from django.utils.dateparse import parse_datetime
from etl.common.state_manager import StateManagerDriver
from etl.sources import MssqlSource, OracleSource, PostgresSource
from etl.targets import ClickhouseTarget, PostgresTarget
from opentelemetry._logs import get_logger

from databank.utils.logging import log_with_etl
from task_manager.models import Graph, Task

from .models import DatabaseConfiguration, ETLConfiguration

logger = logging.getLogger(__name__)
otel_logger = get_logger(__name__)


class DBStateManager(StateManagerDriver):
    def __init__(self, etl_config):
        self.etl_config = etl_config
        self.replication_key = self.etl_config.replication_key
        super().__init__()

    def set_state(self, replication_value):
        try:
            json.dumps(replication_value)
        except TypeError:
            replication_value = str(replication_value)

        state = self.etl_config.replication_state or {}
        state["replication_value"] = replication_value
        self.etl_config.replication_state = state
        self.etl_config.save()

    def get_state(self):
        state = self.etl_config.replication_state or {}
        if "replication_value" in state:
            value = state["replication_value"]
            if "backfill" in state:
                backfill = state["backfill"]
                ts = parse_datetime(value)
                assert (
                    ts is not None
                ), "{value} not in time format. backfill assumes time"
                ts = ts - timedelta(seconds=backfill)
                ts = ts.replace(microsecond=0)
                return str(ts)
            else:
                return value


def _get_etl_src(etl_config):
    source_db_type = etl_config.source_database.database_type
    source_conn = etl_config.source_database.connection_config
    if etl_config.replication_mode == "incremental":
        state_manager = DBStateManager(etl_config)
    else:
        state_manager = None

    src_config = {
        "connection": source_conn,
        "table": etl_config.source_table,
    }

    if source_db_type == DatabaseConfiguration.DBTypes.POSTGRES:
        return PostgresSource(src_config, state_manager)
    elif source_db_type == DatabaseConfiguration.DBTypes.MSSQL:
        return MssqlSource(src_config, state_manager)
    elif source_db_type == DatabaseConfiguration.DBTypes.ORACLE:
        return OracleSource(src_config, state_manager)
    else:
        raise ValueError(f"Unknown source {source_db_type}")


def _get_etl_tgt(etl_config):
    target_db_type = etl_config.target_database.database_type
    target_conn = etl_config.target_database.connection_config
    target_config = {
        "connection": target_conn,
        "table": etl_config.target_table,
    }
    if target_db_type == DatabaseConfiguration.DBTypes.POSTGRES:
        return PostgresTarget(target_config)
    elif target_db_type == DatabaseConfiguration.DBTypes.CLICKHOUSE:
        return ClickhouseTarget(target_config)
    else:
        raise ValueError(f"Unknown target {target_db_type}")


def run_etl(etl_config_id):
    etl_config = ETLConfiguration.objects.get(id=etl_config_id)

    if etl_config.status == "paused":
        log_with_etl(logger, "ETL is paused. Skipping...", etl_config)
        return

    log_with_etl(logger, "ETL started", etl_config)
    log_with_etl(logger, f"Source DB: {etl_config.source_database}", etl_config)
    log_with_etl(logger, f"Target DB: {etl_config.target_database}", etl_config)
    log_with_etl(logger, f"Source Table: {etl_config.source_table}", etl_config)
    log_with_etl(logger, f"Target Table: {etl_config.target_table}", etl_config)

    src = _get_etl_src(etl_config)
    tgt = _get_etl_tgt(etl_config)

    # FULL REFRESH LOGIC
    if etl_config.replication_mode == "full_refresh":
        log_with_etl(
            logger, "Dropping target table before run", etl_config, level="warn"
        )
        try:
            tgt.drop_table()
        except Exception as e:
            log_with_etl(logger, f"Drop failed (ignored): {str(e)}", etl_config)

    etl_schema = src.get_etl_schema()
    log_with_etl(logger, f"Schema detected: {etl_schema}", etl_config)

    tgt.ensure_schema(etl_schema)
    log_with_etl(logger, "Schema ensured on target", etl_config)

    batch_count = 0

    try:
        for batch in src.stream_batches():
            batch_count += 1

            log_with_etl(
                logger,
                f"Processing batch {batch_count}",
                etl_config,
                extra={"rows": len(batch)},
            )

            tgt.load_batch(batch, etl_schema)

        log_with_etl(
            logger,
            "ETL completed successfully",
            etl_config,
            extra={"total_batches": batch_count},
        )

    except Exception as e:
        log_with_etl(
            logger,
            f"ETL failed: {str(e)}",
            etl_config,
            level="error",
        )
        raise


def recreate_etl_task(etl_id):
    graph_name = f"etl_{etl_id}"

    # it will create a new graph
    graph, _ = Graph.objects.get_or_create(name=graph_name)

    # it will delete all old tasks
    for t in graph.tasks.all():
        t.delete()

    etl_config = ETLConfiguration.objects.get(id=etl_id)
    # again it will create a new task
    if etl_config.replication_mode == "one_time":
        Task.create_task(
            fn=run_etl,
            args={"etl_config_id": etl_id},
            graph=graph,
            periodic_interval=None,
        )

    elif etl_config.run_interval:
        Task.create_task(
            fn=run_etl,
            args={"etl_config_id": etl_id},
            graph=graph,
            periodic_interval=etl_config.run_interval,
        )

    log_with_etl(logger, "Task graph recreated", etl_config)


def delete_etl_graph(etl_id):
    graph_name = f"etl_{etl_id}"
    try:
        graph = Graph.objects.get(name=graph_name)
        graph.tasks.all().delete()  # ensure tasks removed
        graph.delete()
        etl_config = ETLConfiguration.objects.get(id=etl_id)
        log_with_etl(logger, "ETL graph deleted", etl_config)
    except Graph.DoesNotExist:
        pass
