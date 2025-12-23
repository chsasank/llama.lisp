import json
import logging

from etl.common.state_manager import StateManagerDriver
from etl.sources import MssqlSource, OracleSource, PostgresSource
from etl.targets import ClickhouseTarget, PostgresTarget
from task_manager.models import Graph, Task

from .models import DatabaseConfiguration, ETLConfiguration
from datetime import datetime, timedelta
from django.utils.dateparse import parse_datetime

logger = logging.getLogger(__name__)


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
        self.etl_config.replication_state = {
            "replication_value": replication_value,
        }
        self.etl_config.save()

    def get_state(self):
        try:
            state = self.etl_config.replication_state
            if not state:
                return None

            value = state.get("replication_value")
            if value is None:
                return None

            backfill = state.get("backfill")
            if not backfill:
                backfill = 0

            if isinstance(value, datetime):
                ts = value
            else:
                ts = parse_datetime(value)
                if ts is None:
                    return value

            ts = ts - timedelta(seconds=backfill)

            return ts.strftime("%Y-%m-%d %H:%M:%S")

        except (TypeError, KeyError):
            return None


def _get_etl_src(etl_config):
    source_db_type = etl_config.source_database.database_type
    source_conn = etl_config.source_database.connection_config
    if etl_config.replication_key:
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

    logger.info("=== START ETL ===")
    logger.info(f"Source DB: {etl_config.source_database}")
    logger.info(f"Target DB: {etl_config.target_database}")
    logger.info(f"Source Table: {etl_config.source_table}")
    logger.info(f"Target Table: {etl_config.target_table}")

    src = _get_etl_src(etl_config)
    tgt = _get_etl_tgt(etl_config)

    etl_schema = src.get_etl_schema()
    logger.info(f"Source schema: {etl_schema}")

    tgt.ensure_schema(etl_schema)
    logger.info("Schema ensured on target.")

    batch_count = 0

    for batch in src.stream_batches():
        batch_count += 1
        logger.info(f"Loading batch {batch_count} with {len(batch)} rows...")
        tgt.load_batch(batch, etl_schema)

    logger.info("=== END ETL ===")
    logger.info(f"Total batches: {batch_count}")


def recreate_etl_task(etl_id):
    graph_name = f"etl_{etl_id}"

    # it will create a new graph
    graph, _ = Graph.objects.get_or_create(name=graph_name)

    # it will delete all old tasks
    for t in graph.tasks.all():
        t.delete()

    etl_config = ETLConfiguration.objects.get(id=etl_id)
    # again it will create a new task
    Task.create_task(
        fn=run_etl,
        args={"etl_config_id": etl_id},
        graph=graph,
        periodic_interval=etl_config.run_interval,
    )

    logger.info(f"[ETL] Task graph recreated for ETL {etl_id}")


def delete_etl_graph(etl_id):
    graph_name = f"etl_{etl_id}"
    try:
        graph = Graph.objects.get(name=graph_name)
        graph.delete()
        logger.info(f"[ETL] Deleted task graph for ETL {etl_id}")
    except Graph.DoesNotExist:
        pass
