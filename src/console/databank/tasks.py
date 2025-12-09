import json
import logging

from etl.common.state_manager import StateManagerDriver
from etl.sources import MssqlSource, OracleSource, PostgresSource
from etl.targets import ClickhouseTarget, PostgresTarget

from .models import DatabaseConfiguration, ETLConfiguration

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
            return self.etl_config.replication_state["replication_value"]
        except TypeError:
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
        raise ValueError(f"Unknown source {etl_config.source_database.database_type}")


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
        raise ValueError(f"Unknown source {etl_config.source_database.database_type}")


def run_etl(etl_config_id):
    etl_config = ETLConfiguration.objects.get(id=etl_config_id)
    src = _get_etl_src(etl_config)
    tgt = _get_etl_tgt(etl_config)

    etl_schema = src.get_etl_schema()
    tgt.ensure_schema(etl_schema)

    batches = src.stream_batches()
    for batch in batches:
        tgt.load_batch(batch, etl_schema)
        logger.info(f"transferred {len(batch)} rows")
