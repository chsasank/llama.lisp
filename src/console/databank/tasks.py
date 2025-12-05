from etl.sources import MssqlSource, OracleSource, PostgresSource
from etl.targets import ClickhouseTarget, PostgresTarget

from .models import ETLConfiguration


def _get_etl_src(etl_config):
    source_db_type = etl_config.source_database.database_type
    source_conn = etl_config.source_database.connection_config

    if source_db_type == ETLConfiguration.DBTypes.POSTGRES:
        return PostgresSource(
            {
                "connection": source_conn,
                "table": etl_config.source_table,
            }
        )
    elif source_db_type == ETLConfiguration.DBTypes.MSSQL:
        return MssqlSource(
            {
                "connection": source_conn,
                "table": etl_config.source_table,
            }
        )
    elif source_db_type == ETLConfiguration.DBTypes.ORACLE:
        return OracleSource(
            {
                "connection": source_conn,
                "table": etl_config.source_table,
            }
        )
    else:
        raise ValueError(f"Unknown source {etl_config.source_database.database_type}")


def _get_etl_tgt(etl_config):
    target_db_type = etl_config.target_database.database_type
    target_conn = etl_config.target_database.connection_config
    if target_db_type == ETLConfiguration.DBTypes.POSTGRES:
        return PostgresTarget(
            {
                "connection": target_conn,
                "table": etl_config.target_table,
            }
        )
    elif target_db_type == ETLConfiguration.DBTypes.CLICKHOUSE:
        return ClickhouseTarget(
            {
                "connection": target_conn,
                "table": etl_config.target_table,
            }
        )
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
