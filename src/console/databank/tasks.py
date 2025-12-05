from databank.etl.sources import MssqlSource, OracleSource, PostgresSource
from databank.etl.targets import ClickhouseTarget, PostgresTarget

from .models import ETLConfiguration, DatabaseConfiguration


def _get_etl_src(etl_config):
    source_db_type = etl_config.source_database.database_type
    source_conn = etl_config.source_database.connection_config

    if source_db_type == DatabaseConfiguration.DBTypes.POSTGRES:
        return PostgresSource(
            {
                "connection": source_conn,
                "table": etl_config.source_table,
            }
        )
    elif source_db_type == DatabaseConfiguration.DBTypes.MSSQL:
        return MssqlSource(
            {
                "connection": source_conn,
                "table": etl_config.source_table,
            }
        )
    elif source_db_type == DatabaseConfiguration.DBTypes.ORACLE:
        return OracleSource(
            {
                "connection": source_conn,
                "table": etl_config.source_table,
            }
        )
    else:
        raise ValueError(f"Unknown source {source_db_type}")


def _get_etl_tgt(etl_config):
    target_db_type = etl_config.target_database.database_type
    target_conn = etl_config.target_database.connection_config

    if target_db_type == DatabaseConfiguration.DBTypes.POSTGRES:
        return PostgresTarget(
            {
                "connection": target_conn,
                "table": etl_config.target_table,
            }
        )
    elif target_db_type == DatabaseConfiguration.DBTypes.CLICKHOUSE:
        return ClickhouseTarget(
            {
                "connection": target_conn,
                "table": etl_config.target_table,
            }
        )
    else:
        raise ValueError(f"Unknown target {target_db_type}")


def run_etl(etl_config_id):
    etl_config = ETLConfiguration.objects.get(id=etl_config_id)

    print("=== START ETL ===")
    print("Source DB:", etl_config.source_database.connection_config)
    print("Target DB:", etl_config.target_database.connection_config)
    print("Source Table:", etl_config.source_table)
    print("Target Table:", etl_config.target_table)

    src = _get_etl_src(etl_config)
    tgt = _get_etl_tgt(etl_config)

    etl_schema = src.get_etl_schema()
    print("Schema:", etl_schema)

    tgt.ensure_schema(etl_schema)
    print("Schema ensured on target.")

    batch_count = 0

    for batch in src.stream_batches():
        batch_count += 1
        print(f"Loading batch {batch_count} with {len(batch)} rows...")
        tgt.load_batch(batch, etl_schema)

    print("=== END ETL ===")
    print(f"Total batches: {batch_count}")
