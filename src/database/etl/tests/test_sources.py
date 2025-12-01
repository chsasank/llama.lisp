import logging
import sys

import psycopg
from etl.common import ETLDataTypes
from etl.sources import PostgresSource

logging.basicConfig(stream=sys.stderr, level=logging.INFO)

test_psql_config = {
    "connection": {
        "host": "localhost",
        "port": 5511,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    },
    "table": "tap_github.commits",
}


def test_psql_init():
    src = PostgresSource(test_psql_config)
    assert isinstance(src.conn, psycopg.Connection)


def test_psql_schema():
    expected_schemas = {
        "tap_github.commits": {
            "columns": [
                ("org", ETLDataTypes.STRING),
                ("repo", ETLDataTypes.STRING),
                ("repo_id", ETLDataTypes.INTEGER),
                ("node_id", ETLDataTypes.STRING),
                ("url", ETLDataTypes.STRING),
                ("sha", ETLDataTypes.STRING),
                ("html_url", ETLDataTypes.STRING),
                ("commit_timestamp", ETLDataTypes.DATE_TIME),
                ("commit", ETLDataTypes.JSON),
                ("author", ETLDataTypes.JSON),
                ("committer", ETLDataTypes.JSON),
                ("_sdc_extracted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_received_at", ETLDataTypes.DATE_TIME),
                ("_sdc_batched_at", ETLDataTypes.DATE_TIME),
                ("_sdc_deleted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_sequence", ETLDataTypes.INTEGER),
                ("_sdc_table_version", ETLDataTypes.INTEGER),
                ("_sdc_sync_started_at", ETLDataTypes.INTEGER),
            ],
            "primary_keys": ["node_id"],
        },
        "public.actor": {
            "columns": [
                ("actor_id", ETLDataTypes.INTEGER),
                ("first_name", ETLDataTypes.STRING),
                ("last_name", ETLDataTypes.STRING),
                ("last_update", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["actor_id"],
        },
        "public.staff": {
            "columns": [
                ("staff_id", ETLDataTypes.INTEGER),
                ("first_name", ETLDataTypes.STRING),
                ("last_name", ETLDataTypes.STRING),
                ("address_id", ETLDataTypes.INTEGER),
                ("email", ETLDataTypes.STRING),
                ("store_id", ETLDataTypes.INTEGER),
                ("active", ETLDataTypes.BOOLEAN),
                ("username", ETLDataTypes.STRING),
                ("password", ETLDataTypes.STRING),
                ("last_update", ETLDataTypes.DATE_TIME),
                ("picture", ETLDataTypes.BYTES),
            ],
            "primary_keys": ["staff_id"],
        },
        "public.payment_p2022_07": {
            "columns": [
                ("payment_id", ETLDataTypes.INTEGER),
                ("customer_id", ETLDataTypes.INTEGER),
                ("staff_id", ETLDataTypes.INTEGER),
                ("rental_id", ETLDataTypes.INTEGER),
                ("amount", ETLDataTypes.FLOAT),
                ("payment_date", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["payment_date", "payment_id"],
        },
        "tap_github.dependencies": {
            "columns": [
                ("repo", ETLDataTypes.STRING),
                ("org", ETLDataTypes.STRING),
                ("repo_id", ETLDataTypes.INTEGER),
                ("dependency_repo_id", ETLDataTypes.INTEGER),
                ("package_name", ETLDataTypes.STRING),
                ("package_manager", ETLDataTypes.STRING),
                ("requirements", ETLDataTypes.STRING),
                ("has_dependencies", ETLDataTypes.BOOLEAN),
                ("dependency", ETLDataTypes.JSON),
                ("_sdc_extracted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_received_at", ETLDataTypes.DATE_TIME),
                ("_sdc_batched_at", ETLDataTypes.DATE_TIME),
                ("_sdc_deleted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_sequence", ETLDataTypes.INTEGER),
                ("_sdc_table_version", ETLDataTypes.INTEGER),
                ("_sdc_sync_started_at", ETLDataTypes.INTEGER),
            ],
            "primary_keys": [
                "repo_id",
                "package_name",
                "package_manager",
                "requirements",
            ],
        },
    }

    for table_name, expected_schema in expected_schemas.items():
        src = PostgresSource(
            {"connection": test_psql_config["connection"], "table": table_name}
        )
        assert (
            src.get_etl_schema() == expected_schema
        ), f"{table_name} schema didn't match"
        print(f"schema matched for {table_name}")


def test_psql_stream_batches():
    src = PostgresSource(test_psql_config, batch_size=100)
    batches = src.stream_batches()
    first_batch = next(batches)
    assert len(first_batch) == 100
    row = first_batch[0]
    assert row[0] == "meltano"
    assert row[1] == "meltano"
    assert len(row) == len(src.get_etl_schema()["columns"])


def test_psql_stream_batches_replication():
    src = PostgresSource(
        {**test_psql_config, "replication_key": "commit_timestamp"},
        state_id="test_run_psql_source",
        batch_size=100,
    )
    for batch in src.stream_batches():
        assert len(batch) <= 100
        row = batch[0]
        assert row[0] == "meltano"
        assert row[1] == "meltano"
        assert len(row) == len(src.get_etl_schema()["columns"])

    assert src.state_manager.get_state() is not None


if __name__ == "__main__":
    test_psql_init()
    test_psql_schema()
    test_psql_stream_batches()
    test_psql_stream_batches_replication()
