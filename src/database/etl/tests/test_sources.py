import psycopg
from etl.sources import PostgresSource
from etl.common import ETLDataTypes
import logging
import sys

logging.basicConfig(stream=sys.stderr, level=logging.INFO)

test_psql_config = {
    "connection": {
        "host": "100.64.0.200",
        "port": 5511,
        "user": "github",
        "password": "intelarc",
        "database": "github",
    },
    "table": "tap_github.commits",
}


def test_psql_init():
    src = PostgresSource(test_psql_config)
    assert isinstance(src.conn, psycopg.Connection)


def test_psql_schema():
    src = PostgresSource(test_psql_config)

    expected_schema = [
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
    ]

    assert src.get_etl_schema() == {
        "columns": expected_schema,
        "primary_keys": ["node_id"],
    }


def test_psql_stream_batches():
    src = PostgresSource(test_psql_config, batch_size=100)
    batches = src.stream_batches()
    first_batch = next(batches)
    assert len(first_batch) == 100
    row = first_batch[0]
    assert row[0] == "meltano"
    assert row[1] == "meltano"
    assert len(row) == len(src.get_etl_schema()["columns"])


if __name__ == "__main__":
    test_psql_init()
    test_psql_schema()
    test_psql_stream_batches()
