import psycopg2
from etl.sources import PostgresSource
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
    assert isinstance(src.conn, psycopg2.extensions.connection)


def test_psql_schema():
    src = PostgresSource(test_psql_config)

    expected_schema = [
        ("org", "text"),
        ("repo", "text"),
        ("repo_id", "bigint"),
        ("node_id", "text"),
        ("url", "text"),
        ("sha", "text"),
        ("html_url", "text"),
        ("commit_timestamp", "timestamp without time zone"),
        ("commit", "jsonb"),
        ("author", "jsonb"),
        ("committer", "jsonb"),
        ("_sdc_extracted_at", "timestamp without time zone"),
        ("_sdc_received_at", "timestamp without time zone"),
        ("_sdc_batched_at", "timestamp without time zone"),
        ("_sdc_deleted_at", "timestamp without time zone"),
        ("_sdc_sequence", "bigint"),
        ("_sdc_table_version", "bigint"),
        ("_sdc_sync_started_at", "bigint"),
    ]

    assert src.get_schema() == expected_schema


def test_psql_stream_batches():
    src = PostgresSource(test_psql_config, batch_size=100)
    batches = src.stream_batches()
    first_batch = next(batches)
    assert len(first_batch) == 100
    row = first_batch[0]
    assert row[0] == "meltano"
    assert row[1] == "meltano"
    assert len(row) == len(src.get_schema())


if __name__ == "__main__":
    test_psql_init()
    test_psql_schema()
    test_psql_stream_batches()
