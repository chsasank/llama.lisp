from etl.targets import PostgresTarget
from etl.common import ETLDataTypes
import pickle
import psycopg
import logging
import sys

logging.basicConfig(stream=sys.stderr, level=logging.INFO)

test_psql_config = {
    "connection": {
        "host": "100.64.0.200",
        "port": 5503,
        "user": "custom_etl",
        "password": "intelarc",
        "database": "postgres",
    },
    "table": "tap_github.commits",
}


def test_psql_conn():
    tgt = PostgresTarget(test_psql_config)
    assert isinstance(tgt.conn, psycopg.Connection)


def test_ensure_schema():
    tgt = PostgresTarget(test_psql_config)
    etl_schema = {
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
    }
    tgt.ensure_schema(etl_schema)

def test_load_batch():
    tgt = PostgresTarget(test_psql_config)
    etl_schema = {
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
    }

    rows = pickle.load(open('testing_batch.pkl', 'rb'))
    tgt.load_batch(rows, etl_schema)

if __name__ == "__main__":
    test_psql_conn()
    test_ensure_schema()
    test_load_batch()