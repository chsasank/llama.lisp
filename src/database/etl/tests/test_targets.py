import logging
import os
import pickle
import sys

import clickhouse_connect
import psycopg
from common import testing_database_host
from etl.common import ETLDataTypes
from etl.targets import ClickhouseTarget, PostgresTarget

here = os.path.dirname(os.path.realpath(__file__))

logging.basicConfig(stream=sys.stderr, level=logging.INFO)

test_psql_config = {
    "connection": {
        "host": testing_database_host,
        "port": 5512,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    },
    "table": "tap_github.commits",
}

test_ch_config = {
    "connection": {
        "host": testing_database_host,
        "port": 8123,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    },
    "table": "tap_github_commits",
}


test_etl_schema = {
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


def test_psql_conn():
    tgt = PostgresTarget(test_psql_config)
    assert isinstance(tgt.conn, psycopg.Connection)


def test_psql_ensure_schema():
    tgt = PostgresTarget(test_psql_config)
    tgt.ensure_schema(test_etl_schema)


def test_psql_load_batch():
    tgt = PostgresTarget(test_psql_config)
    rows = pickle.load(open(os.path.join(here, "data/testing_batch.pkl"), "rb"))
    tgt.load_batch(rows, test_etl_schema)


def test_ch_conn():
    tgt = ClickhouseTarget(test_ch_config)
    assert isinstance(tgt.client, clickhouse_connect.driver.client.Client)


def test_ch_ensure_schema():
    tgt = ClickhouseTarget(test_ch_config)
    tgt.ensure_schema(test_etl_schema)


def test_ch_load_batch():
    tgt = ClickhouseTarget(test_ch_config)
    rows = pickle.load(open(os.path.join(here, "data/testing_batch.pkl"), "rb"))
    tgt.load_batch(rows, test_etl_schema)


if __name__ == "__main__":
    test_psql_conn()
    test_psql_ensure_schema()
    test_psql_load_batch()
    test_ch_conn()
    test_ch_ensure_schema()
    test_ch_load_batch()
