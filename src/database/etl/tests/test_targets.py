import logging
import os
import pickle
import sys

import clickhouse_connect
import psycopg
import datetime
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

def test_ch_date_normalization():
    tgt = ClickhouseTarget(test_ch_config)

    d = datetime.date(2024, 1, 15)
    dt = datetime.datetime(2024, 1, 15, 10, 30, 0)

    normalized_date = tgt._normalize_value(d, ETLDataTypes.DATE)
    normalized_dt = tgt._normalize_value(dt, ETLDataTypes.DATE_TIME)

    assert isinstance(normalized_date, datetime.datetime)
    assert normalized_date.hour == 0
    assert normalized_date.minute == 0
    assert normalized_date.second == 0

    assert normalized_dt == dt

def test_ch_load_batch_with_date_values():
    tgt = ClickhouseTarget(test_ch_config)
    tgt.ensure_schema(test_etl_schema)

    rows = [
        (
            "openai",
            "chatgpt",
            1,
            "node-1",
            "https://api.github.com",
            "sha-1",
            "https://github.com",
            datetime.date(2023, 12, 1),
            {},
            {},
            {},
            datetime.datetime.utcnow(),
            datetime.datetime.utcnow(),
            datetime.datetime.utcnow(),
            None,
            1,
            1,
            1700000000,
        )
    ]

    # Should NOT raise
    tgt.load_batch(rows, test_etl_schema)

def test_ch_load_batch_with_null_date():
    tgt = ClickhouseTarget(test_ch_config)
    tgt.ensure_schema(test_etl_schema)

    rows = [
        (
            "openai",
            "chatgpt",
            2,
            "node-2",
            "https://api.github.com",
            "sha-2",
            "https://github.com",
            None,               # NULL date
            {},
            {},
            {},
            datetime.datetime.utcnow(),
            datetime.datetime.utcnow(),
            datetime.datetime.utcnow(),
            None,
            2,
            1,
            1700000001,
        )
    ]

    # Should NOT raise
    tgt.load_batch(rows, test_etl_schema)
    
def test_ch_normalize_utf_string_datetime():
    tgt = ClickhouseTarget(test_ch_config)

    value = "2024-01-15T10:30:00"
    normalized = tgt._normalize_value(value, ETLDataTypes.DATE_TIME)

    assert normalized == value

if __name__ == "__main__":
    test_psql_conn()
    test_psql_ensure_schema()
    test_psql_load_batch()
    test_ch_conn()
    test_ch_ensure_schema()
    test_ch_load_batch()
    test_ch_date_normalization()
    test_ch_load_batch_with_date_values()
    test_ch_load_batch_with_null_date()
    test_ch_normalize_utf_string_datetime()