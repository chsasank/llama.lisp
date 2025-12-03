import logging
import sys

from common import testing_database_host
from etl.sources import MssqlSource, PostgresSource
from etl.targets import ClickhouseTarget, PostgresTarget

logging.basicConfig(stream=sys.stderr, level=logging.INFO)


def test_psql_etl():
    source_conn = {
        "host": testing_database_host,
        "port": 5511,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    }
    target_conn = {
        "host": testing_database_host,
        "port": 5512,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    }

    tables_to_copy = [
        "tap_github.commits",
        "public.actor",
        "public.staff",
        "public.payment_p2022_07",
        "tap_github.dependencies",
        "public.rental",
    ]
    for table_name in tables_to_copy:
        print(f"Copying {table_name}")

        src = PostgresSource({"connection": source_conn, "table": table_name})
        tgt = PostgresTarget({"connection": target_conn, "table": table_name})

        etl_schema = src.get_etl_schema()
        tgt.ensure_schema(etl_schema)

        batches = src.stream_batches()
        for batch in batches:
            tgt.load_batch(batch, etl_schema)


def test_ch_etl():
    source_conn = {
        "host": testing_database_host,
        "port": 5511,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    }
    target_conn = {
        "host": testing_database_host,
        "port": 8123,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    }

    tables_to_copy = [
        "tap_github.commits",
        "public.actor",
        "public.staff",
        "public.payment_p2022_07",
        "tap_github.dependencies",
        "public.rental",
    ]
    for table_name in tables_to_copy:
        print(f"Copying {table_name}")

        src = PostgresSource({"connection": source_conn, "table": table_name})
        tgt = ClickhouseTarget({"connection": target_conn, "table": table_name})

        etl_schema = src.get_etl_schema()
        tgt.ensure_schema(etl_schema)

        batches = src.stream_batches()
        for batch in batches:
            tgt.load_batch(batch, etl_schema)


def test_mssql_etl():
    source_conn = {
        "host": testing_database_host,
        "port": 1433,
        "user": "SA",
        "password": "Intelarc@123",
        "database": "WideWorldImporters",
    }

    target_conn = {
        "host": testing_database_host,
        "port": 5512,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    }

    tables_to_copy = ["Sales.Customers", "Purchasing.Suppliers"]

    for table_name in tables_to_copy:
        print(f"Copying {table_name}")

        src = MssqlSource({"connection": source_conn, "table": table_name})

        # converting schema.table to safe table for PostgreSQL
        pg_table = table_name.replace(".", "_").lower()

        tgt = PostgresTarget({"connection": target_conn, "table": f"public.{pg_table}"})

        etl_schema = src.get_etl_schema()

        # lowercase columns so Postgres drift checking doesn’t break
        etl_schema["columns"] = [
            (col.lower(), dtype) for col, dtype in etl_schema["columns"]
        ]
        etl_schema["primary_keys"] = [pk.lower() for pk in etl_schema["primary_keys"]]

        tgt.ensure_schema(etl_schema)

        for batch in src.stream_batches():
            tgt.load_batch(batch, etl_schema)


if __name__ == "__main__":
    test_psql_etl()
    test_ch_etl()
    test_mssql_etl()
