from etl.targets import PostgresTarget
from etl.sources import PostgresSource
import logging
import sys

logging.basicConfig(stream=sys.stderr, level=logging.INFO)


def test_etl():
    source_conn = {
        "host": "localhost",
        "port": 5511,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    }
    target_conn = {
        "host": "localhost",
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
        "public.rental"
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

    


if __name__ == "__main__":
    test_etl()
