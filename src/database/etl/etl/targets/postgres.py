import psycopg
import random
from psycopg import types
import io
import os
import logging
from etl.common import TargetDriver, ETLDataTypes

logger = logging.getLogger(__name__)


class PostgresTarget(TargetDriver):
    def __init__(self, config):
        self.config = config
        self.conn = self._connect()  # ONE long-lived connection
        self.cur = self.conn.cursor()  # Reusable cursor

    def _connect(self):
        cfg = self.config["connection"]
        return psycopg.connect(
            host=cfg["host"],
            port=cfg["port"],
            user=cfg["user"],
            password=cfg["password"],
            dbname=cfg["database"],
        )

    def pg_data_types(self, etl_dtype):
        if etl_dtype == ETLDataTypes.INTEGER:
            return "bigint"
        elif etl_dtype == ETLDataTypes.FLOAT:
            return "real"
        elif etl_dtype == ETLDataTypes.BOOLEAN:
            return "bool"
        elif etl_dtype == ETLDataTypes.DATE_TIME:
            return "timestamp without time zone"
        elif etl_dtype == ETLDataTypes.DATE:
            return "date"
        elif etl_dtype == ETLDataTypes.TIME:
            return "time"
        elif etl_dtype == ETLDataTypes.TIME_INTERVAL:
            return "interval"
        elif etl_dtype == ETLDataTypes.JSON:
            return "jsonb"
        elif etl_dtype == ETLDataTypes.STRING:
            return "text"
        elif etl_dtype == ETLDataTypes.BYTES:
            return "bytea"
        else:
            raise ValueError(f"Unknown data type: {etl_dtype}")

    def ensure_schema(self, etl_schema):
        schema, table = self.config["table"].split(".")

        conn = self.conn
        cur = self.cur

        # Create schema if missing
        cur.execute(f"CREATE SCHEMA IF NOT EXISTS {schema}")

        # Check existing table
        cur.execute(
            """
            SELECT COUNT(*)
            FROM information_schema.tables
            WHERE table_schema=%s AND table_name=%s
            """,
            (schema, table),
        )

        exists = cur.fetchone()[0] > 0

        # 1. Create table if missing
        if not exists:
            col_defs = []
            for col, etl_dtype in etl_schema["columns"]:
                pg_type = self.pg_data_types(etl_dtype)
                col_defs.append(f"{col} {pg_type}")

            if etl_schema["primary_keys"]:
                col_defs.append(
                    "PRIMARY KEY ({})".format(", ".join(etl_schema["primary_keys"]))
                )

            ddl = f"CREATE TABLE {schema}.{table} ({', '.join(col_defs)})"
            logger.info(f"Creating new table {schema}.{table} with SQL: {ddl}")
            cur.execute(ddl)

        # 2. SCHEMA DRIFT: add missing columns
        cur.execute(
            """
            SELECT column_name
            FROM information_schema.columns
            WHERE table_schema=%s AND table_name=%s
            """,
            (schema, table),
        )

        existing_cols = {r[0] for r in cur.fetchall()}

        for col, etl_dtype in etl_schema["columns"]:
            if col not in existing_cols:
                pg_type = self.pg_data_types(etl_dtype)
                logger.warn(
                    f"[Schema Drift] Adding missing column: {col} with {pg_type}"
                )
                cur.execute(f"ALTER TABLE {schema}.{table} ADD COLUMN {col} {pg_type}")

        conn.commit()

    def load_batch(self, rows, etl_schema):
        schema, table = self.config["table"].split(".")

        conn = self.conn
        cur = self.cur

        rand_id = random.randint(100, 10000000)
        temp_table_name = f"temp_{table}_{rand_id}"

        cur.execute(
            f"CREATE TEMP TABLE {temp_table_name} (LIKE {schema}.{table} INCLUDING ALL)"
        )

        column_names = ", ".join([x[0] for x in etl_schema["columns"]])
        with cur.copy(f"COPY {temp_table_name} ({column_names}) FROM STDIN") as copy:
            rows = [list(x) for x in rows]
            for row in rows:
                # Wrap json column
                for idx in range(len(row)):
                    if etl_schema["columns"][idx][1] == ETLDataTypes.JSON:
                        row[idx] = psycopg.types.json.Jsonb(row[idx])
                copy.write_row(row)

        primary_keys = ", ".join(etl_schema["primary_keys"])
        conflict_cols = ", ".join(
            [
                f"{c[0]}=EXCLUDED.{c[0]}"
                for c in etl_schema["columns"]
                if c not in etl_schema["primary_keys"]
            ]
        )

        merge = f"""
            INSERT INTO {schema}.{table} ({column_names})
            SELECT {column_names} FROM {temp_table_name}
            ON CONFLICT ({primary_keys})
            DO UPDATE SET {conflict_cols}
        """

        cur.execute(merge)
        cur.execute(f"DROP TABLE {temp_table_name}")
        conn.commit()
