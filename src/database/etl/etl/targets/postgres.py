import logging
import random

import psycopg
from etl.common import ETLDataTypes, TargetDriver

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

    def normalize_row(self, row, etl_schema):
        row = list(row)
        # Wrap json column
        for idx in range(len(row)):
            if etl_schema["columns"][idx][1] == ETLDataTypes.JSON:
                row[idx] = psycopg.types.json.Jsonb(row[idx])

        return row

    def ensure_schema(self, etl_schema):
        schema, table = self.config["table"].split(".")

        # Fail fast if no primary key
        if not etl_schema["primary_keys"]:
            raise ValueError(
                f"Table {schema}.{table} has no primary key — cannot perform UPSERT. "
                "Add a primary key or modify ETL to allow append-only mode."
            )

        conn = self.conn
        cur = self.cur

        # Create schema if missing
        cur.execute(
            psycopg.sql.SQL("CREATE SCHEMA IF NOT EXISTS {schema}").format(
                schema=psycopg.sql.Identifier(schema)
            )
        )

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
                col_defs.append(
                    psycopg.sql.SQL("{col} " + pg_type).format(
                        col=psycopg.sql.Identifier(col)
                    )
                )

            if etl_schema["primary_keys"]:
                col_defs.append(
                    psycopg.sql.SQL("PRIMARY KEY ({pkeys})").format(
                        pkeys=psycopg.sql.SQL(", ").join(
                            [
                                psycopg.sql.Identifier(x)
                                for x in etl_schema["primary_keys"]
                            ]
                        )
                    )
                )

            ddl = psycopg.sql.SQL("CREATE TABLE {table_name} ({col_defs_join})").format(
                table_name=psycopg.sql.Identifier(schema, table),
                col_defs_join=psycopg.sql.SQL(", ").join(col_defs),
            )
            logger.info(
                f"Creating new table {schema}.{table} with SQL: {ddl.as_string()}"
            )
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
                cur.execute(
                    psycopg.sql.SQL(
                        "ALTER TABLE {table_name} ADD COLUMN {col} " + pg_type
                    ).format(
                        table_name=psycopg.sql.Identifier(schema, table),
                        col=psycopg.sql.Identifier(col),
                    )
                )

        conn.commit()

    def load_batch(self, rows, etl_schema):
        schema, table = self.config["table"].split(".")

        conn = self.conn
        cur = self.cur

        rand_id = random.randint(100, 10000000)
        temp_table_name = f"temp_{table}_{rand_id}"

        cur.execute(
            psycopg.sql.SQL(
                "CREATE TEMP TABLE {temp_table_name} (LIKE {table_name} INCLUDING ALL)"
            ).format(
                temp_table_name=psycopg.sql.Identifier(temp_table_name),
                table_name=psycopg.sql.Identifier(schema, table),
            )
        )

        column_names = psycopg.sql.SQL(", ").join(
            [psycopg.sql.Identifier(x[0]) for x in etl_schema["columns"]]
        )
        with cur.copy(
            psycopg.sql.SQL(
                "COPY {temp_table_name} ({column_names}) FROM STDIN"
            ).format(
                temp_table_name=psycopg.sql.Identifier(temp_table_name),
                column_names=column_names,
            )
        ) as copy:
            for row in rows:
                row = self.normalize_row(row, etl_schema)
                copy.write_row(row)

        primary_keys = psycopg.sql.SQL(", ").join(
            [psycopg.sql.Identifier(x) for x in etl_schema["primary_keys"]]
        )
        conflict_cols = psycopg.sql.SQL(", ").join(
            [
                psycopg.sql.SQL("{col}=EXCLUDED.{col}").format(
                    col=psycopg.sql.Identifier(c[0])
                )
                for c in etl_schema["columns"]
                if c not in etl_schema["primary_keys"]
            ]
        )

        merge = psycopg.sql.SQL("""
            INSERT INTO {table_name} ({column_names})
            SELECT {column_names} FROM {temp_table_name}
            ON CONFLICT ({primary_keys})
            DO UPDATE SET {conflict_cols}
        """).format(
            table_name=psycopg.sql.Identifier(schema, table),
            column_names=column_names,
            temp_table_name=psycopg.sql.Identifier(temp_table_name),
            primary_keys=primary_keys,
            conflict_cols=conflict_cols,
        )

        cur.execute(merge)
        cur.execute(
            psycopg.sql.SQL("DROP TABLE {temp_table_name}").format(
                temp_table_name=psycopg.sql.Identifier(temp_table_name)
            )
        )
        conn.commit()
        logger.info(f"Loaded {len(rows)} rows into {schema}.{table}")
