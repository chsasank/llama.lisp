import logging

import psycopg
from etl.common import ETLDataTypes, SourceDriver, StateManagerDriver

logger = logging.getLogger(__name__)


class PostgresSource(SourceDriver):
    def __init__(self, config, state_manager=None, batch_size=10000):
        self.config = config
        self.conn = self._connect()  # ONE connection for entire ETL
        self.cur = None  # streaming cursor reused
        self.batch_size = batch_size
        self.state_manager = state_manager
        if self.state_manager is not None:
            assert isinstance(self.state_manager, StateManagerDriver)

    def _connect(self):
        cfg = self.config["connection"]
        return psycopg.connect(
            host=cfg["host"],
            port=cfg["port"],
            user=cfg["user"],
            password=cfg["password"],
            dbname=cfg["database"],
        )

    def normalize_data_type(self, dtype):
        if dtype in ("integer", "bigint", "smallint"):
            return ETLDataTypes.INTEGER
        elif dtype in ("real", "double", "numeric"):
            return ETLDataTypes.FLOAT
        elif dtype in ("boolean"):
            return ETLDataTypes.BOOLEAN
        elif "timestamp " in dtype:
            return ETLDataTypes.DATE_TIME
        elif dtype in ("date"):
            return ETLDataTypes.DATE
        elif "time " in dtype:
            return ETLDataTypes.TIME
        elif "interval" in dtype:
            return ETLDataTypes.TIME_INTERVAL
        elif "json" in dtype:
            return ETLDataTypes.JSON
        elif dtype in ("varchar", "text"):
            return ETLDataTypes.STRING
        elif dtype in ("bytea"):
            return ETLDataTypes.BYTES
        else:
            raise ValueError(f"Unknown data type: {dtype}")

    def get_etl_schema(self):
        schema, table = self.config["table"].split(".")

        # find column names
        cur = self.conn.cursor()
        cur.execute(
            """
            SELECT column_name, data_type
            FROM information_schema.columns
            WHERE table_schema=%s AND table_name=%s
            ORDER BY ordinal_position
        """,
            (schema, table),
        )

        cols = [(r[0], self.normalize_data_type(r[1])) for r in cur.fetchall()]

        # find primary key: https://stackoverflow.com/a/19570298
        cur = self.conn.cursor()
        cur.execute(
            """
            SELECT column_name
            FROM information_schema.table_constraints
                JOIN information_schema.key_column_usage
                    USING (constraint_catalog, constraint_schema, constraint_name,
                            table_catalog, table_schema, table_name)
            WHERE constraint_type = 'PRIMARY KEY'
            AND (table_schema, table_name) = (%s, %s)
            ORDER BY ordinal_position;
        """,
            (schema, table),
        )
        primary_cols = [r[0] for r in cur.fetchall()]
        return {"columns": cols, "primary_keys": primary_cols}

    def _save_state(self, batch, etl_schema):
        # store state
        if self.state_manager:
            col_names = [x[0] for x in etl_schema["columns"]]
            rep_key_idx = col_names.index(self.state_manager.replication_key)
            last_row_state = batch[-1][rep_key_idx]
            self.state_manager.set_state(last_row_state)
            logger.info(
                f"Replication key {self.state_manager.replication_key} updated with {last_row_state}"
            )

    def stream_batches(self):
        if not self.cur:
            self.cur = self.conn.cursor(name="stream")
        cur = self.cur

        cur.itersize = self.batch_size

        schema, table = self.config["table"].split(".")
        etl_schema = self.get_etl_schema()
        col_names = [x[0] for x in etl_schema["columns"]]
        format_cols = psycopg.sql.SQL(", ").join(
            [psycopg.sql.Identifier(x) for x in col_names]
        )

        if self.state_manager:
            assert self.state_manager.replication_key in col_names, (
                f"{self.state_manager.replication_key} is not in cols {col_names}"
            )
            replication_key = self.state_manager.replication_key
            current_state = self.state_manager.get_state()
            logger.info(f"Replication key found: {replication_key}")
            if current_state:
                sql = psycopg.sql.SQL(
                    "SELECT {format_cols} FROM {table} where {replication_key} >= %s ORDER BY {replication_key}"
                ).format(
                    format_cols=format_cols,
                    table=psycopg.sql.Identifier(schema, table),
                    replication_key=psycopg.sql.Identifier(replication_key),
                )
                params = (current_state,)
            else:
                sql = psycopg.sql.SQL(
                    "SELECT {format_cols} FROM {table} ORDER BY {replication_key}"
                ).format(
                    format_cols=format_cols,
                    table=psycopg.sql.Identifier(schema, table),
                    replication_key=psycopg.sql.Identifier(replication_key),
                )
                params = None
        else:
            # do full replication if there is no state
            sql = psycopg.sql.SQL("SELECT {format_cols} FROM {table}").format(
                format_cols=format_cols, table=psycopg.sql.Identifier(schema, table)
            )
            params = None

        logger.info(f"Fetching rows with SQL={sql.as_string()}, params={params}")
        cur.execute(sql, params)

        while True:
            batch = cur.fetchmany(self.batch_size)
            if not batch:
                break

            logger.info(f"Extracted {len(batch)} rows from {schema}.{table}")

            yield batch
            self._save_state(batch, etl_schema)

        self.cur = None
