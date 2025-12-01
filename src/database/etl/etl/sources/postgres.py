import logging

import psycopg
from etl.common import ETLDataTypes, SourceDriver

logger = logging.getLogger(__name__)


class PostgresSource(SourceDriver):
    def __init__(self, config, batch_size=10000):
        self.config = config
        self.conn = self._connect()  # ONE connection for entire ETL
        self.cur = None  # streaming cursor reused
        self.batch_size = batch_size

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

    def stream_batches(self):
        if not self.cur:
            self.cur = self.conn.cursor(name="stream")
        cur = self.cur

        cur.itersize = self.batch_size

        table = self.config["table"]
        cols = self.get_etl_schema()["columns"]

        sql = "SELECT {} FROM {}".format(", ".join([x[0] for x in cols]), table)

        logger.info(f"SQL = {sql}")
        cur.execute(sql)

        while True:
            batch = cur.fetchmany(self.batch_size)
            if not batch:
                break

            logger.info(f"Extracted {len(batch)} rows")
            yield batch
        self.cur = None
