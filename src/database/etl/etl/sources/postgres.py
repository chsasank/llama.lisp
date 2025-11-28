import psycopg2
from etl.common.driver_base import SourceDriver
import logging

logger = logging.getLogger(__name__)


class PostgresSource(SourceDriver):

    def __init__(self, config, batch_size=10000):
        self.config = config
        self.conn = self._connect()  # ONE connection for entire ETL
        self.cur = None  # streaming cursor reused
        self.batch_size = batch_size

    def reconnect(self):
        try:
            self.conn.close()
        except:
            pass
        self.conn = self._connect()
        self.cur = None

    def _connect(self):
        cfg = self.config["connection"]
        return psycopg2.connect(
            host=cfg["host"],
            port=cfg["port"],
            user=cfg["user"],
            password=cfg["password"],
            dbname=cfg["database"],
        )

    def get_schema(self):
        schema, table = self.config["table"].split(".")
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

        cols = [(r[0], r[1]) for r in cur.fetchall()]
        return cols

    def stream_batches(self):
        if not self.cur:
            self.cur = self.conn.cursor(name="stream")
        cur = self.cur

        cur.itersize = self.batch_size

        table = self.config["table"]
        schema = self.get_schema()

        sql = "SELECT {} FROM {}".format(", ".join([x[0] for x in schema]), table)

        logger.info(f"SQL = {sql}")
        cur.execute(sql)

        while True:
            batch = cur.fetchmany(self.batch_size)
            if not batch:
                break

            logger.info(f"Extracted {len(batch)} rows")
            yield batch
        self.cur = None
