import psycopg2
from common.driver_base import SourceDriver

class PostgresSource(SourceDriver):

    def __init__(self, config):
        self.config = config
        self.conn = self._connect()     # ONE connection for entire ETL
        self.cur = None                 # streaming cursor reused

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
            dbname=cfg["database"]
        )

    def get_columns(self):
        schema, table = self.config["table"].split(".")
        cur = self.conn.cursor()

        cur.execute("""
            SELECT column_name, data_type
            FROM information_schema.columns
            WHERE table_schema=%s AND table_name=%s
            ORDER BY ordinal_position
        """, (schema, table))

        cols = [(r[0], r[1]) for r in cur.fetchall()]
        cur.close()
        return cols

    def stream_rows(self, last_ts, last_msn, batch_size):
        if not self.cur:
            self.cur = self.conn.cursor(name="stream")
        cur = self.cur

        cur.itersize = batch_size

        table = self.config["table"]

        date_start = self.config["date_filter"]["start"]
        date_end   = self.config["date_filter"]["end"]

        sql = f"""
            SELECT *
            FROM {table}
            WHERE ts >= %s AND ts < %s
        """
        params = [date_start, date_end]

        # Resume logic
        if last_ts:
            sql += " AND (ts > %s OR (ts = %s AND msn_id > %s))"
            params.extend([last_ts, last_ts, last_msn])

        sql += " ORDER BY ts, msn_id"

        # print("[DEBUG] SQL =", sql)
        # print("[DEBUG] params =", params)

        cur.execute(sql, params)

        while True:
            batch = cur.fetchmany(batch_size)
            if not batch:
                break

            # print(f"[DEBUG] Extracted {len(batch)} rows")
            yield batch
        self.cur = None
