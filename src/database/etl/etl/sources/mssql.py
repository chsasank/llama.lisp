import logging

import pyodbc
from etl.common import ETLDataTypes, SourceDriver, StateManager

logger = logging.getLogger(__name__)


class MssqlSource(SourceDriver):
    def __init__(self, config, state_id=None, batch_size=10000):
        self.config = config
        self.conn = self._connect()
        self.cur = None
        self.batch_size = batch_size

        if "replication_key" in self.config:
            assert state_id is not None
            self.state_manager = StateManager(state_id, self.config["replication_key"])
        else:
            self.state_manager = None

    def _connect(self):
        cfg = self.config["connection"]
        conn = pyodbc.connect(
            "Driver={ODBC Driver 18 for SQL Server};"
            f"Server={cfg['host']},{cfg.get('port', 1433)};"
            f"Database={cfg['database']};"
            f"UID={cfg['user']};"
            f"PWD={cfg['password']};"
            "Encrypt=no;"
            "TrustServerCertificate=yes;"
        )

        # Auto-convert unsupported SQL Server types (like geography, geometry)
        # -151 = SQL_SS_UDT (User-defined types including geography, geometry)
        conn.add_output_converter(-151, lambda v: v.hex() if v else None)

        return conn

    def normalize_data_type(self, dtype):
        if dtype in ("int", "smallint", "bigint", "tinyint"):
            return ETLDataTypes.INTEGER
        elif dtype in ("float", "real", "decimal", "numeric", "money", "smallmoney"):
            return ETLDataTypes.FLOAT
        elif dtype == "bit":
            return ETLDataTypes.BOOLEAN
        elif dtype in ("datetime", "datetime2", "smalldatetime", "datetimeoffset"):
            return ETLDataTypes.DATE_TIME
        elif dtype == "date":
            return ETLDataTypes.DATE
        elif dtype == "time":
            return ETLDataTypes.TIME
        elif dtype in ("binary", "varbinary", "image"):
            return ETLDataTypes.BYTES
        elif dtype in ("nvarchar", "varchar", "nchar", "char", "text", "ntext"):
            return ETLDataTypes.STRING
        elif dtype in ("json"):
            return ETLDataTypes.JSON
        elif dtype in ("xml", "geography", "geometry"):
            # cast these unsupported types to string
            return ETLDataTypes.STRING
        else:
            raise ValueError(f"Unknown data type: {dtype}")

    def get_etl_schema(self):
        schema, table = self.config["table"].split(".")

        # Column names and datatypes
        cur = self.conn.cursor()
        cur.execute(
            """
            SELECT COLUMN_NAME, DATA_TYPE
            FROM INFORMATION_SCHEMA.COLUMNS
            WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?
            ORDER BY ORDINAL_POSITION;
            """,
            (schema, table),
        )
        cols = [(r[0], self.normalize_data_type(r[1])) for r in cur.fetchall()]

        # Primary key detection
        cur.execute(
            """
            SELECT k.COLUMN_NAME
            FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS t
            JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE k
                ON t.CONSTRAINT_NAME = k.CONSTRAINT_NAME
            WHERE t.TABLE_SCHEMA = ?
              AND t.TABLE_NAME = ?
              AND t.CONSTRAINT_TYPE = 'PRIMARY KEY'
            ORDER BY k.ORDINAL_POSITION;
            """,
            (schema, table),
        )
        primary_cols = [r[0] for r in cur.fetchall()]
        return {"columns": cols, "primary_keys": primary_cols}

    def _save_state(self, batch, etl_schema):
        if self.state_manager:
            col_names = [x[0] for x in etl_schema["columns"]]
            rep_key_idx = col_names.index(self.state_manager.replication_key)
            last_row_state = batch[-1][rep_key_idx]
            self.state_manager.set_state(last_row_state)
            logger.info(
                f"Replication key {self.state_manager.replication_key} updated with {last_row_state}"
            )

    def stream_batches(self):
        # pyodbc does NOT support server-side named cursors → use normal cursor
        if not self.cur:
            self.cur = self.conn.cursor()
        cur = self.cur

        # same meaning as psql itersize
        cur.arraysize = self.batch_size

        table = self.config["table"]
        etl_schema = self.get_etl_schema()
        col_names = [x[0] for x in etl_schema["columns"]]
        # mssql server needs bracket quoting
        format_cols = ", ".join(f"[{c}]" for c in col_names)

        if self.state_manager:
            assert (
                self.state_manager.replication_key in col_names
            ), f"{self.state_manager.replication_key} is not in cols {col_names}"
            replication_key = self.state_manager.replication_key
            current_state = self.state_manager.get_state()
            logger.info(f"Replication key found: {replication_key}")

            if current_state:
                sql = (
                    f"SELECT {format_cols} FROM {table} "
                    f"WHERE [{replication_key}] >= ? "
                    f"ORDER BY [{replication_key}]"
                )
                params = (current_state,)
            else:
                sql = f"SELECT {format_cols} FROM {table} ORDER BY [{replication_key}]"
                params = ()
        else:
            # full extraction
            sql = f"SELECT {format_cols} FROM {table}"
            params = ()

        logger.info(f"Fetching rows with SQL={sql}, params={params}")
        cur.execute(sql, params)

        while True:
            batch = cur.fetchmany(self.batch_size)
            if not batch:
                break

            logger.info(f"Extracted {len(batch)} rows")

            yield batch
            self._save_state(batch, etl_schema)

        self.cur = None
