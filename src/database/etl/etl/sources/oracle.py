import logging
import oracledb
from etl.common import ETLDataTypes, SourceDriver, StateManager

logger = logging.getLogger(__name__)

class OracleSource(SourceDriver):
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
        conn = oracledb.connect(
            user=cfg["user"],
            password=cfg["password"],
            host=cfg["host"],
            port=cfg.get("port", 1521),
            service_name=cfg["service"],
        )
        return conn

    def normalize_data_type(self, dtype):
        # https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlrf/Data-Types.html
        if dtype in ("NUMBER", "INTEGER", "INT", "SMALLINT"):
            return ETLDataTypes.INTEGER
        elif dtype in ("FLOAT", "BINARY_FLOAT", "BINARY_DOUBLE"):
            return ETLDataTypes.FLOAT
        elif dtype in ("BOOLEAN",):
            return ETLDataTypes.BOOLEAN
        elif dtype in ("TIMESTAMP", "TIMESTAMP WITH TIME ZONE", "TIMESTAMP WITH LOCAL TIME ZONE"):
            return ETLDataTypes.DATE_TIME
        elif dtype in ("DATE",):
            return ETLDataTypes.DATE
        elif dtype.startswith("INTERVAL"):
            return ETLDataTypes.TIME_INTERVAL
        elif dtype in ("RAW", "BLOB", "BFILE"):
            return ETLDataTypes.BYTES
        elif dtype in ("CHAR", "NCHAR", "VARCHAR2", "NVARCHAR2", "CLOB", "NCLOB"):
            return ETLDataTypes.STRING
        elif dtype == "JSON":
            return ETLDataTypes.JSON
        elif dtype == "XMLTYPE":
            return ETLDataTypes.STRING
        elif dtype in ("LONG", "LONG RAW"):
            return ETLDataTypes.STRING
        elif dtype in ("ROWID", "UROWID"):
            return ETLDataTypes.STRING
        else:
            raise ValueError(f"Unknown data type: {dtype}")

    def get_etl_schema(self):
        schema, table = self.config["table"].split(".")
        schema = schema.upper()
        table = table.upper()

        cur = self.conn.cursor()
        # https://docs.oracle.com/en/database/oracle/oracle-database/26/refrn/ALL_TAB_COLUMNS.html
        # about ALL_TAB_COLUMNS

        # https://python-oracledb.readthedocs.io/en/latest/user_guide/bind.html
        # bind variables are denoted with a colon and a name such as :owner and :tname
        cur.execute(
            """
            SELECT COLUMN_NAME, DATA_TYPE
            FROM ALL_TAB_COLUMNS
            WHERE OWNER = :owner
            AND TABLE_NAME = :tname
            ORDER BY COLUMN_ID
            """,
            {"owner": schema, "tname": table},
        )
        cols = [(r[0], self.normalize_data_type(r[1])) for r in cur.fetchall()]

        # https://docs.oracle.com/en/database/oracle/oracle-database/21/refrn/ALL_CONSTRAINTS.html
        # about ALL_CONSTRAINTS
        cur.execute(
            """
            SELECT cols.COLUMN_NAME
            FROM ALL_CONSTRAINTS cons
            JOIN ALL_CONS_COLUMNS cols
            ON cons.CONSTRAINT_NAME = cols.CONSTRAINT_NAME
            AND cons.OWNER = cols.OWNER
            WHERE cons.CONSTRAINT_TYPE = 'P'
            AND cons.OWNER = :owner
            AND cons.TABLE_NAME = :tname
            ORDER BY cols.POSITION
            """,
            {"owner": schema, "tname": table},
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
        if not self.cur:
            self.cur = self.conn.cursor()
        cur = self.cur

        cur.arraysize = self.batch_size

        table = self.config["table"]
        schema, table_name = table.split(".")
        schema = schema.upper()
        table_name = table_name.upper()

        etl_schema = self.get_etl_schema()
        col_names = [x[0] for x in etl_schema["columns"]]

        # Oracle requires quoting using double-quotes for identifiers
        format_cols = ", ".join(f'"{c}"' for c in col_names)
        full_table = f'{schema}."{table_name}"'

        if self.state_manager:
            assert (
                self.state_manager.replication_key in col_names
            ), f"{self.state_manager.replication_key} is not in cols {col_names}"
            replication_key = self.state_manager.replication_key
            current_state = self.state_manager.get_state()
            logger.info(f"Replication key found: {replication_key}")

            if current_state:
                sql = (
                    f"SELECT {format_cols} FROM {full_table} "
                    f"WHERE \"{replication_key}\" >= :state "
                    f"ORDER BY \"{replication_key}\""
                )
                params = {"state": current_state}
            else:
                sql = (
                    f"SELECT {format_cols} FROM {full_table} "
                    f"ORDER BY \"{replication_key}\""
                )
                params = {}
        else:
            sql = f"SELECT {format_cols} FROM {full_table}"
            params = {}

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
