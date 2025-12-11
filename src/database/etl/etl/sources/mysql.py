import logging
import MySQLdb
import MySQLdb.cursors

from etl.common import ETLDataTypes, SourceDriver, StateManagerDriver

logger = logging.getLogger(__name__)


class MysqlSource(SourceDriver):
    def __init__(self, config, state_manager=None, batch_size=10000):
        self.config = config
        self.conn = self._connect()
        self.cur = None
        self.batch_size = batch_size
        self.state_manager = state_manager
        if self.state_manager is not None:
            assert isinstance(self.state_manager, StateManagerDriver)

    def _connect(self):
        cfg = self.config["connection"]
        conn = MySQLdb.connect(
            host=cfg["host"],
            port=cfg.get("port", 3306),
            user=cfg["user"],
            passwd=cfg["password"],
            db=cfg["database"],
            charset="utf8mb4",
            cursorclass=MySQLdb.cursors.Cursor,
        )
        return conn

    def get_all_tables(self):
        cur = self.conn.cursor()

        # Fetch all base tables except system schemas
        cur.execute(
            """
                SELECT TABLE_SCHEMA, TABLE_NAME
                FROM INFORMATION_SCHEMA.TABLES
                WHERE TABLE_TYPE = 'BASE TABLE'
                AND TABLE_SCHEMA NOT IN ('information_schema', 'mysql', 'performance_schema', 'sys')
                ORDER BY TABLE_SCHEMA, TABLE_NAME
            """
        )

        rows = cur.fetchall()
        tables = [f"{schema}.{table}" for schema, table in rows]
        return tables


    def normalize_data_type(self, dtype):
        dtype = dtype.lower()
        if dtype in ("int", "integer", "smallint", "mediumint", "bigint", "tinyint"):
            return ETLDataTypes.INTEGER
        elif dtype in ("float", "double", "decimal", "numeric"):
            return ETLDataTypes.FLOAT
        elif dtype in ("bit", "bool", "boolean"):
            return ETLDataTypes.BOOLEAN
        elif dtype in ("datetime", "timestamp"):
            return ETLDataTypes.DATE_TIME
        elif dtype == "date":
            return ETLDataTypes.DATE
        elif dtype == "time":
            return ETLDataTypes.TIME
        elif dtype in ("json"):
            return ETLDataTypes.JSON
        elif dtype in (
            "char", "varchar",
            "text", "tinytext", "mediumtext", "longtext",
            "enum", "set", "year"
        ):
            return ETLDataTypes.STRING
        elif dtype in (
            "blob", "tinyblob", "mediumblob", "longblob",
            "binary", "varbinary"
        ):
            return ETLDataTypes.BYTES
        else:
            raise ValueError(f"Unknown data type: {dtype}")

    # In the below function if the user gives schema.table, we use that schema; 
    # otherwise we fall back to the default database from the connection, 
    # so the query never fails due to missing database.
    def _parse_table(self):
        db_default = self.config["connection"]["database"]
        table = self.config["table"]

        if "." in table:
            schema, table_name = table.split(".", 1)
        else:
            schema = db_default
            table_name = table

        return schema, table_name

    # Schema extraction
    def get_etl_schema(self):
        schema, table_name = self._parse_table()
        cur = self.conn.cursor()

        # Column definitions
        cur.execute(
            """
            SELECT COLUMN_NAME, DATA_TYPE
            FROM INFORMATION_SCHEMA.COLUMNS
            WHERE TABLE_SCHEMA=%s AND TABLE_NAME=%s
            ORDER BY ORDINAL_POSITION
            """,
            (schema, table_name),
        )
        cols = [(r[0], self.normalize_data_type(r[1])) for r in cur.fetchall()]

        # Primary key
        cur.execute(
            """
            SELECT COLUMN_NAME
            FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
            WHERE TABLE_SCHEMA=%s
              AND TABLE_NAME=%s
              AND CONSTRAINT_NAME='PRIMARY'
            ORDER BY ORDINAL_POSITION
            """,
            (schema, table_name),
        )
        primary_cols = [r[0] for r in cur.fetchall()]

        return {"columns": cols, "primary_keys": primary_cols}

    # Save replication key
    def _save_state(self, batch, etl_schema):
        if self.state_manager:
            col_names = [c[0] for c in etl_schema["columns"]]
            rk_idx = col_names.index(self.state_manager.replication_key)
            last_row_state = batch[-1][rk_idx]
            self.state_manager.set_state(last_row_state)
            logger.info(
                f"Replication key {self.state_manager.replication_key} updated with {last_row_state}"
            )

    def stream_batches(self):
        if not self.cur:
            self.cur = self.conn.cursor()

        cur = self.cur
        cur.arraysize = self.batch_size

        etl_schema = self.get_etl_schema()
        col_names = [x[0] for x in etl_schema["columns"]]
        format_cols = ", ".join(f"`{c}`" for c in col_names)

        schema, table_name = self._parse_table()
        full_table = f"`{schema}`.`{table_name}`"

        # Replication mode
        if self.state_manager:
            replication_key = self.state_manager.replication_key
            assert replication_key in col_names, (
                f"{replication_key} not found in cols {col_names}"
            )
            current_state = self.state_manager.get_state()
            logger.info(f"Replication key found: {replication_key}")

            if current_state is not None:
                sql = (
                    f"SELECT {format_cols} FROM {full_table} "
                    f"WHERE `{replication_key}` >= %s "
                    f"ORDER BY `{replication_key}`"
                )
                params = (current_state,)
            else:
                sql = (
                    f"SELECT {format_cols} FROM {full_table} "
                    f"ORDER BY `{replication_key}`"
                )
                params = ()
        else:
            # Full extraction
            sql = f"SELECT {format_cols} FROM {full_table}"
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
