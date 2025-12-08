import clickhouse_connect
import random
from databank.etl.common import TargetDriver, ETLDataTypes
import logging
import json

logger = logging.getLogger(__name__)


class ClickhouseTarget(TargetDriver):
    def __init__(self, config):
        self.config = config
        self.client = self._connect()

    def _connect(self):
        cfg = self.config["connection"]
        return clickhouse_connect.get_client(
            host=cfg["host"],
            port=cfg["port"],
            username=cfg["user"],
            password=cfg["password"],
            database=cfg["database"],
        )

    def ch_data_types(self, etl_dtype):
        if etl_dtype == ETLDataTypes.INTEGER:
            return "Int64"
        elif etl_dtype == ETLDataTypes.FLOAT:
            return "Float64"
        elif etl_dtype == ETLDataTypes.BOOLEAN:
            return "UInt8"
        elif etl_dtype == ETLDataTypes.DATE_TIME:
            return "DateTime64(3)"
        elif etl_dtype == ETLDataTypes.DATE:
            return "Date"
        elif etl_dtype == ETLDataTypes.TIME:
            return "String"
        elif etl_dtype == ETLDataTypes.TIME_INTERVAL:
            return "String"
        elif etl_dtype == ETLDataTypes.JSON:
            return "Json"
        elif etl_dtype == ETLDataTypes.STRING:
            return "String"
        elif etl_dtype == ETLDataTypes.BYTES:
            return "String"
        else:
            raise ValueError(f"Unknown data type: {etl_dtype}")

    def ensure_schema(self, etl_schema):
        database = self.config["connection"]["database"]
        table = self.config["table"]
        full_table = f"{database}.`{table}`"

        # It will check wheather table is exists or not
        result = self.client.query(
            f"""
            SELECT COUNT()
            FROM system.tables
            WHERE database = '{database}' AND name = '{table}'
        """
        )
        exists = result.result_rows[0][0] > 0

        # create a new table
        if not exists:
            col_defs = []
            primary_keys = etl_schema["primary_keys"]
            for col, etl_dtype in etl_schema["columns"]:
                ch_type = self.ch_data_types(etl_dtype)
                if col in primary_keys:
                    col_defs.append(f"`{col}` {ch_type}")
                else:
                    col_defs.append(f"`{col}` Nullable({ch_type})")

            if primary_keys:
                order_keys = ", ".join(primary_keys)
                ddl = f"""CREATE TABLE {full_table} ({", ".join(col_defs)})
                    ENGINE = ReplacingMergeTree() ORDER BY ({order_keys})"""
            else:
                ddl = f"""CREATE TABLE {full_table} ({", ".join(col_defs)})
                    ENGINE = MergeTree()"""

            logger.info(f"Creating ClickHouse table: {ddl}")
            self.client.command(ddl)

        # deal with schema drift
        result = self.client.query(
            f"""SELECT name
            FROM system.columns
            WHERE database = '{database}' AND table = '{table}'"""
        )
        existing_cols = {r[0] for r in result.result_rows}

        for col, etl_dtype in etl_schema["columns"]:
            if col not in existing_cols:
                ch_type = self.ch_data_types(etl_dtype)
                logger.warning(f"[Schema Drift] Adding col {col} ({ch_type})")
                self.client.command(
                    f"ALTER TABLE {full_table} ADD COLUMN `{col}` {ch_type}"
                )

    def load_batch(self, rows, etl_schema):
        database = self.config["connection"]["database"]
        table = self.config["table"]
        full_table = f"{database}.`{table}`"

        columns = [col for col, _ in etl_schema["columns"]]
        col_types = [dtype for _, dtype in etl_schema["columns"]]

        # Bulk insert (ReplacingMergeTree performs UPSERT automatically)
        self.client.insert(full_table, rows, column_names=columns)
