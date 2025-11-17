import psycopg2
import os
from psycopg2.extras import RealDictCursor
from tinydb import TinyDB, Query

# Please update User,Password, Host, Port and Database according to your source and target databases.
# Here i have selected PostgreSQL as my source and target databases.
SOURCE_DB = "postgres://user:password@host:port/database"      
TARGET_DB = "postgres://user:password@host:port/database"      

progress_path = os.path.join(os.path.expanduser("~"), ".etl_progress")
os.makedirs(progress_path, exist_ok=True)

PROGRESS_DB = TinyDB(os.path.join(progress_path, "state.json"))
ProgressTable = Query()

SOURCE_TABLE_NAME = "tap_mssql__mdm.t_blp_tp"
TARGET_TABLE_NAME = "t_blp_tp"
REPLICATION_KEY_COLUMN = "ts"
BATCH_SIZE = 10000

def read_last_checkpoint():
    result = PROGRESS_DB.get(ProgressTable.table_name == SOURCE_TABLE_NAME)
    return result

def update_last_checkpoint(new_value, total_rows):
    existing = PROGRESS_DB.get(ProgressTable.table_name == SOURCE_TABLE_NAME)

    if existing is None:
        PROGRESS_DB.insert({
            "table_name": SOURCE_TABLE_NAME,
            "last_value": str(new_value),
            "total_rows_copied": total_rows
        })
    else:
        PROGRESS_DB.update(
            {
                "last_value": str(new_value),
                "total_rows_copied": total_rows
            },
            ProgressTable.table_name == SOURCE_TABLE_NAME
        )

def stream_source_rows(last_processed_value):
    source_connection = psycopg2.connect(SOURCE_DB)
    source_connection.autocommit = False

    source_cursor = source_connection.cursor(
        name="streaming_cursor",
        cursor_factory=RealDictCursor
    )

    if last_processed_value is not None:
        select_query = f"""
            SELECT * FROM {SOURCE_TABLE_NAME}
            WHERE {REPLICATION_KEY_COLUMN} IS NOT NULL
              AND {REPLICATION_KEY_COLUMN} > %s
            ORDER BY {REPLICATION_KEY_COLUMN}
        """
        source_cursor.execute(select_query, (last_processed_value,))
    else:
        select_query = f"""
            SELECT * FROM {SOURCE_TABLE_NAME}
            WHERE {REPLICATION_KEY_COLUMN} IS NOT NULL
            ORDER BY {REPLICATION_KEY_COLUMN}
        """
        source_cursor.execute(select_query)

    while True:
        row_batch = source_cursor.fetchmany(BATCH_SIZE)
        
        if len(row_batch) == 0:
            break

        yield row_batch

    source_cursor.close()
    source_connection.close()

def load_batch_into_target(row_batch):
    target_connection = psycopg2.connect(TARGET_DB)
    target_cursor = target_connection.cursor()

    first_row = row_batch[0]
    column_names = list(first_row.keys())

    formatted_column_names = ",".join(f'"{column}"' for column in column_names)
    placeholders = ",".join(["%s"] * len(column_names))

    update_parts = []
    for column in column_names:
        if column not in ("msn_id", "ts"):
            update_parts.append(f'"{column}" = EXCLUDED."{column}"')

    if len(update_parts) > 0:
        update_clause = ", ".join(update_parts)
    else:
        update_clause = ""

    insert_sql = f"""
        INSERT INTO {TARGET_TABLE_NAME} ({formatted_column_names})
        VALUES ({placeholders})
    """

    if update_clause != "":
        insert_sql = insert_sql + f" ON CONFLICT (msn_id, ts) DO UPDATE SET {update_clause}"

    batch_data = []
    for row in row_batch:
        row_tuple = tuple(row[column] for column in column_names)
        batch_data.append(row_tuple)

    target_cursor.executemany(insert_sql, batch_data)
    target_connection.commit()

    target_cursor.close()
    target_connection.close()

def run_simple_etl():
    print("\n---> ETL Process Started <---")

    checkpoint_record = read_last_checkpoint()
    if checkpoint_record is None:
        last_checkpoint_value = None
        total_rows_copied = 0
    else:
        last_checkpoint_value = checkpoint_record["last_value"]
        total_rows_copied = checkpoint_record.get("total_rows_copied", 0)

    print("Starting from checkpoint:", last_checkpoint_value)
    print("Total rows previously copied:", total_rows_copied)

    for row_batch in stream_source_rows(last_checkpoint_value):
        batch_size = len(row_batch)
        total_rows_copied += batch_size

        highest_value_in_batch = None
        for row in row_batch:
            current_value = row[REPLICATION_KEY_COLUMN]
            if highest_value_in_batch is None or current_value > highest_value_in_batch:
                highest_value_in_batch = current_value

        load_batch_into_target(row_batch)
        update_last_checkpoint(highest_value_in_batch, total_rows_copied)

        print(
            f"\rLoaded {total_rows_copied} rows. "
            f"Checkpoint: {highest_value_in_batch}",
            end="",
            flush=True
        )

    print("---> ETL Process Completed <---\n")

if __name__ == "__main__":
    run_simple_etl()