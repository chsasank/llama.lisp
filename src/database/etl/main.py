import json
import psycopg2
import os
import time
import io
import sys
from psycopg2 import sql
from datetime import datetime
from psycopg2.extras import RealDictCursor
from tinydb import TinyDB, Query
from decimal import Decimal

# CONFIGURATION : Please chnage the configuration as per you database.
# Here i have selected PostgreSQL as my source and target databases.
SOURCE_DB = "postgres://user:password@host:port/database"      
TARGET_DB = "postgres://user:password@host:port/database" 

SOURCE_TABLE_NAME = "tap_mssql__mdm.t_blp_tp"
TARGET_TABLE_NAME = "t_blp_tp"
REPLICATION_KEY_COLUMN = "ts"
BATCH_SIZE = 100000
DATE_START = "2025-01-01"
DATE_END   = "2025-01-11"

progress_path = os.path.join(os.path.expanduser("~"), ".etl_progress")
os.makedirs(progress_path, exist_ok=True)
PROGRESS_DB = TinyDB(os.path.join(progress_path, "state.json"))
ProgressTable = Query()

# Converting datatime to JSON
def to_json_safe(v):
    if isinstance(v, datetime):
        return v.isoformat()
    if isinstance(v, Decimal):
        return float(v)
    return v

# FETCH SOURCE TABLE SCHEMA → SINGER SCHEMA
def get_singer_schema():
    src_schema, src_table = SOURCE_TABLE_NAME.split(".")

    conn = psycopg2.connect(SOURCE_DB)
    cur = conn.cursor()
    cur.execute("""
        SELECT column_name, data_type
        FROM information_schema.columns
        WHERE table_schema=%s AND table_name=%s
        ORDER BY ordinal_position
    """, (src_schema, src_table))

    cols = cur.fetchall()
    conn.close()

    props = {}
    for name, dtype in cols:

        if dtype in ("integer", "bigint", "smallint"):
            props[name] = {"type": ["null", "integer"]}

        elif dtype in ("numeric", "double precision", "real"):
            props[name] = {"type": ["null", "number"]}

        elif "timestamp" in dtype:
            props[name] = {"type": ["null", "string"], "format": "date-time"}

        else:
            props[name] = {"type": ["null", "string"]}

    return {"type": "object", "properties": props}


# CREATE TARGET TABLE AUTOMATICALLY FROM SINGER SCHEMA
def ensure_target_table_exists():
    print("\n[Schema Init] Creating target table from Singer schema...")

    schema = get_singer_schema()["properties"]
    src_schema, _ = SOURCE_TABLE_NAME.split(".")

    tgt_conn = psycopg2.connect(TARGET_DB)
    cur = tgt_conn.cursor()

    # Create schema
    cur.execute(
        sql.SQL("CREATE SCHEMA IF NOT EXISTS {}")
        .format(sql.Identifier(src_schema))
    )

    # Build CREATE TABLE DDL
    col_defs = []
    for col, spec in schema.items():

        if "integer" in spec["type"]:
            coltype = "BIGINT"

        elif "number" in spec["type"]:
            coltype = "DOUBLE PRECISION"

        elif spec.get("format") == "date-time":
            coltype = "TIMESTAMP WITHOUT TIME ZONE"

        else:
            coltype = "TEXT"

        col_defs.append(f"{col} {coltype}")

    ddl = f"""
        CREATE TABLE IF NOT EXISTS {src_schema}.{TARGET_TABLE_NAME} 
        ({', '.join(col_defs)});
    """

    cur.execute(ddl)

    # Add UPSERT KEY
    try:
        cur.execute(
            f"""
            ALTER TABLE {src_schema}.{TARGET_TABLE_NAME}
            ADD CONSTRAINT {TARGET_TABLE_NAME}_pkey
            UNIQUE (msn_id, ts);
            """
        )
    except psycopg2.Error:
        tgt_conn.rollback()

    tgt_conn.commit()
    cur.close()
    tgt_conn.close()

    print("[Schema Init] Done.")


# SINGER SCHEMA EMISSION (LIGHTWEIGHT)
def emit_stream_schema():
    schema = get_singer_schema()
    emit_schema(TARGET_TABLE_NAME, schema, ["msn_id", "ts"])


# CHECKPOINT STATE
def read_last_checkpoint():
    return PROGRESS_DB.get(ProgressTable.table_name == SOURCE_TABLE_NAME)

def update_last_checkpoint(ts_value, msn_id, total_rows):
    PROGRESS_DB.upsert(
        {
            "table_name": SOURCE_TABLE_NAME,
            "last_ts": ts_value.isoformat() if isinstance(ts_value, datetime) else ts_value,
            "last_msn_id": msn_id,
            "total_rows_copied": total_rows
        },
        ProgressTable.table_name == SOURCE_TABLE_NAME
    )


# STREAM SOURCE → RESPECT CHECKPOINT
def stream_source_rows(last_ts, last_msn):

    conn = psycopg2.connect(SOURCE_DB)
    cur = conn.cursor(name="stream", cursor_factory=RealDictCursor)

    # Base SQL
    sql_query = """
        SELECT *
        FROM {} 
        WHERE ts >= %s AND ts < %s
    """.format(SOURCE_TABLE_NAME.replace(".", "."))

    params = [DATE_START, DATE_END]

    # Apply checkpoint correctly
    if last_ts:
        sql_query += " AND (ts > %s OR (ts = %s AND msn_id > %s))"
        params.extend([last_ts, last_ts, last_msn])



    # Always ordered
    sql_query += " ORDER BY ts, msn_id"

    cur.execute(sql_query, params)

    while True:
        batch = cur.fetchmany(BATCH_SIZE)
        if not batch:
            break
        yield batch

    cur.close()
    conn.close()


# LOAD BATCH INTO TARGET (FIXED FOR MISSING COLUMNS)
def load_batch_into_target(rows):

    src_schema, _ = SOURCE_TABLE_NAME.split(".")

    tgt_conn = psycopg2.connect(TARGET_DB)
    meta_cur = tgt_conn.cursor()

    # Fetch target columns
    meta_cur.execute("""
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema=%s AND table_name=%s
        ORDER BY ordinal_position
    """, (src_schema, TARGET_TABLE_NAME))

    target_columns = [r[0] for r in meta_cur.fetchall()]
    meta_cur.close()

    # Fill missing data from Singer schema
    for row in rows:
        for col in target_columns:
            if col not in row:
                row[col] = None

    colnames = target_columns
    temp_tbl = f"temp_{TARGET_TABLE_NAME}_{os.getpid()}"

    cur = tgt_conn.cursor()

    # Temp table
    cur.execute(
        sql.SQL("CREATE TEMP TABLE {t} (LIKE {schema}.{table} INCLUDING ALL)")
        .format(
            t=sql.Identifier(temp_tbl),
            schema=sql.Identifier(src_schema),
            table=sql.Identifier(TARGET_TABLE_NAME)
        )
    )

    # COPY FROM buffer
    buf = io.StringIO()
    for r in rows:
        vals = [(str(r[c]) if r[c] is not None else "\\N") for c in colnames]
        buf.write("\t".join(vals) + "\n")

    buf.seek(0)
    cur.copy_from(buf, temp_tbl, sep="\t", columns=colnames)

    # UPSERT
    update_parts = [
        sql.SQL("{c}=excluded.{c}").format(c=sql.Identifier(c))
        for c in colnames if c not in ("msn_id", "ts")
    ]

    merge = sql.SQL("""
        INSERT INTO {schema}.{table} ({cols})
        SELECT {cols} FROM {tmp}
        ON CONFLICT (msn_id, ts)
        DO UPDATE SET {updates}
    """).format(
        schema=sql.Identifier(src_schema),
        table=sql.Identifier(TARGET_TABLE_NAME),
        tmp=sql.Identifier(temp_tbl),
        cols=sql.SQL(", ").join(sql.Identifier(c) for c in colnames),
        updates=sql.SQL(", ").join(update_parts)
    )

    cur.execute(merge)
    tgt_conn.commit()
    cur.close()
    tgt_conn.close()


def run_simple_etl():

    print("\n---> ETL Started <---\n")

    ensure_target_table_exists()

    checkpoint = read_last_checkpoint()
    last_ts = checkpoint["last_ts"] if checkpoint else None
    last_msn = checkpoint["last_msn_id"] if checkpoint else None

    total_previously = checkpoint["total_rows_copied"] if checkpoint else 0

    print("Starting from checkpoint:", (last_ts, last_msn))
    print("Rows previously copied :", total_previously, "\n")

    total_session_rows = 0
    total_write_time = 0.0
    full_start = time.time()

    for batch in stream_source_rows(last_ts, last_msn):

        # Write to target and measure write time
        write_start = time.time()
        load_batch_into_target(batch)
        total_write_time += (time.time() - write_start)

        # Row & checkpoint update
        batch_size = len(batch)
        total_session_rows += batch_size

        # Absolute total rows (previous + this run)
        new_total = total_previously + total_session_rows

        # Saving the checkpoint
        latest_row = max(batch, key=lambda r: (r["ts"], r["msn_id"]))
        highest_ts = latest_row["ts"]
        highest_msn = latest_row["msn_id"]

        update_last_checkpoint(highest_ts, highest_msn, new_total)


        # Progress print
        print(
            f"\rProgress: {new_total:,} rows | Last TS: {highest_ts} | MSN: {highest_msn}",
            end="", flush=True
        )

    total_exec_time = time.time() - full_start
    overall_total = total_previously + total_session_rows

    print("\n\n-------- ETL SUMMARY --------")
    print(f"Rows Loaded This Run : {total_session_rows:,}")
    print(f"Total Rows Overall   : {overall_total:,}")
    print(f"Total Write Time     : {total_write_time:.2f}s")
    print(f"Total Execution      : {total_exec_time:.2f}s")
    print("-----------------------------\n")


if __name__ == "__main__":
    run_simple_etl()