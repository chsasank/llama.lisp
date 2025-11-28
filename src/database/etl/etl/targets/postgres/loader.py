import psycopg2
from psycopg2 import sql
import io
import os
from common.driver_base import TargetDriver

class PostgresTarget(TargetDriver):
    def __init__(self, config):
        self.config = config
        self.conn = self._connect()   # ONE long-lived connection
        self.cur = self.conn.cursor() # Reusable cursor

    def reconnect(self):
        try:
            self.conn.close()
        except:
            pass
        self.conn = self._connect()
        self.cur = self.conn.cursor()


    def _connect(self):
        cfg = self.config["connection"]
        return psycopg2.connect(
            host=cfg["host"],
            port=cfg["port"],
            user=cfg["user"],
            password=cfg["password"],
            dbname=cfg["database"]
        )

    def ensure_schema(self, columns):
        schema = self.config["schema"]
        table  = self.config["table"]

        conn = self.conn
        cur = self.cur

        # Create schema if missing
        cur.execute(f"CREATE SCHEMA IF NOT EXISTS {schema}")

        # Check existing table
        cur.execute("""
            SELECT COUNT(*)
            FROM information_schema.tables
            WHERE table_schema=%s AND table_name=%s
        """, (schema, table))

        exists = cur.fetchone()[0] > 0

        # 1. Create table if missing
        if not exists:
            print(f"[Schema] Creating new table {schema}.{table}")

            col_defs = []
            for col, dtype in columns:
                if dtype in ("integer", "numeric", "bigint", "smallint"):
                    pg_type = dtype
                elif "timestamp" in dtype:
                    pg_type = dtype
                else:
                    pg_type = "TEXT"
                col_defs.append(f"{col} {pg_type}")

            ddl = f"CREATE TABLE {schema}.{table} ({', '.join(col_defs)})"
            cur.execute(ddl)

        # 2. SCHEMA DRIFT: add missing columns
        cur.execute("""
            SELECT column_name
            FROM information_schema.columns
            WHERE table_schema=%s AND table_name=%s
        """, (schema, table))

        existing_cols = {r[0] for r in cur.fetchall()}

        for col, dtype in columns:
            if col not in existing_cols:
                # print(f"[Schema Drift] Adding missing column: {col}")

                # map to pg type
                if dtype in ("integer", "numeric", "bigint", "smallint"):
                    pg_type = dtype
                elif "timestamp" in dtype:
                    pg_type = dtype
                else:
                    pg_type = "TEXT"

                cur.execute(
                    sql.SQL("ALTER TABLE {}.{} ADD COLUMN {} {}")
                    .format(
                        sql.Identifier(schema),
                        sql.Identifier(table),
                        sql.Identifier(col),
                        sql.SQL(pg_type)
                    )
                )

        # 3. Ensure UNIQUE(msn_id, ts)
        cur.execute("""
            SELECT tc.constraint_name,
                array_agg(kcu.column_name ORDER BY kcu.ordinal_position)::text
            FROM information_schema.table_constraints tc
            JOIN information_schema.key_column_usage kcu
                ON tc.constraint_name = kcu.constraint_name
            WHERE tc.table_schema=%s
            AND tc.table_name=%s
            AND tc.constraint_type='UNIQUE'
            GROUP BY tc.constraint_name
        """, (schema, table))

        constraints = cur.fetchall()

        unique_exists = False
        for cname, cols_text in constraints:
            cols = cols_text.strip("{}").split(",")
            cols = [c.strip() for c in cols]

            if set(cols) == {"msn_id", "ts"}:
                unique_exists = True
                break

        if not unique_exists:
            # print(f"[Schema] Adding UNIQUE(msn_id, ts) to {schema}.{table}")
            cur.execute(
                sql.SQL("""
                    ALTER TABLE {}.{}
                    ADD CONSTRAINT {} UNIQUE (msn_id, ts)
                """).format(
                    sql.Identifier(schema),
                    sql.Identifier(table),
                    sql.Identifier(f"{table}_msn_ts_uk")
                )
            )

        conn.commit()


    def load_batch(self, columns, rows):
        schema = self.config["schema"]
        table  = self.config["table"]

        conn = self.conn
        cur = self.cur

        temp = f"temp_{table}_{os.getpid()}"

        cur.execute(
            sql.SQL("CREATE TEMP TABLE {} (LIKE {}.{} INCLUDING ALL)")
            .format(
                sql.Identifier(temp),
                sql.Identifier(schema),
                sql.Identifier(table)
            )
        )


        buf = io.StringIO()
        for r in rows:
            buf.write("\t".join(str(v) if v is not None else "\\N" for v in r) + "\n")
        buf.seek(0)

        cur.copy_from(buf, temp, sep="\t", columns=[c for c, _ in columns])

        update_cols = [
            sql.SQL("{}=EXCLUDED.{}").format(sql.Identifier(c), sql.Identifier(c))
            for c, _ in columns if c not in ("msn_id", "ts")
        ]

        merge = sql.SQL("""
            INSERT INTO {s}.{t} ({cols})
            SELECT {cols} FROM {tmp}
            ON CONFLICT (msn_id, ts)
            DO UPDATE SET {updates}
        """).format(
            s=sql.Identifier(schema),
            t=sql.Identifier(table),
            tmp=sql.Identifier(temp),
            cols=sql.SQL(", ").join(sql.Identifier(c) for c, _ in columns),
            updates=sql.SQL(", ").join(update_cols)
        )

        cur.execute(merge)
        cur.execute(sql.SQL("DROP TABLE {}").format(sql.Identifier(temp)))
        conn.commit()
