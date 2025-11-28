import json
import yaml
from common.state_manager import StateManager
from extractors.postgres.extractor import PostgresSource
from loaders.postgres.loader import PostgresTarget

DRIVERS = {
    "postgres": {
        "source": PostgresSource,
        "target": PostgresTarget
    }
}

def load_config():
    with open("etl.yml", "r") as f:
        return yaml.safe_load(f)

def run():
    cfg = load_config()
    state = StateManager.load()

    source_type = cfg["source"]["type"]
    target_type = cfg["target"]["type"]

    SourceClass = DRIVERS[source_type]["source"]
    TargetClass = DRIVERS[target_type]["target"]

    with open(cfg["source"]["config"], "r") as f:
        source_config = json.load(f)

    with open(cfg["target"]["config"], "r") as f:
        target_config = json.load(f)

    source = SourceClass(source_config)
    target = TargetClass(target_config)

    columns = source.get_columns()
    target.ensure_schema(columns)

    last_ts = state.get("last_ts")
    last_msn = state.get("last_msn_id")
    total_rows = state.get("total_rows", 0)

    batch_size = cfg["batch_size"]

    # Precompute column indexes
    col_names = [col for col, _ in columns]
    ts_idx = col_names.index("ts")
    msn_idx = col_names.index("msn_id")

    # Show previously loaded only once
    print(f"Previously loaded: {total_rows:,} rows")

    for batch in source.stream_rows(last_ts, last_msn, batch_size):

        target.load_batch(columns, batch)

        last = batch[-1]
        last_ts = str(last[ts_idx])
        last_msn = last[msn_idx]
        total_rows += len(batch)

        # Save checkpoint
        StateManager.save({
            "last_ts": last_ts,
            "last_msn_id": last_msn,
            "total_rows": total_rows
        })

        print(
            f"\r[Progress] Extracting {len(batch):,} rows per batch | Loaded {total_rows:,} rows",
            end="",
            flush=True
        )

    print(f"\n[Done] Total rows loaded: {total_rows:,}")


if __name__ == "__main__":
    run()

# if __name__ == "__main__":
#     import cProfile
#     import pstats
#     import os

#     PROFILE_FILE = "etl_run.prof"

#     print("[Profiler] Running ETL with cProfile...")

#     cProfile.run("run()", PROFILE_FILE)

#     print(f"[Profiler] Profile saved to {PROFILE_FILE}")
#     print("To visualize it:")
#     print("    snakeviz etl_run.prof")
