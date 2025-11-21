# Simple batch-based ETL pipeline

This script is about to run a simple ETL pipeline that reads data from the source database in small batches and writes it into a target database. It does not attempt to load the full table into memory.

### Why this ETL is safe?
Instead of using `fetchall()`, which loads everything into memory, the script uses a server-side cursor along with `fetchmany()`. This streams a limited number of rows at a time, ensuring predictable and low memory usage even for large tables.
The loading step uses `UPSERT (INSERT ... ON CONFLICT DO UPDATE)`, so it never creates duplicates. Even if a batch is replayed due to a crash, the target table still stays correct.

### Storing checkpoints with TinyDB
This ETL maintains a minimal state using TinyDB, which stores progress in a small JSON file inside the user’s home directory.

This file records:
* the last processed timestamp (last_value)
* the total number of rows copied so far (total_rows_copied)

If the script is restarted even after days, it will reads the JSON state and resumes exactly where it got stopped.

No SQL tables are required for progress tracking. The entire state lives in a lightweight JSON file.

### What we need to do before running?
Before starting the ETL:
1. Create the target table manually in the target PostgreSQL database. The structure must match the source table, including the primary key (`msn_id`, `ts`).

2. Install TinyDB if not already installed:
```
pip install tinydb
```
3. Update the connection URLs at the top of the script to match your source and target databases credentials.

### What will happen when this script runs?
When the script runs: 
1. It reads the last saved checkpoint (if anything stored) from the JSON file.
2. It streams rows from the source using a server-side cursor.
3. Each batch is inserted into the target table with UPSERT logic.
4. The checkpoint and row count are updated in the JSON file.
5. A single console line repeatedly updates to show progress in terminal. As shown below:
```
Loaded 350000 rows. Checkpoint: 2025-01-01 05:30:00
```

The script can be stopped at any time and we can restart it safely.