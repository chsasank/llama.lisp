#!/usr/bin/env bash
cd "$(dirname "$0")"

# create testing container
TEST_SOURCE_DB=etl-test-source-psql-db
podman kill $TEST_SOURCE_DB && podman rm $TEST_SOURCE_DB || true
podman run -d --replace \
    --name $TEST_SOURCE_DB \
    -e POSTGRES_USER=testing \
    -e POSTGRES_PASSWORD=intelarc \
    -e POSTGRES_DB=github \
    -p 5511:5432 \
    postgres

sleep 5
echo "==> created $TEST_SOURCE_DB"

# put in testing data

# dump created using: pg_dump --no-owner -x -h 100.64.0.200 -p 5511 -U github -f ./test_pg.sql -Fc
podman run --rm \
    -v ./test_pg.sql:/data/test_pg.sql \
    -e PGPASSWORD=intelarc \
    --entrypoint=pg_restore \
    alpine/psql \
    --no-owner --no-privileges  -C -h host.containers.internal -U testing -p 5511 -d github -Fc /data/test_pg.sql
echo "==> populated $TEST_SOURCE_DB with test data"


# Creating target database 
TEST_TARGET_DB=etl-test-target-psql-db
podman kill $TEST_TARGET_DB && podman rm $TEST_TARGET_DB || true
podman run -d --replace \
    --name $TEST_TARGET_DB \
    -e POSTGRES_USER=testing \
    -e POSTGRES_PASSWORD=intelarc \
    -e POSTGRES_DB=github \
    -p 5512:5432 \
    postgres

sleep 5
echo "==> created $TEST_TARGET_DB"

# Creating clikhouse database
TEST_TARGET_DB=etl-test-target-ch-db
podman kill $TEST_TARGET_DB && podman rm $TEST_TARGET_DB || true
podman run -d --replace \
    --name $TEST_TARGET_DB \
    -e CLICKHOUSE_USER=testing \
    -e CLICKHOUSE_PASSWORD=intelarc \
    -e CLICKHOUSE_DB=github \
    -e CLICKHOUSE_DEFAULT_ACCESS_MANAGEMENT=1 \
    -p 8123:8123 \
    -p 9000:9000 \
    clickhouse/clickhouse-server

sleep 5
echo "==> created $TEST_TARGET_DB"

# Creating MSSQL database
TEST_SOURCE_DB=etl-test-source-mssql-db
MSSQL_PASSWORD=Intelarc@123
podman kill $TEST_SOURCE_DB && podman rm $TEST_SOURCE_DB || true
podman run -d --replace \
    --name $TEST_SOURCE_DB \
    -e ACCEPT_EULA=Y \
    -e MSSQL_SA_PASSWORD=$MSSQL_PASSWORD \
    -e MSSQL_PID=Developer \
    -p 1433:1433 \
    mcr.microsoft.com/mssql/server:2025-latest

# After the Database got create use below command for login to database
# podman exec -it etl-test-target-mssql-db /opt/mssql-tools18/bin/sqlcmd -No -S localhost -U SA -P "Intelarc@123"

# Creating directory inside the container
podman exec -it $TEST_SOURCE_DB mkdir /var/opt/mssql/backup

# It will download the file if not exists
SCRIPT_DIR="$(pwd)"

if [ -f "$SCRIPT_DIR/wwi.bak" ]; then
    echo "wwi.bak already exists → skipping download."
else
    echo "wwi.bak not found → downloading..."
    curl -L -o "$SCRIPT_DIR/wwi.bak" \
      'https://github.com/Microsoft/sql-server-samples/releases/download/wide-world-importers-v1.0/WideWorldImporters-Full.bak'
fi

sleep 10

# To copy the file to the container
podman cp "$SCRIPT_DIR/wwi.bak" $TEST_SOURCE_DB:/var/opt/mssql/backup

# To restore the database inside the container
podman exec -it $TEST_SOURCE_DB /opt/mssql-tools18/bin/sqlcmd -No \
   -S localhost -U sa -P $MSSQL_PASSWORD \
   -Q 'RESTORE DATABASE WideWorldImporters FROM DISK = "/var/opt/mssql/backup/wwi.bak" WITH MOVE "WWI_Primary" TO "/var/opt/mssql/data/WideWorldImporters.mdf", MOVE "WWI_UserData" TO "/var/opt/mssql/data/WideWorldImporters_userdata.ndf", MOVE "WWI_Log" TO "/var/opt/mssql/data/WideWorldImporters.ldf", MOVE "WWI_InMemory_Data_1" TO "/var/opt/mssql/data/WideWorldImporters_InMemory_Data_1"'

sleep 5
echo "==> created $TEST_SOURCE_DB"