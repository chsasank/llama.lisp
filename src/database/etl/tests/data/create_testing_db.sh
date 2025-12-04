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

# after the database git created use below command for login to database
# podman exec -it etl-test-target-ch-db clickhouse-client 

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
# podman exec -it etl-test-source-mssql-db /opt/mssql-tools18/bin/sqlcmd -No -S localhost -U SA -P "Intelarc@123"

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

# Create Oracle Free database
TEST_SOURCE_DB=etl-test-source-oracle-db
ORACLE_PASSWORD="Intelarc123"

podman kill $TEST_SOURCE_DB && podman rm $TEST_SOURCE_DB || true

podman run -d --replace \
    --name $TEST_SOURCE_DB \
    -e ORACLE_PWD=$ORACLE_PASSWORD \
    -e ORACLE_CHARACTERSET=AL32UTF8 \
    -p 1521:1521 -p 5500:5500 \
    container-registry.oracle.com/database/free:latest

echo "==> Waiting for Oracle to become healthy..."

# readiness wait loop
while [ "$(podman inspect -f '{{.State.Health.Status}}' $TEST_SOURCE_DB)" != "healthy" ]; do
    echo "  → Oracle is still starting..."
    sleep 10
done

echo "==> Oracle is healthy!"

# After the Database got create use below command for login to database
# podman exec -it etl-test-source-oracle-db sqlplus system/Intelarc123@//localhost:1521/freepdb1

# Download sample schemas on host
SCRIPT_DIR="$(pwd)"

if [ -d "$SCRIPT_DIR/db-sample-schemas" ]; then
    echo "==> db-sample-schemas already exists → skipping download."
else
    echo "==> Downloading Oracle sample schemas into $SCRIPT_DIR/db-sample-schemas..."
    git clone https://github.com/oracle/db-sample-schemas.git "$SCRIPT_DIR/db-sample-schemas"
fi

# Copy to container
podman cp "$SCRIPT_DIR/db-sample-schemas" $TEST_SOURCE_DB:/opt/oracle/

sleep 5

# Installing HR schema (Human Resources)
echo "Installing HR schema..."
podman exec -i $TEST_SOURCE_DB bash <<EOF
expect <<EOD
spawn sqlplus system/$ORACLE_PASSWORD@//localhost:1521/freepdb1 "@/opt/oracle/db-sample-schemas/human_resources/hr_install.sql"
expect "Enter a password for the user HR:"
send "$ORACLE_PASSWORD\r"
expect "Enter a tablespace for HR*"
send "USERS\r"
expect "Do you want to overwrite the schema*"
send "YES\r"
expect eof
EOD
EOF

echo "HR schema installed"

# Installing CO schema (Customer Orders)
echo "Installing CO schema..."
podman exec -i $TEST_SOURCE_DB bash <<EOF
expect <<EOD
spawn sqlplus system/$ORACLE_PASSWORD@//localhost:1521/freepdb1 "@/opt/oracle/db-sample-schemas/customer_orders/co_install.sql"
expect "Enter password for the user CO"
send "$ORACLE_PASSWORD\r"
expect "Enter a tablespace for CM*"
send "USERS\r"
expect "Do you want to overwrite the schema*"
send "YES\r"
expect eof
EOD
EOF
echo "CO schema installed"

echo "==> HR and CO Oracle sample schemas installed successfully!"

echo "==> created $TEST_SOURCE_DB"
