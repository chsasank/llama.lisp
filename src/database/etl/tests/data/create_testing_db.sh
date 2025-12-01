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