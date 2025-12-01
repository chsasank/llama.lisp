#!/usr/bin/env bash
cd "$(dirname "$0")"

# create testing container
TEST_SOURCE_DB=etl-test-source-db
podman kill $TEST_SOURCE_DB && podman rm $TEST_SOURCE_DB || true
podman run -d --replace \
    --name $TEST_SOURCE_DB \
    -e POSTGRES_USER=testing \
    -e POSTGRES_PASSWORD=intelarc \
    -e POSTGRES_DB=github \
    -p 5511:5432 \
    postgres

sleep 5

# put in testing data

# dump created using: pg_dump --no-owner -x -h 100.64.0.200 -p 5511 -U github -f ./test_pg.sql -Fc
podman run --rm \
    -v ./test_pg.sql:/data/test_pg.sql \
    -e PGPASSWORD=intelarc \
    --entrypoint=pg_restore \
    alpine/psql \
    --no-owner --no-privileges  -C -h host.containers.internal -U testing -p 5511 -d github -Fc /data/test_pg.sql


# Creating target database 
TEST_TARGET_DB=etl-test-target-db
podman kill $TEST_TARGET_DB && podman rm $TEST_TARGET_DB || true
podman run -d --replace \
    --name $TEST_TARGET_DB \
    -e POSTGRES_USER=testing \
    -e POSTGRES_PASSWORD=intelarc \
    -e POSTGRES_DB=github \
    -p 5512:5432 \
    postgres