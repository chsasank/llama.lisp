set -e

# create testing container
podman kill etl-test-db && podman rm etl-test-db || true
podman run -d --replace \
    --name etl-test-db \
    -e POSTGRES_USER=testing \
    -e POSTGRES_PASSWORD=intelarc \
    -e POSTGRES_DB=github \
    -p 5511:5432 \
    postgres

sleep 5

# put in testing data

# dump created using: pg_dump --no-owner -x -h 100.64.0.200 -p 5511 -U github -f ./test_pg.sql -Fc
PGPASSWORD="intelarc" pg_restore --no-owner --no-privileges  -C -h localhost -U testing -p 5511 -d github -Fc test_pg.sql
