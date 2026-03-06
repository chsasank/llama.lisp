# Clickhouse and clickstack


Clickhouse is a OLAP. Edit passwords in `compose.clickhouse.yaml` 

```
podman-compose -f compose.clickhouse.yaml up
```

## Installing Clickstack

Once setup, login to clickhouse and create a new user. Make sure to change the password


```
CREATE DATABASE otel;
CREATE USER hyperdx_ingest IDENTIFIED WITH sha256_password BY 'ClickH0u3eRocks123!';
GRANT SELECT, INSERT, CREATE DATABASE, CREATE TABLE, CREATE VIEW ON otel.* TO hyperdx_ingest;
```

Copy example.env to .env

```
cp example.env .env
```

Edit .env file with above create clickhouse username/password
