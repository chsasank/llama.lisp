(define-app
    (version "15")
    (ports 8080 8123 4318)
    (let (
          ;; ClickHouse configs
          (clickhouse-password ,(gen-password))
          (clickhouse-user ,(interactive-input "ClickHouse Username for Logs: " "Enter ClickHouse username. Example: hyperdx_ingest"))
          (clickhouse-database ,(interactive-input "ClickHouse Database for Logs: " "Enter ClickHouse DB name for logs. Example: JOHNAIC_Logs"))

          ;; HyperDX / Clickstack configs
          (hyperdx-api-port "8000")
          (hyperdx-app-port "8080")
          (hyperdx-app-url ,(interactive-input "HyperDX sitename: " "Enter HyperDX sitename. Example: https://logs.johnaic.com or http://localhost"))
          (hyperdx-log-level "debug")
          (clickhouse-endpoint "tcp://localhost:9000?dial_timeout=10s")
          )
        (containers

            ;; ClickHouse Database
            (container
                (name "clickhouse")
                (image "clickhouse/clickhouse-server:26.1")
                (restart "always")
                (ports
                    (8123 8123)
                    (9000 9000))
                (volumes
                    ("clickhouse-data" "/var/lib/clickhouse")
                    ("clickhouse-logs" "/var/log/clickhouse-server"))
                (environment
                    ("CLICKHOUSE_USER" ,clickhouse-user)
                    ("CLICKHOUSE_PASSWORD" ,clickhouse-password)
                    ("CLICKHOUSE_DB" ,clickhouse-database)
                    ("CLICKHOUSE_DEFAULT_ACCESS_MANAGEMENT" "1"))
                (healthcheck
                    ("CMD-SHELL" "clickhouse-client --query 'SELECT 1'")
                    (interval "10s")
                    (timeout "5s")
                    (retries 10))
            )

            ;; MongoDB
            (container
                (name "db")
                (image "mongo:5.0.32-focal")
                (restart "always")
                (volumes
                    ("mongodb-data" "/data/db"))
                (healthcheck
                    ("CMD-SHELL" "mongosh --eval 'db.adminCommand(\"ping\")'")
                    (interval "10s")
                    (timeout "5s")
                    (retries 10)))

            ;; OTEL Collector
            (container
                (name "otel-collector")
                (image "docker.clickhouse.com/clickhouse/clickstack-otel-collector:2")
                (restart "always")
                (depends-on
                    ("clickhouse" "healthy"))
                (ports
                    (3133 3133)
                    (4225 4225)
                    (4317 4317)
                    (4318 4318)
                    (8888 8888))
                (environment
                    ("CLICKHOUSE_ENDPOINT" ,clickhouse-endpoint)
                    ("CLICKHOUSE_USER" ,clickhouse-user)
                    ("CLICKHOUSE_PASSWORD" ,clickhouse-password)
                    ("HYPERDX_OTEL_EXPORTER_CLICKHOUSE_DATABASE" ,clickhouse-database)
                    ("HYPERDX_LOG_LEVEL" ,hyperdx-log-level)
                    ("HYPERDX_OTEL_EXPORTER_CREATE_LEGACY_SCHEMA" "true")))

            ;; HyperDX App
            (container
                (name "app")
                (image "docker.hyperdx.io/hyperdx/hyperdx:2")
                (restart "always")
                (depends-on
                    ("db" "healthy")
                    ("clickhouse" "healthy")
                    ("otel-collector" "started"))
                (ports
                    (,hyperdx-api-port ,hyperdx-api-port)
                    (,hyperdx-app-port ,hyperdx-app-port))
                
                (environment
                    ("FRONTEND_URL" ,hyperdx-app-url)
                    ("HYPERDX_API_PORT" ,hyperdx-api-port)
                    ("HYPERDX_APP_PORT" ,hyperdx-app-port)
                    ("HYPERDX_APP_URL" ,hyperdx-app-url)
                    ("HYPERDX_LOG_LEVEL" ,hyperdx-log-level)
                    ("MINER_API_URL" "http://miner:5123")
                    ("MONGO_URI" "mongodb://systemd-clickstack-db:27017/hyperdx")
                    ("SERVER_URL" "http://127.0.0.1:8000")
                    ("OPAMP_PORT" "4320")
                    ("OTEL_EXPORTER_OTLP_ENDPOINT" "http://systemd-clickstack-otel-collector:4318")
                    ("OTEL_SERVICE_NAME" "hdx-oss-app")
                    ("USAGE_STATS_ENABLED" "true")

                    ;; Needed for templates
                    ("CLICKHOUSE_HOST" "http://systemd-clickstack-clickhouse:8123")
                    ("CLICKHOUSE_OTEL_USERNAME" ,clickhouse-user)
                    ("CLICKHOUSE_OTEL_PASSWORD" ,clickhouse-password)
                    ("CLICKHOUSE_OTEL_DATABASE" ,clickhouse-database)
                )))))
