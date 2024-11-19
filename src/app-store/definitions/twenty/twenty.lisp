(define-app
    (version "0.31.2")
    (ports 3000)
    (let ((db-password ,(gen-password))
          (server-url ,(interactive-input "server url" "This is the external URL"))
          (access-token-secret ,(gen-password))
          (login-token-secret ,(gen-password))
          (refresh-token-secret ,(gen-password))
          (file-token-secret ,(gen-password)))
        (containers
            (container
                (name "server")
                (image "twentycrm/twenty:v0.31.2")
                (volumes
                    ("server-local-data" "/app/packages/twenty-server/.local-storage")
                    ("docker-data" "/app/docker-data")
                    ("entrypoint.sh" "/app/entrypoint.sh"))
                (environment
                    ("PORT" "3000")
                    ("PG_DATABASE_URL" "postgres://twenty:twenty@localhost:5432/default")
                    ("SERVER_URL" ,server-url)
                    ("FRONT_BASE_URL" ,server-url)
                    ("REDIS_PORT" "6379")
                    ("REDIS_HOST" "localhost")

                    ("ENABLE_DB_MIGRATIONS" "true")
                    ("SIGN_IN_PREFILLED" "true")
                    ("STORAGE_TYPE" "local")

                    ("ACCESS_TOKEN_SECRET" ,access-token-secret)
                    ("LOGIN_TOKEN_SECRET" ,login-token-secret)
                    ("REFRESH_TOKEN_SECRET" ,refresh-token-secret)
                    ("FILE_TOKEN_SECRET" ,file-token-secret)))
            (container
                (name "worker")
                (image "twentycrm/twenty:v0.31.2")
                (command "yarn worker:prod")
                (environment
                    ("PG_DATABASE_URL" "postgres://twenty:twenty@localhost:5432/default")
                    ("SERVER_URL" ,server-url)
                    ("FRONT_BASE_URL" ,server-url)
                    ("REDIS_PORT" "6379")
                    ("REDIS_HOST" "localhost")

                    ("ENABLE_DB_MIGRATIONS" "false")
                    ("STORAGE_TYPE" "local")

                    ("ACCESS_TOKEN_SECRET" ,access-token-secret)
                    ("LOGIN_TOKEN_SECRET" ,login-token-secret)
                    ("REFRESH_TOKEN_SECRET" ,refresh-token-secret)
                    ("FILE_TOKEN_SECRET" ,file-token-secret)))
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_worker.sh")
                (volumes
                    ("server-local-data" "/mnt/server-local-data")
                    ("docker-data" "/mnt/docker-data")
                    ("init_worker.sh" "/mnt/init_worker.sh")))
            (container
                (name "db")
                (image "docker.io/twentycrm/twenty-postgres:v0.31.2")
                (volumes
                    ("db-data" "/bitnami/postgresql"))
                (environment
                    ("POSTGRESQL_PASSWORD" ,db-password)))
            (container
                (name "redis")
                (image "docker.io/library/redis")))))
