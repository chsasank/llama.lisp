(define-app
    (version "2.1.2")
    (ports 3000)
    (let ((db-password ,(gen-password)))
        (containers
            (container 
                (name "db")
                (image "docker.io/library/postgres:16")
                (volumes
                    ("pgdata" "/var/lib/postgresql/data"))
                (environment 
                    ("POSTGRES_DB" "flowise")
                    ("POSTGRES_USER" "flowise")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container 
                (name "flowise")
                (image "docker.io/flowiseai/flowise:2.1.2")
                (volumes 
                    ("flowise" "/root/flowise"))
                (environment 
                    ("PORT" 3000)
                    ("FLOWISE_USERNAME" "user")
                    ("FLOWISE_PASSWORD" ,(gen-password))
                    ("FLOWISE_FILE_SIZE_LIMIT" "50mb")
                    ("DATABASE_PATH" "/root/flowise")
                    ("DATABASE_TYPE" "postgres")
                    ("DATABASE_PORT" 5432)
                    ("DATABASE_HOST" "localhost")
                    ("DATABASE_NAME" "flowise")
                    ("DATABASE_USER" "flowise")
                    ("DATABASE_PASSWORD" ,db-password)
                    ("APIKEY_PATH" "/root/flowise")
                    ("SECRETKEY_PATH" "/root/flowise")
                    ("LOG_PATH" "/root/flowise/logs")
                    ("BLOB_STORAGE_PATH" "/root/flowise/storage")
                    ("DISABLE_FLOWISE_TELEMETRY" "true")
                    ("MODEL_LIST_CONFIG_JSON" "/root/models/models.json"))))))
