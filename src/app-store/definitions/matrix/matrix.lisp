(define-app
    (version "1.116.0")
    (ports 8008)
    (url "https://github.com/open-webui/open-webui/")
    (let ((server-name "example.com")
          (db-password ,(gen-password)))
        (containers
            (container
                (name "synapse")
                (image "docker.io/matrixdotorg/synapse:v1.116.0")
                (entrypoint "bash")
                (command "/start.sh")
                (volumes
                    ("synapse-data" "/data")
                    ("start.sh" "/start.sh"))
                (environment
                    ("SYNAPSE_SERVER_NAME" ,server-name)
                    ("SYNAPSE_REPORT_STATS" "no")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container
                (name "postgres")
                (image "docker.io/library/postgres:12-alpine")
                (volumes
                    ("postgres-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_USER" "synapse")
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_INITDB_ARGS" "--encoding=UTF-8 --lc-collate=C --lc-ctype=C"))))))
