(define-app
    (version "1.123.11")
    (ports 5678)
    (let ((db-password ,(gen-password))
          (non-root-db-password ,(gen-password)))
        (containers
            (container
                (name "db")
                (image "docker.io/library/postgres:16")
                (volumes
                    ("db_storage" "/var/lib/postgresql/data")
                    ("init-data.sh" "/docker-entrypoint-initdb.d/init-data.sh"))
                (environment
                    ("POSTGRES_USER" "n8n")
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_DB" "n8n")
                    ("POSTGRES_NON_ROOT_USER" "n8n_2")
                    ("POSTGRES_NON_ROOT_PASSWORD" ,non-root-db-password)))
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_worker.sh")
                (volumes
                    ("n8n_storage" "/home/node/.n8n")
                    ("init_worker.sh" "/mnt/init_worker.sh")))
            (container
                (name "n8n")
                (image "docker.io/n8nio/n8n:1.123.11")
                (volumes
                    ("n8n_storage" "/home/node/.n8n"))
                (environment
                   ("DB_TYPE" "postgresdb")
                   ("DB_POSTGRESDB_HOST" "localhost")
                   ("DB_POSTGRESDB_PORT" "5432")
                   ("DB_POSTGRESDB_DATABASE" "n8n")
                   ("DB_POSTGRESDB_USER" "n8n_2")
                   ("DB_POSTGRESDB_PASSWORD" ,non-root-db-password))))))
