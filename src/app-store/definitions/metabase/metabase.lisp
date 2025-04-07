(define-app
    (version "0.53.5.2")
    (ports 3000)
    (let ((db-password ,(gen-password)))
        (containers
            (container
                (name "db")
                (image "docker.io/library/postgres:16")
                (volumes
                    ("db_storage" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_USER" "metabase")
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_DB" "metabaseappdb")))
            (container
                (name "metabase")
                (image "docker.io/metabase/metabase:v0.53.5.2")
                (environment
                   ("MB_DB_TYPE" "postgres")
                   ("MB_DB_DBNAME" "metabaseappdb")
                   ("MB_DB_PORT" "5432")
                   ("MB_DB_USER" "metabase")
                   ("MB_DB_PASS" ,db-password)
                   ("MB_DB_HOST" "localhost"))))))
