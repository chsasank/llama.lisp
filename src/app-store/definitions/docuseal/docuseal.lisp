(define-app
    (version "1.7.5")
    (ports 3000)
    (let ((db-password ,(gen-password)))
        (containers
            (container 
                (name "docuseal")
                (image "docker.io/docuseal/docuseal:1.7.5")
                (volumes 
                    ("docuseal" "/data/docuseal"))
                (environment
                    ("FORCE_SSL" "false")
                    ("DATABASE_HOST" "localhost")
                    ("DATABASE_PORT" "5432")
                    ("DATABASE_USER" "postgres")
                    ("DATABASE_PASSWORD" ,db-password)
                    ("DATABASE_NAME" "docuseal")))
            (container
                (name "postgres")
                (image "docker.io/library/postgres:17")
                (volumes
                    ("pg_data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_USER" "postgres")
                    ("POSTGRES_DB" "docuseal")
                    ("POSTGRES_PASSWORD" ,db-password))))))
