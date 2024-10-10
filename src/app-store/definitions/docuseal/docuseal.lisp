(define-app
    (version "1.7.5")
    (ports 3000)
    (let ((db_password ,(gen-password)))
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
                    ("DATABASE_PASSWORD" ,db_password)
                    ("DATABASE_NAME" "docuseal")))
            (container 
                (name "postgres")
                (image "docker.io/postgres:15")
                (volumes 
                    ("pg_data" "/var/lib/postgresql/data"))
                (environment 
                    ("POSTGRES_USER" "postgres")
                    ("POSTGRES_PASSWORD" ,db_password)
                    ("POSTGRES_DB" "docuseal"))))))
