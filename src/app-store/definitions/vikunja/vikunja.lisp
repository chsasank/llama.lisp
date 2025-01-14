(define-app
    (version "0.24.6")
    (ports 3456)
    (let ((db-password ,(gen-password))
          (public-url ,(interactive-input "Vikunja URL" "Enter URL where this is going to be deployed? Example: https://vikunja.example.com")))
        (containers
            (container
                (name "db")
                (image "docker.io/bitnami/postgresql:17")
                (volumes
                    ("psql_data" "/bitnami/postgresql"))
                (environment
                    ("POSTGRESQL_USERNAME" "vikunja")
                    ("POSTGRESQL_DATABASE" "vikunja")
                    ("POSTGRESQL_PASSWORD" ,db-password)))
            (container
                (name "vikunja")
                (image "docker.io/vikunja/vikunja:0.24.6")
                (volumes
                    ("vikunja_files" "/app/vikunja/files"))
                (environment
                    ("VIKUNJA_SERVICE_PUBLICURL" ,public-url)
                    ("VIKUNJA_DATABASE_HOST" "localhost")
                    ("VIKUNJA_DATABASE_PASSWORD" ,db-password)
                    ("VIKUNJA_DATABASE_TYPE" "postgres")
                    ("VIKUNJA_DATABASE_USER" "vikunja")
                    ("VIKUNJA_DATABASE_DATABASE" "vikunja")
                    ("VIKUNJA_SERVICE_JWTSECRET" ,(gen-password)))))))
