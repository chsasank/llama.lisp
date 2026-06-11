(define-app
    (version "3.1")
    (ports 3000)
    (let ((db-password ,(gen-password))
          (app-secret ,(gen-password 32)))
        (containers
            (container
                (name "umami")
                (image "ghcr.io/umami-software/umami:3.1")
                (environment
                    ("DATABASE_URL" ,(format "postgresql://umami:{}@localhost:5432/umami" ,db-password))
                    ("APP_SECRET" ,app-secret)))
            (container
                (name "db")
                (image "docker.io/postgres:15-alpine")
                (volumes
                    ("db-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_DB" "umami")
                    ("POSTGRES_USER" "umami")
                    ("POSTGRES_PASSWORD" ,db-password))))))
