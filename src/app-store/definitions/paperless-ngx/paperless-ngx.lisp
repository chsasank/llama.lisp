(define-app
    (version "v2.12.0")
    (ports 8000)
    (let ((db-password ,(gen-password)))
        (containers 
            (container
                (name "broker")
                (image "docker.io/library/redis:7")
                (volumes 
                    ("redisdata" "/data"))) 
            (container 
                (name "db")
                (image "docker.io/library/postgres:16")
                (volumes
                    ("pgdata" "/var/lib/postgresql/data"))
                (environment 
                    ("POSTGRES_DB" "paperless")
                    ("POSTGRES_USER" "paperless")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container 
                (name "webserver")
                (image "ghcr.io/paperless-ngx/paperless-ngx:2.12.0")
                (volumes 
                    ("data" "/usr/src/paperless/data")
                    ("media" "/usr/src/paperless/media")
                    ("export" "/usr/src/paperless/export")
                    ("consume" "/usr/src/paperless/consume")) 
                (environment 
                    ("PAPERLESS_OCR_LANGUAGE" "eng")
                    ("PAPERLESS_ADMIN_USER" "admin")
                    ("PAPERLESS_ADMIN_PASSWORD" ,(gen-password))
                    ("PAPERLESS_REDIS" "redis://localhost:6379")
                    ("PAPERLESS_DBPASS" ,db-password)
                    ("PAPERLESS_DBHOST" "localhost"))))))

