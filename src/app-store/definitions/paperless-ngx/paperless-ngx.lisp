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
                (name "postgresql")
                (image "docker.io/bitnami/postgresql:17")
                (volumes
                    ("postgresql_data" "/bitnami/postgresql"))
                (environment
                    ("POSTGRESQL_USERNAME" "paperless")
                    ("POSTGRESQL_DATABASE" "paperless")
                    ("POSTGRESQL_PASSWORD" ,db-password)))
            (container 
                (name "webserver")
                (image "ghcr.io/paperless-ngx/paperless-ngx:2.14.7")
                (volumes 
                    ("data" "/usr/src/paperless/data")
                    ("media" "/usr/src/paperless/media")
                    ("export" "/usr/src/paperless/export")
                    ("consume" "/usr/src/paperless/consume"))
                (environment 
                    ("PAPERLESS_OCR_LANGUAGE" "eng")
                    ("PAPERLESS_ADMIN_USER" "admin")
                    ("PAPERLESS_URL" ,(interactive-input "Where is this hosted" "This is the external URL. Example: https://paperless.example.com"))
                    ("PAPERLESS_ADMIN_PASSWORD" ,(gen-password))
                    ("PAPERLESS_REDIS" "redis://localhost:6379")
                    ("PAPERLESS_DBPASS" ,db-password)
                    ("PAPERLESS_DBENGINE" "postgresql")
                    ("PAPERLESS_DBNAME" "paperless")
                    ("PAPERLESS_DBUSER" "paperless")
                    ("PAPERLESS_DBHOST" "localhost"))))))
