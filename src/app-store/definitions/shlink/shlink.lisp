(define-app
    (version "4.4.0")
    (ports 8080)
    (let ((db-password ,(gen-password)))
        (containers 
            (container
                (name "postgresql")
                (image "docker.io/bitnami/postgresql:17")
                (volumes
                    ("postgresql_data" "/bitnami/postgresql"))
                (environment
                    ("POSTGRESQL_USERNAME" "shlink")
                    ("POSTGRESQL_DATABASE" "shlink")
                    ("POSTGRESQL_PASSWORD" ,db-password)))
            (container 
                (name "shlink")
                (image "docker.io/shlinkio/shlink:4.5.2")
                (environment 
                    ("INITIAL_API_KEY" ,(gen-password))
                    ("DEFAULT_DOMAIN" ,(interactive-input "Where is this going to be deployed?" "Example: sh.example.com"))
                    ("IS_HTTPS_ENABLED" "true")
                    ("TIMEZONE" "Asia/Kolkata")
                    ("DB_DRIVER" "postgres")
                    ("DB_NAME" "shlink")
                    ("DB_USER" "shlink")
                    ("DB_PASSWORD" ,db-password)
                    ("DB_HOST" "localhost"))))))
