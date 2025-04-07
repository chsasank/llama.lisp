(define-app
    (version "1.24.3")
    (ports 1337)
    (let ((server-url ,(interactive-input "server url" "This is the external URL. Example: https://planka.example.com")))
        (containers
            (container
                (name "planka")
                (image "ghcr.io/plankanban/planka:1.24.3")
                (volumes
                    ("user-avatars" "/app/public/user-avatars")
                    ("project-background-images" "/app/public/project-background-images")
                    ("attachments" "/app/private/attachments"))
                (environment
                    ("BASE_URL" ,server-url)
                    ("DATABASE_URL" "postgresql://postgres@localhost/planka")
                    ("SECRET_KEY" ,(gen-password))
                    ("DEFAULT_ADMIN_EMAIL" "admin@example.com")
                    ("DEFAULT_ADMIN_PASSWORD" ,(gen-password))
                    ("DEFAULT_ADMIN_NAME" "Admin Admin")
                    ("DEFAULT_ADMIN_USERNAME" "admin")))
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_worker.sh")
                (volumes 
                    ("user-avatars" "/app/public/user-avatars")
                    ("project-background-images" "/app/public/project-background-images")
                    ("attachments" "/app/private/attachments")
                    ("init_worker.sh" "/mnt/init_worker.sh")))             
            (container
                (name "postgres")
                (image "docker.io/library/postgres:16-alpine")
                (volumes
                    ("db-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_DB" "planka")
                    ("POSTGRES_HOST_AUTH_METHOD" "trust"))))))
