(define-app
    (version "2.4.1")
    (ports 8080)
    (let ((db-password ,(gen-password))
          (penpot-public-url ,(interactive-input "Penport URL" "Enter URL of the exposed penpot address. Example: https://penpot.johnaic.com"))
          (mattermost-server-name ,(interactive-input "Mattermost URL" "Enter your exposed mattermost domain address. We need it for authentication. Example: https://mattermost.johnaic.com"))
          (penpot-flags "enable-login-with-oidc disable-email-verification enable-oidc-registration disable-login-with-password"))
        (containers
            (container
                (name "frontend")
                (image "docker.io/penpotapp/frontend:2.4.1")
                (volumes 
                    ("penpot_assets" "/opt/data/assets"))
                (environment
                    ("PENPOT_BACKEND_URI" "http://localhost:6060")
                    ("PENPOT_EXPORTER_URI" "http://localhost:6061")
                    ("PENPOT_INTERNAL_RESOLVER" "8.8.8.8")
                    ("PENPOT_FLAGS" ,penpot-flags)))
            (container
                (name "backend")
                (image "docker.io/penpotapp/backend:2.4.1")
                (volumes
                    ("penpot_assets" "/opt/data/assets"))
                (environment 
                    ("PENPOT_FLAGS" ,penpot-flags)
                    ("PENPOT_PUBLIC_URI" ,penpot-public-url)
                    ("PENPOT_SECRET_KEY" ,(gen-password 64))

                    ("PENPOT_DATABASE_URI" "postgresql://localhost/penpot")                    
                    ("PENPOT_DATABASE_USERNAME" penpot)
                    ("PENPOT_DATABASE_PASSWORD" ,db-password)

                    ("PENPOT_REDIS_URI" "redis://localhost/0")
                    ("PENPOT_ASSETS_STORAGE_BACKEND" "assets-fs")
                    ("PENPOT_STORAGE_ASSETS_FS_DIRECTORY" "/opt/data/assets")
                    ("PENPOT_TELEMETRY_ENABLED" false)

                    ("PENPOT_OIDC_CLIENT_ID" ,(interactive-input "OIDC Client ID" "please enter OIDC_CLIENT_ID from mattermost. For tutorial on how to set the OIDC client_ID using mattermost, look at this guide: https://outline.von-neumann.ai/s/f13351f6-801c-4401-ac1b-839e51dd4aa4"))
                    ("PENPOT_OIDC_CLIENT_SECRET" ,(interactive-input "OIDC Secret"))
                    ("PENPOT_OIDC_BASE_URI" ,mattermost-server-name)
                    ("PENPOT_OIDC_AUTH_URI" ,(format "{}/oauth/authorize" ,mattermost-server-name))
                    ("PENPOT_OIDC_TOKEN_URI" ,(format "{}/oauth/access_token" ,mattermost-server-name))
                    ("PENPOT_OIDC_USER_URI" ,(format"{}/api/v4/users/me" ,mattermost-server-name))

                    ; smtp
                    ("PENPOT_SMTP_DEFAULT_FROM" "no-reply@example.com")
                    ("PENPOT_SMTP_DEFAULT_REPLY_TO" "no-reply@example.com")
                    ("PENPOT_SMTP_HOST" "localhost")
                    ("PENPOT_SMTP_PORT" "1025")
                    ("PENPOT_SMTP_USERNAME" "")
                    ("PENPOT_SMTP_PASSWORD" "")
                    ("PENPOT_SMTP_TLS" "false")
                    ("PENPOT_SMTP_SSL" "false")))
            (container
                (name "exporter")
                (image "docker.io/penpotapp/exporter:2.4.1")
                (environment
                    ("PENPOT_PUBLIC_URI" "http://localhost:8080")
                    ("PENPOT_REDIS_URI" "redis://localhost/0")))
            (container
                (name "postgres")
                (image "docker.io/library/postgres:15")
                (volumes
                    ("penpot_postgres" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_INITDB_ARGS" "--data-checksums")
                    ("POSTGRES_DB" "penpot")
                    ("POSTGRES_USER" "penpot")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_worker.sh")
                (volumes
                    ("penpot_assets" "/opt/data/assets")
                    ("init_worker.sh" "/mnt/init_worker.sh")))
            (container 
                (name "redis")
                (image "docker.io/library/redis:7.2"))
            (container
                (name "penpot-mailcatch")
                (image "docker.io/sj26/mailcatcher:latest")))))