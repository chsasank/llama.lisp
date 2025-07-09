(define-app
    (version "1")
    (ports 3000)
    (let ((postgres_password ,(gen-password))
          (outline-url ,(interactive-input "Outline URL" "Enter URL of the exposed outline address. Example: https://wiki.johnaic.com")) 
          (mattermost-server-name ,(interactive-input "Mattermost URL" "Enter your exposed mattermost domain address. We need it for authentication. Example: https://mattermost.von-neumann.ai"))) 
        (containers 
            (container
                (name "outline")
                (image "docker.io/outlinewiki/outline:0.81.1")
                (volumes
                    ("storage-data" "/var/lib/outline/data"))
                (environment 
                    ("URL" ,outline-url) 
                    ("PGSSLMODE" disable)
                    ("NODE_ENV" "production")
                    ("SECRET_KEY" ,(gen-password-hex32))
                    ("UTILS_SECRET" ,(gen-password-hex32))
                    ("REDIS_URL" redis://localhost:6379)
                    ("PORT" "3000")
                    ("FILE_STORAGE" local)
                    ("FILE_STORAGE_LOCAL_ROOT_DIR" "/var/lib/outline/data")
                    ("FILE_STORAGE_UPLOAD_MAX_SIZE" 262144000)
                    ("DATABASE_URL" ,(format "postgres://user:{}@localhost:5432/outline",postgres_password))
                    ("FORCE_HTTPS" true)
                    ("WEB_CONCURRENCY" 1)
                    ("DEBUG" "http")
                    ("LOG_LEVEL" info)
                    ("DEFAULT_LANGUAGE" en_US)
                    ("RATE_LIMITER_ENABLED" true)
                    ("RATE_LIMITER_REQUESTS" 1000)
                    ("RATE_LIMITER_DURATION_WINDOW" 60)
                    ("OIDC_CLIENT_ID" ,(interactive-input "OIDC Client ID" "please enter OIDC_CLIENT_ID from mattermost. For tutorial on how to set the OIDC client_ID using mattermost, look at this guide: https://outline.von-neumann.ai/s/f13351f6-801c-4401-ac1b-839e51dd4aa4"))
                    ("OIDC_CLIENT_SECRET" ,(interactive-input "OIDC Secret"))
                    ("OIDC_AUTH_URI" ,(format "{}/oauth/authorize" ,mattermost-server-name))
                    ("OIDC_TOKEN_URI" ,(format"{}/oauth/access_token" ,mattermost-server-name))
                    ("OIDC_USERINFO_URI" ,(format"{}/api/v4/users/me" ,mattermost-server-name))
                    ("OIDC_DISPLAY_NAME" Mattermost)))                 
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_worker.sh")
                (volumes
                    ("storage-data" "/var/lib/outline/data")
                    ("init_worker.sh" "/mnt/init_worker.sh")))
            (container 
                (name "redis")
                (image "docker.io/library/redis")
                (volumes 
                    ("redis.conf" "/redis.conf"))
                (environment
                    ("REDIS_URL" redis://localhost:6379))
                (command "redis-server")
                (command "/redis.conf"))
            (container 
                (name "postgres")
                (image "docker.io/library/postgres")
                (volumes 
                    ("database-data" "/var/lib/postgresql/data"))
                (environment 
                    ("POSTGRES_USER" user)
                    ("POSTGRES_PASSWORD" ,postgres_password)
                    ("POSTGRES_DB" outline))))))