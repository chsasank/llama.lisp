(define-app
    (version "1")
    (ports 3000)
    (let ((postgres_password ,(gen-password))
          (server-name ,(interactive-input "enter your mattermost domain address/server address without ending it with a '/'"))) 
        (containers 
            (container
                (name "outline")
                (image "outlinewiki/outline:0.81.1")
                (volumes
                    ("storage-data" "/var/lib/outline/data"))
                (environment 
                    ("URL" ,(interactive-input "URL of the publicly exposed outline address ")) 
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
                    ("OIDC_CLIENT_ID" ,(interactive-input "please enter OIDC_CLIENT_ID for mattermost"))
                    ("OIDC_CLIENT_SECRET" ,(interactive-input "please enter OIDC_CLIENT_SECRET for mattermost"))
                    ("OIDC_AUTH_URI" ,(format "{}/oauth/authorize",server-name))
                    ("OIDC_TOKEN_URI" ,(format"{}/oauth/access_token",server-name))
                    ("OIDC_USERINFO_URI" ,(format"{}/api/v4/users/me",server-name))
                    ("OIDC_DISPLAY_NAME" Mattermost)))                 
            (container 
                (name "redis")
                (image "redis")
                (volumes 
                    ("redis.conf" "/redis.conf"))
                (environment
                    ("REDIS_URL" redis://localhost:6379))
                (command "redis-server")
                (command "/redis.conf"))
            (container 
                (name "postgres")
                (image "postgres")
                (volumes 
                    ("database-data" "/var/lib/postgresql/data"))
                (environment 
                    ("POSTGRES_USER" user)
                    ("POSTGRES_PASSWORD" ,postgres_password)
                    ("POSTGRES_DB" outline))))))