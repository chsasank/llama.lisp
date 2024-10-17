(define-app
    (version "3.3.2")
    (ports 3000)
    (url "https://www.discourse.org/")
    (let ((db-user "bn_discourse")
          (db-password ,(gen-password))
          (db-name "bitnami_discourse")
          (redis-password ,(gen-password))
          (discourse-image "docker.io/bitnami/discourse:3.3.2")
          (discourse-host "www.example.com"))
        (containers
            (container
                (name "postgresql")
                (image "docker.io/bitnami/postgresql:17")
                (volumes
                    ("postgresql_data" "/bitnami/postgresql"))
                (environment
                    ("POSTGRESQL_USERNAME" ,db-user)
                    ("POSTGRESQL_DATABASE" ,db-name)
                    ("POSTGRESQL_PASSWORD" ,db-password)))
            (container
                (name "redis")
                (image "docker.io/bitnami/redis:7.4")
                (volumes
                    ("redis_data" "/bitnami/redis"))
                (environment
                    ("REDIS_PASSWORD" ,redis-password)))
            (container
                (name "discourse")
                (image ,discourse-image)
                (volumes
                    ("discourse_data" "/bitnami/discourse"))
                (environment
                    ("DISCOURSE_HOST" ,discourse-host)
                    ("DISCOURSE_DATABASE_HOST" "localhost")
                    ("DISCOURSE_DATABASE_PORT_NUMBER" 5432)
                    ("DISCOURSE_DATABASE_USER" ,db-user)
                    ("DISCOURSE_DATABASE_NAME" ,db-name)
                    ("DISCOURSE_DATABASE_PASSWORD" ,db-password)
                    ("DISCOURSE_REDIS_HOST" "localhost")
                    ("DISCOURSE_REDIS_PORT_NUMBER" 6379)
                    ("DISCOURSE_REDIS_PASSWORD" ,redis-password)))
            (container
                (name "sidekiq")
                (image ,discourse-image)
                (volumes
                    ("sidekiq_data" "/bitnami/discourse"))
                (command "/opt/bitnami/scripts/discourse-sidekiq/run.sh")
                (environment
                    ("DISCOURSE_HOST" ,discourse-host)
                    ("DISCOURSE_DATABASE_HOST" "localhost")
                    ("DISCOURSE_DATABASE_PORT_NUMBER" 5432)
                    ("DISCOURSE_DATABASE_USER" ,db-user)
                    ("DISCOURSE_DATABASE_NAME" ,db-name)
                    ("DISCOURSE_DATABASE_PASSWORD" ,db-password)
                    ("DISCOURSE_REDIS_HOST" "localhost")
                    ("DISCOURSE_REDIS_PORT_NUMBER" 6379)
                    ("DISCOURSE_REDIS_PASSWORD" ,redis-password))))))
