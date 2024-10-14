(define-app
    (version "v1.117.0")
    (ports 3001)
    (let ((db-password "postgres")
          (db-username "postgres")
          (db-database-name "immich"))
        (containers
            (container
                (name "server")
                (image "ghcr.io/immich-app/immich-server:v1.117.0")
                (volumes
                    ("immich-data" "/usr/src/app/upload")
                    ("/etc/localtime" "/etc/localtime"))
                (environment
                    ("DB_HOSTNAME" "localhost")
                    ("DB_PASSWORD" ,db-password)
                    ("DB_USERNAME" ,db-username)
                    ("DB_DATABASE_NAME" ,db-database-name)
                    ("REDIS_HOSTNAME" "localhost")
                    ("IMMICH_MACHINE_LEARNING_URL" "http://localhost:3003")))
            (container
                (name "machine-learning")
                (image "ghcr.io/immich-app/immich-machine-learning:pr-13375")
                (volumes
                    ("model-cache" "/cache"))
                (environment
                    ("DB_HOSTNAME" "localhost")
                    ("DB_PASSWORD" ,db-password)
                    ("DB_USERNAME" ,db-username)
                    ("DB_DATABASE_NAME" ,db-database-name)
                    ("REDIS_HOSTNAME" "localhost")))
            (container
                (name "redis")
                (image "docker.io/redis:6.2-alpine"))
            (container
                (name "postgres")
                (image "docker.io/tensorchord/pgvecto-rs:pg14-v0.2.0")
                (volumes
                  ("postgres-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_USER" ,db-username)
                    ("POSTGRES_DB" ,db-database-name)
                    ("POSTGRES_INITDB_ARG" "--data-checksums"))
                (command "postgres -c shared_preload_libraries=vectors.so -c 'search_path=\"$user\", public, vectors' -c logging_collector=on -c max_wal_size=2GB -c shared_buffers=512MB -c wal_compression=on")))))
