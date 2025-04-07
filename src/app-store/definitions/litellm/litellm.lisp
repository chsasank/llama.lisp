(define-app
    (version "1.60.8")
    (ports 4000 9090)
    (let ((db-password ,(gen-password)))
        (containers
            (container
                (name "litellm")
                (image "ghcr.io/berriai/litellm:main-v1.60.8")
                (command "--config=/app/config.yaml")
                (volumes
                    ("config.yaml" "/app/config.yaml"))
                (environment
                    ("DATABASE_URL"  ,(format "postgres://llmproxy:{}@localhost:5432/litellm" ,db-password))
                    ("STORE_MODEL_IN_DB" "True")
                    ("LITELLM_MASTER_KEY" ,(format "sk-{}" ,(gen-password)))
                    ("LITELLM_SALT_KEY" ,(format "sk-{}" ,(gen-password)))))
            (container
                (name "db")
                (image "docker.io/library/postgres:16-alpine")
                (volumes
                    ("db-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_DB" "litellm")
                    ("POSTGRES_USER" "llmproxy")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container
                (name "prometheus-init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_worker.sh")
                (volumes
                    ("prometheus_data" "/prometheus")
                    ("init_worker.sh" "/mnt/init_worker.sh")
                    ))
            (container
                (name "prometheus")
                (image "docker.io/prom/prometheus:latest")
                (command "--config.file=/etc/prometheus/prometheus.yml --storage.tsdb.path=/prometheus --storage.tsdb.retention.time=15d")
                (volumes
                    ("prometheus_data" "/prometheus")
                    ("prometheus.yml" "/etc/prometheus/prometheus.yml"))))))
