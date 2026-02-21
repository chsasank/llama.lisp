(define-app
    (version "0.6.2")
    (ports 8081 8080)
    (url "https://github.com/open-webui/open-webui/")
    (let ((postgres_password ,(gen-password)))
        (containers
            (container
                (name "ollama")
                (image "docker.io/ollama/ollama:0.11.10")
                (volumes
                    ("models" "/root/.ollama"))
                (additional-flags "--device nvidia.com/gpu=all"))
            (container
                (name "open-webui")
                (image "ghcr.io/open-webui/open-webui:0.6.2")
                (additional-flags "--device nvidia.com/gpu=all")
                (volumes
                    ("open-webui-data" "/app/backend/data"))
                (environment
                    ("VECTOR_DB" "pgvector")
                    ("DATABASE_URL" ,(format "postgres://postgres:{}@localhost:5432/webui" ,postgres_password))
                    ("DEFAULT_MODELS" "llama3.1:8b-instruct-q8_0")
                    ("OLLAMA_BASE_URL" "http://localhost:11434")))
            (container 
                (name "pgvector")
                (image "docker.io/pgvector/pgvector:pg17")
                (volumes 
                    ("database-data" "/var/lib/postgresql/data"))
                (environment 
                    ("POSTGRES_USER" postgres)
                    ("POSTGRES_PASSWORD" ,postgres_password)
                    ("POSTGRES_DB" webui)))
            (container
                (name "open-webui-pipelines")
                ; released on 30 March
                (image "ghcr.io/open-webui/pipelines:git-275655f")
                (volumes
                    ("open-webui-pipelines" "/app/pipelines")))
            (container
                (name "caddy")
                (image "docker.io/caddy:2.8.4-alpine")
                (environment
                    ("OLLAMA_API_KEY" ,(gen-password)))
                (volumes
                    ("Caddyfile" "/etc/caddy/Caddyfile"))))))
