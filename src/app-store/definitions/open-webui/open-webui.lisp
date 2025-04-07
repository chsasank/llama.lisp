(define-app
    (version "0.5.7")
    (ports 8081 8080)
    (url "https://github.com/open-webui/open-webui/")
    (containers
        (container
            (name "ollama")
            (image "docker.io/ollama/ollama:0.5.7")
            (volumes
                ("models" "/root/.ollama"))
            (additional-flags "--device nvidia.com/gpu=all"))
        (container
            (name "open-webui")
            (image "ghcr.io/open-webui/open-webui:0.5.7")
            (volumes
                ("open-webui-data" "/app/backend/data"))
            (environment
                ("DEFAULT_MODELS" "llama3.1:8b-instruct-q8_0")
                ("OLLAMA_BASE_URL" "http://localhost:11434")))
        (container
            (name "caddy")
            (image "docker.io/caddy:2.8.4-alpine")
            (environment
                ("OLLAMA_API_KEY" ,(gen-password)))
            (volumes
                ("Caddyfile" "/etc/caddy/Caddyfile")))))
