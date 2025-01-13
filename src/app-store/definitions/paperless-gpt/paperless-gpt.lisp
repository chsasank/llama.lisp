(define-app
    (version "0.5.1")
    (ports)
    (containers
        (container
            (name "ollama")
            (image "docker.io/ollama/ollama:0.4.1")
            (volumes
                ("models" "/root/.ollama")))
        (container
            (name "paperless-gpt")
            (image "docker.io/icereed/paperless-gpt:v0.5.1")
            (volumes
                ("prompts" "/app/prompts"))
            (environment
                ("PAPERLESS_BASE_URL" "http://paperless.example.com")
                ("PAPERLESS_API_TOKEN" "my-token-here")
                ("LLM_PROVIDER" "ollama")
                ("LLM_MODEL" "llama3.1")))))
