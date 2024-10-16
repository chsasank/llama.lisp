(define-app
    (version "0.6.2")
    (ports 8080)
    (url "https://github.com/open-webui/open-webui/")
    (containers
        (container
            (name "vllm")
            (build "localhost/johnaic/vllm:0.6.2")
            (environment
                ("HF_TOKEN" ""))
            (volumes
                ("models" "/root/.cache/huggingface"))
            (command
                "vllm serve --max_model_len 32768 --gpu_memory_utilization 1.0 --kv_cache_dtype=fp8 --disable-log-requests neuralmagic/Meta-Llama-3.1-8B-Instruct-FP8")
            (additional-flags "--device nvidia.com/gpu=all"))
        (container
            (name "caddy")
            (image "docker.io/caddy:2.8.4-alpine")
            (environment
                ("VLLM_API_KEY" ,(gen-password)))
            (volumes
                ("Caddyfile" "/etc/caddy/Caddyfile")))))
