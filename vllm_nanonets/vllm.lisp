(define-app
    (version "0.6.2")
    (ports 8080)
    (containers
        (container
            (name "vllm")
            (image "docker.io/vllm/vllm-openai:v0.14.1")
            (environment
                ("HF_TOKEN" "")
                ("MODEL" "nanonets/Nanonets-OCR2-3B"))
            (volumes
                ("models" "/root/.cache/huggingface"))
            (command "--model nanonets/Nanonets-OCR2-3B --max-model-len 32768 --gpu-memory-utilization 0.8 --kv-cache-dtype fp8 " )
            (additional-flags "--device nvidia.com/gpu=all"))
        (container
            (name "caddy")
            (image "docker.io/caddy:2.8.4-alpine")
            (environment
                ("VLLM_API_KEY" ,(gen-password)))
            (volumes
                ("Caddyfile" "/etc/caddy/Caddyfile")))))

