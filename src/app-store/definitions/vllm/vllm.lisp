(define-app
    (version "0.6.2")
    (ports 8080)
    (url "https://github.com/open-webui/open-webui/")
    (containers
        (container
            (name "vllm")
            (build "localhost/johnaic/vllm:0.6.2")
            (environment
                ("HF_TOKEN" "")
                ("MY_MODEL" "neuralmagic/Meta-Llama-3.1-8B-Instruct-FP8")
                ("MY_CONTEXT_SIZE" "32768")
                ("MY_GPU_UTILIZATION" "1.0")
                ("MY_KV_CACHE_DTYPE" "fp8"))
            (volumes
                ("models" "/root/.cache/huggingface")
                ("init.sh" "/root/init.sh"))
            (command "bash /root/init.sh")
            (additional-flags "--device nvidia.com/gpu=all"))
        (container
            (name "caddy")
            (image "docker.io/caddy:2.8.4-alpine")
            (environment
                ("VLLM_API_KEY" ,(gen-password)))
            (volumes
                ("Caddyfile" "/etc/caddy/Caddyfile")))))
