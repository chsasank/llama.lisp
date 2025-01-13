(define-app
    (version "0.5.0")
    (ports 8000)
    (url "https://github.com/fedirz/faster-whisper-server")
    (containers
        (container
            (name "server")
            (image "docker.io/fedirz/faster-whisper-server:0.5.0-cuda")
            (volumes
                ("models" "/root/.cache/huggingface"))
            (additional-flags "--device nvidia.com/gpu=all"))))
