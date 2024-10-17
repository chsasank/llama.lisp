(define-app
    (version "0.18.2")
    (ports 8080)
    (url "https://github.com/matatonic/openedai-speech")
    (containers
        (container
            (name "server")
            (image "ghcr.io/matatonic/openedai-speech:0.18.2")
            (environment
                ("TTS_HOME" "voices")
                ("HF_HOME" "voices"))
            (volumes
                ("voices" "/app/voices")
                ("config" "/app/config"))
            (additional-flags "--device nvidia.com/gpu=all"))
        (container
            (name "caddy")
            (image "docker.io/caddy:2.8.4-alpine")
            (environment
                ("OPENEDAI_SPEECH_API_KEY" ,(gen-password)))
            (volumes
                ("Caddyfile" "/etc/caddy/Caddyfile")))))
