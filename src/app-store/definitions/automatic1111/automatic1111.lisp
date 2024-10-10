(define-app
    ; name of the app is taken from file name
    (version "1.10.0")
    (ports 7861)
    (containers
        (container
            (name "automatic1111")
            (image "docker.io/johnaic/automatic1111:1.10.0")
            (additional-flags "--device nvidia.com/gpu=all")
            (environment
                ("venv_dir" "/apps/deps/venv"))
            (volumes
                ("deps" "/apps/deps/")
                ("repos" "/apps/automatic111/repositories")
                ("models" "/apps/automatic111/models")))
        (let ((user "user")
              (password ,(gen-password))
              (hashed-password ,(hash-password ,password)))
            (container
                (name "caddy")
                (image "docker.io/caddy:2.8.4-alpine")
                (environment
                    ("HTTP_BASIC_AUTH_USER" ,user)
                    ("HTTP_BASIC_AUTH_PASSWORD" ,password)
                    ("HTTP_BASIC_AUTH_PASSWORD_HASHED" ,hashed-password))
                (volumes
                    ("Caddyfile" "/etc/caddy/Caddyfile"))))))