(define-app
    ; name of the app is taken from file name
    (version "0.2.2")
    (ports 8189)
    (containers
        (container
            (name "comfyui")
            (build "localhost/johnaic/comfyui:0.2.2")
            (additional-flags "--device nvidia.com/gpu=all")
            (volumes
                ("checkpoints" "/apps/comfyui/models/checkpoints")))
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
