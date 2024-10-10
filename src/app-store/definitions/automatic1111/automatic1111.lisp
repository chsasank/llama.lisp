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
                ("venv_dir" "/apps/deps/venv")
                ("install_dir" "/apps/automatic1111"))
            (volumes
                ("deps" "/apps/deps/")
                ("data" "/apps/automatic1111/")))
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
