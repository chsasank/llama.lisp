(define-app
    ; name of the app is taken from file name
    (version "1.10.0")
    (ports 7861)
    (containers
        (container
            (name "automatic1111")
            (build "localhost/johnaic/automatic1111:1.10.0")
            (command "bash webui.sh -f --listen --xformers")
            (environment
                ("venv_dir" "/apps/deps/venv")
                ("install_dir" "/apps/automatic1111"))
            (volumes
                ("deps" "/apps/deps/")
                ("data" "/apps/automatic1111/"))
            (additional-flags "--device nvidia.com/gpu=all"))
        ; TODO: ditch for default auth of sd
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
