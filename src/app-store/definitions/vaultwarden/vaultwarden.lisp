(define-app
    (version "1.32.2")
    (ports 80)
    (containers 
        (container
            (name "vaultwarden") 
            (image "docker.io/vaultwarden/server:1.32.2-alpine")
            (environment 
                ("SIGNUPS_ALLOWED" "true"))
            (volumes
                ("vw-data" "/data")))))
