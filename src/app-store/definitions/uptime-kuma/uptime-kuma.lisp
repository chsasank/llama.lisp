(define-app
    (version "1.23.15")
    (ports 3001)
    (containers
        (container
            (name "uptime-kuma")
            (image "docker.io/louislam/uptime-kuma:1.23.15")
            (volumes
                ("data" "/app/data")))))
