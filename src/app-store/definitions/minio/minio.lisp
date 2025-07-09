(define-app
    (version "2025-04-22T22-12-26Z")
    (ports 9000 9001)
    (let ((minio_secret_key ,(gen-password)))
        (containers
            (container
                (name "minio")
                (image "minio/minio:RELEASE.2025-04-22T22-12-26Z")
                (volumes 
                    ("minio_data" "/data"))
                (environment 
                    ("MINIO_ROOT_USER" "root")
                    ("MINIO_ROOT_PASSWORD" ,minio_secret_key))
                (command "server /data --console-address ':9001'")))))
                