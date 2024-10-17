(define-app
    (version "1.12.0")
    (ports 6333 6334)
        (containers
            (container
                (name "qdrant")
                (image "docker.io/qdrant/qdrant:v1.12.0")
                (volumes 
                    ("qdrant_storage" "/qdrant/storage"))
                (environment
                    ("QDRANT__SERVICE__API_KEY" ,(gen-password)) 
                    ("QDRANT__SERVICE__READ_ONLY_API_KEY" ,(gen-password))
                    ("QDRANT__SERVICE__JWT_RBAC" "true")))))
