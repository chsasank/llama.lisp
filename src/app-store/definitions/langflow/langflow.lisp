(define-app
    (version "v1.0.19")
    (ports 7860)
    (let ((db-password ,(gen-password)))
        (containers
            (container
                (name "langflow")
                (image "docker.io/langflowai/langflow:1.0.19")
                (environment
                    ("DO_NOT_TRACK" "true")
                    ("LANGFLOW_AUTO_LOGIN"  "False")
                    ("LANGFLOW_SUPERUSER" "admin")
                    ("LANGFLOW_SUPERUSER_PASSWORD" ,(gen-password))
                    ("POSTGRESQL_PASSWORD" ,db-password))
                (volumes
                    ("langflow-data" "/app/langflow")
                    ("init.sh" "/app/init.sh")))
            (container
                (name "postgres")
                (image "docker.io/bitnami/postgresql:17")
                (volumes
                    ("langflow-postgres" "/bitnami/postgresql"))
                (environment 
                    ("POSTGRESQL_PASSWORD" ,db-password)
                    ("POSTGRESQL_USERNAME" "langlow")
                    ("POSTGRESQL_DATABASE" "langflow"))))))
