(define-app
    (version "v1.0.19")
    (ports 7860)
    (let ((db-password ,(gen-password)))
        (containers
            (container
                (name "langflow")
                (image "docker.io/langflowai/langflow:1.4.1")
                (command "bash /app/init.sh")
                (environment
                    ("DO_NOT_TRACK" "true")
                    ("LANGFLOW_AUTO_LOGIN"  "False")
                    ("LANGFLOW_SUPERUSER" "admin")
                    ("LANGFLOW_SUPERUSER_PASSWORD" ,(gen-password))
                    ("POSTGRESQL_PASSWORD" ,db-password))
                (volumes
                    ("langflow-data" "/app/langflow")
                    ("init.sh" "/app/init.sh")
                    ("custom_components" "/app/custom_components")))
            (container
                (name "postgres")
                (image  "docker.io/library/postgres:17")
                (volumes
                    ("langflow-postgres" "/var/lib/postgresql/data"))
                (environment 
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_USER" "langflow")
                    ("POSTGRES_DB" "langflow"))))))
