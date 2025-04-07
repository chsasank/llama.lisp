(define-app
    (version "2.1")
    (ports 8080 5432)
    (let ((db-password ,(gen-password)))
        (containers 
            (container
                (name "nocodb")
                (image "docker.io/nocodb/nocodb:latest")
                (volumes 
                    ("nc_data" "/usr/app/data"))
                (environment 
                    ("NC_DB" ,(format "pg://localhost:5432?u=postgres&p={}&d=root_db"
                                ,db-password))))
            (container
                (name "root_db")
                (image "docker.io/postgres")
                (volumes
                    ("db_data" "/var/lib/postgresql/data")) 
                (environment    
                    ("POSTGRES_USER" "postgres")
                    ("POSTGRES_DB" "root_db")
                    ("POSTGRES_PASSWORD" ,db-password))))))
