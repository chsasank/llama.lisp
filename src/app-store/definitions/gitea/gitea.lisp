(define-app
    ; name of the app is taken from file name
    (version "1.21.4")
    (ports 3000 22)
    (let ((db-name "gitea")
          (db-user "gitea")
          (db-password ,(gen-password)))
        (containers
            (container
                (name "gitea")
                (image "docker.io/gitea/gitea:1.21.4")
                (volumes 
                    ("gitea-data" "/data"))
                (environment
                    ("GITEA__database__DB_TYPE" "postgres")
                    ("GITEA__database__HOST" "localhost:5432")
                    ("GITEA__database__NAME" ,db-name)
                    ("GITEA__database__USER" ,db-user)
                    ("GITEA__database__PASSWD" ,db-password)))
            (container
                (name "db")
                (image "docker.io/postgres:14")
                (volumes
                    ("postgres-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_USER" ,db-user)
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_DB" ,db-name))))))
