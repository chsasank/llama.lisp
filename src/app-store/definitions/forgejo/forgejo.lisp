(define-app
    (version "15")
    (ports 3000 22)
    (url "https://forgejo.org/docs/latest/admin/installation/docker/")
    (let ((db-password ,(gen-password))
          (db-name "forgejo")
          (db-user "forgejo"))
        (containers
            (container
                (name "forgejo")
                (image "codeberg.org/forgejo/forgejo:15")
                (volumes
                    ("forgejo-data" "/data")
                    ("/etc/localtime" "/etc/localtime"))
                (environment
                    ("USER_UID" "1000")
                    ("USER_GID" "1000")
                    ("FORGEJO__database__DB_TYPE" "postgres")
                    ("FORGEJO__database__HOST" "localhost:5432")
                    ("FORGEJO__database__NAME" ,db-name)
                    ("FORGEJO__database__USER" ,db-user)
                    ("FORGEJO__database__PASSWD" ,db-password)))
            (container
                (name "postgres")
                (image "docker.io/library/postgres:17")
                (volumes
                    ("postgres-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_USER" ,db-user)
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_DB" ,db-name))))))
