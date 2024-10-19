(define-app
    (version "1.116.0")
    (ports 8008)
    (url "https://github.com/open-webui/open-webui/")
    (let ((server-name ,(interactive-input "server name" "It is important to choose the name for your server before you install Synapse, because it cannot be changed later.\nThe server name determines the \"domain\" part of user-ids for users on your server: these will all be of the format @user:my.domain.name. It also determines how other matrix servers will reach yours for federation."))
          (db-password ,(gen-password)))
        (containers
            (container
                (name "synapse")
                (image "docker.io/matrixdotorg/synapse:v1.116.0")
                (entrypoint "bash")
                (command "/start.sh")
                (volumes
                    ("synapse-data" "/data")
                    ("start.sh" "/start.sh"))
                (environment
                    ("SYNAPSE_SERVER_NAME" ,server-name)
                    ("SYNAPSE_REPORT_STATS" "no")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container
                (name "postgres")
                (image "docker.io/bitnami/postgresql:17")
                (volumes
                    ("postgres-data" "/bitnami/postgresql"))
                (environment
                    ("POSTGRESQL_USERNAME" "synapse")
                    ("POSTGRESQL_DATABASE" "synapse")
                    ("POSTGRESQL_INITDB_ARGS" "--encoding=UTF-8 --lc-collate=C --lc-ctype=C")
                    ("POSTGRESQL_PASSWORD" ,db-password))))))
