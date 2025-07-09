(define-app 
    (version "4.5")
    (ports 8080                         )
    (let 
        ((db-password ,(gen-password)))
        (containers 
            (container 
                (name "mariadb") 
                (image "docker.io/bitnami/mariadb:11.4") 
                (volumes 
                    ("mariadb_data" "/bitnami/mariadb"))
                (environment
                    ("MARIADB_ROOT_PASSWORD" ,(gen-password))
                    ("MARIADB_PASSWORD" ,db-password)
                    ("MARIADB_USER" "bn_moodle")
                    ("MARIADB_DATABASE" "bitnami_moodle")
                    ("MARIADB_CHARACTER_SET" "utf8mb4")
                    ("MARIADB_COLLATE" "utf8mb4_unicode_ci")))
            (container 
                (name "moodle")
                (image "docker.io/bitnami/moodle:4.5") 
                (volumes 
                    ("moodle_data" "/bitnami/moodle")
                    ("moodledata_data" "/bitnami/moodledata")) 
                (environment
                    ("MOODLE_DATABASE_HOST" "127.0.0.1")
                    ("MOODLE_DATABASE_PORT_NUMBER" "3306")
                    ("MOODLE_DATABASE_USER" "bn_moodle")
                    ("MOODLE_DATABASE_NAME" "bitnami_moodle")
                    ("MOODLE_DATABASE_PASSWORD" ,db-password)
                    ("MOODLE_USERNAME" "admin")
                    ("MOODLE_PASSWORD" ,(gen-password)))))))