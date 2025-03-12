(define-app
    (version "8.0.4")
    (ports 80)
    (let ((db-password ,(gen-password))
          (root-db-password ,(gen-password))
          (public-url ,(interactive-input "Snipe IT URL" "Enter URL where this is going to be deployed? Example: https://assets.example.com")))
        (containers
            (container
                (name "db")
                (image "docker.io/library/mariadb:11.5.2")
                (volumes
                    ("db_data" "/var/lib/mysql"))
                (environment
                    ("MYSQL_DATABASE" "snipeit")
                    ("MYSQL_USER" "snipeit")
                    ("MYSQL_PASSWORD" ,db-password)
                    ("MYSQL_ROOT_PASSWORD" ,root-db-password)))
            (container
                (name "app")
                (image "lscr.io/linuxserver/snipe-it:8.0.4")
                (volumes
                    ("data" "/config"))
                (environment
                    ("PUID" "1000")
                    ("PGID" "1000")
                    ("TZ" "Etc/UTC")
                    ("APP_KEY" "")
                    ("APP_URL" ,public-url)
                    ("MYSQL_PORT_3306_TCP_ADDR" "127.0.0.1")
                    ("MYSQL_PORT_3306_TCP_PORT" "3006")
                    ("MYSQL_DATABASE" "snipeit")
                    ("MYSQL_USER" "snipeit")
                    ("MYSQL_PASSWORD" ,db-password)
                    ("APP_DEBUG" "false")
                    ("APP_ENV" "production")
                    ("APP_FORCE_TLS" "false")
                    ("APP_LOCALE" "")
                    ("MAIL_PORT_587_TCP_ADDR" "")
                    ("MAIL_PORT_587_TCP_PORT" "")
                    ("MAIL_ENV_FROM_ADDR" "")
                    ("MAIL_ENV_FROM_NAME" "")
                    ("MAIL_ENV_ENCRYPTION" "")
                    ("MAIL_ENV_USERNAME" "")
                    ("MAIL_ENV_PASSWORD" ""))))))
