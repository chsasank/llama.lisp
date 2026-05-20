(define-app
  (version "9.3.7")
  (ports 80)

  (let ((db-password ,(gen-password))
        (db-user "espocrm")
        (db-name "espocrm")
        (admin-password ,(gen-password)))

    (containers

      (container
        (name "espocrm-db")
        (image "mariadb:10.11")
        (volumes
          ("espocrm-db" "/var/lib/mysql"))
        (environment
          ("MARIADB_ROOT_PASSWORD" ,db-password)
          ("MARIADB_DATABASE" ,db-name)
          ("MARIADB_USER" ,db-user)
          ("MARIADB_PASSWORD" ,db-password)))

      (container
        (name "espocrm")
        (image "espocrm/espocrm:9.3.7")
        ; port 80 is exposed at the app level above
        (volumes
          ("espocrm-data" "/var/www/html"))
        (command "sh -c 'sleep 20 && docker-entrypoint.sh apache2-foreground'")
        (environment
          ("ESPOCRM_DATABASE_PLATFORM" "Mysql")
          ("ESPOCRM_DATABASE_HOST" "127.0.0.1")
          ("ESPOCRM_DATABASE_NAME" ,db-name)
          ("ESPOCRM_DATABASE_USER" ,db-user)
          ("ESPOCRM_DATABASE_PASSWORD" ,db-password)
          ("ESPOCRM_ADMIN_USERNAME" "admin")
          ("ESPOCRM_ADMIN_PASSWORD" ,admin-password)))

      (container
        (name "init")
        (image "espocrm/espocrm:9.3.7")
        (command "sh /app/init.sh")
        (volumes
          ("espocrm-data" "/var/www/html")
          ("init.sh" "/app/init.sh"))
        (environment
          ("ESPOCRM_DATABASE_HOST" "127.0.0.1")
          ("ESPOCRM_DATABASE_NAME" ,db-name)
          ("ESPOCRM_DATABASE_USER" ,db-user)
          ("ESPOCRM_DATABASE_PASSWORD" ,db-password)
          ("ESPOCRM_ADMIN_USERNAME" "admin")
          ("ESPOCRM_ADMIN_PASSWORD" ,admin-password)))

      (container
        (name "volume")
        (image "ubuntu:22.04")
        (command "sh /mnt/volumes.sh")
        (volumes
          ("espocrm-data" "/var/www/html")
          ("volumes.sh" "/mnt/volumes.sh")))

    )
  )
)

