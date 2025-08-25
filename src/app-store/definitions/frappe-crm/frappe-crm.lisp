(define-app
    (version "15")
    (ports 8080)
    (let ((frappe-image-name "ghcr.io/frappe/crm:v15")
          (admin-password ,(gen-password))
          (db-password ,(gen-password))
          (sql-password ,(gen-password))
          (db-user "frappe_crm")
          (mysql-database "crm_database")
          (sitename ,(interactive-input "Sitename: " "Enter the sitename. Example: crm.johnaic.com")))
        (containers
            (container
                (name "frontend")
                (image ,frappe-image-name)
                (volumes
                    ("sites-data" "/home/frappe/frappe-bench/sites"))
                (command "nginx-entrypoint.sh")
                (environment
                    ("BACKEND" "localhost:8000")
                    ("FRAPPE_SITE_NAME_HEADER" ,sitename)
                    ("SOCKETIO_PORT" "9000")))
            
            (container
                (name "backend")
                (image ,frappe-image-name)
                (volumes
                    ("sites-data" "/home/frappe/frappe-bench/sites"))
                (environment
                    ("SITENAME" ,sitename)
                    ("DB_HOST" "localhost")
                    ("DB_PORT" "3306")
                    ("DB_USER" ,db-user)
                    ("DB_PASSWORD" ,sql-password)
                    ("MYSQL_ROOT_PASSWORD" ,db-password)
                    ("ADMIN_PASSWORD" ,admin-password)
                    ("REDIS_CACHE" "redis://localhost:6379")
                    ("REDIS_QUEUE" "redis://localhost:6380")))
            
            (container
                (name "mariadb")
                (image "mariadb:10.6")
                (volumes
                    ("db-data" "/var/lib/mysql"))
                (environment
                    ("MYSQL_ROOT_PASSWORD" ,db-password)
                    ("MYSQL_DATABASE" ,mysql-database)
                    ("MYSQL_USER" ,db-user)
                    ("MYSQL_PASSWORD" ,sql-password)
                    ("ADMIN_PASSWORD" ,admin-password)))

            (container
                (name "redis-cache")
                (image "redis:6.2-alpine"))
            
            (container
                (name "redis-queue")
                (image "docker.io/redis:6.2-alpine")
                (command "redis-server --port 6380"))

            (container
                (name "scheduler")
                (image ,frappe-image-name)
                (command "bench schedule")
                (volumes
                    ("sites-data" "/home/frappe/frappe-bench/sites"))
                (environment
                    ("SITENAME" ,sitename)
                    ("DB_HOST" "localhost")
                    ("DB_PORT" "3306")
                    ("DB_USER" ,db-user)
                    ("DB_PASSWORD" ,sql-password)
                    ("REDIS_CACHE" "redis://localhost:6379")
                    ("REDIS_QUEUE" "redis://localhost:6380")))
            
            (container
                (name "worker")
                (image ,frappe-image-name)
                (command "bench worker --queue short,long,default")
                (volumes
                    ("sites-data" "/home/frappe/frappe-bench/sites"))
                (environment
                    ("SITENAME" ,sitename)
                    ("DB_HOST" "localhost")
                    ("DB_PORT" "3306")
                    ("DB_USER" ,db-user)
                    ("DB_PASSWORD" ,sql-password)
                    ("REDIS_CACHE" "redis://localhost:6379")
                    ("REDIS_QUEUE" "redis://localhost:6380")))

            (container
                (name "init")
                (image ,frappe-image-name)
                (command "bash /app/init.sh")
                (volumes
                    ("sites-data" "/home/frappe/frappe-bench/sites")
                    ("init.sh" "/app/init.sh"))
                (environment
                    ("SITENAME" ,sitename)
                    ("MYSQL_ROOT_PASSWORD" ,db-password)
                    ("ADMIN_PASSWORD" ,admin-password)))
                    
            (container
                (name "volume")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/volumes.sh")
                (volumes
                    ("sites-data" "/home/frappe/frappe-bench/sites")
                    ("volumes.sh" "/mnt/volumes.sh"))))))
