(define-app
    (version "15.57.5")
    (ports 8080)
    (let ((frappe-image-name "docker.io/johnaic/erpnext:15")
          (admin-password ,(gen-password))
          (db-password ,(gen-password)))
        (containers
            (container
                (name "backend")
                (image ,frappe-image-name)
                (volumes
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs"))
                (environment
                    ("DB_HOST" "localhost")
                    ("DB_PORT" "3306")
                    ("MYSQL_ROOT_PASSWORD" ,db-password)
                    ("MARIADB_ROOT_PASSWORD" ,db-password)))
            (container
                (name "configurator")
                (image ,frappe-image-name)
                (volumes
                    ("configurator.sh" "/mnt/configurator.sh")
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs"))
                (entrypoint "bash")
                (command "/mnt/configurator.sh")
                (environment
                    ("DB_HOST" "localhost")
                    ("DB_PORT" "3306")
                    ("REDIS_CACHE" "localhost:6379")
                    ("REDIS_QUEUE" "localhost:6380")
                    ("SOCKETIO_PORT" "9000")))
            (container
                (name "create-site")
                (image ,frappe-image-name)
                (volumes
                    ("create-site.sh" "/mnt/create-site.sh")
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs"))
                (entrypoint "bash")
                (command "/mnt/create-site.sh")
                (environment
                    ("DB_HOST" "localhost")
                    ("DB_PORT" "3306")
                    ("REDIS_CACHE" "localhost:6379")
                    ("REDIS_QUEUE" "localhost:6380")
                    ("ADMIN_PASSWORD" ,admin-password)
                    ("DB_PASSWORD" ,db-password)))
            (container
                (name "db")
                (image "docker.io/library/mariadb:10.6")
                (volumes
                    ("db-data" "/var/lib/mysql"))
                (environment
                    ("MYSQL_ROOT_PASSWORD" ,db-password)
                    ("MARIADB_ROOT_PASSWORD" ,db-password))
                (command "--character-set-server=utf8mb4 --collation-server=utf8mb4_unicode_ci --skip-character-set-client-handshake --skip-innodb-read-only-compressed"))
            (container
                (name "frontend")
                (image ,frappe-image-name)
                (volumes
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs"))
                (command "nginx-entrypoint.sh")
                (environment
                    ("BACKEND" "localhost:8000")
                    ("FRAPPE_SITE_NAME_HEADER" "frontend")
                    ("SOCKETIO" "localhost:9000")
                    ("UPSTREAM_REAL_IP_ADDRESS" "127.0.0.1")
                    ("UPSTREAM_REAL_IP_HEADER" "X-Forwarded-For")
                    ("UPSTREAM_REAL_IP_RECURSIVE" "off")
                    ("PROXY_READ_TIMEOUT" "120")
                    ("CLIENT_MAX_BODY_SIZE" "50m")))
            (container
                (name "queue-long")
                (image ,frappe-image-name)
                (volumes
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs"))
                (command "bench worker --queue long,default,short"))
            (container
                (name "queue-short")
                (image ,frappe-image-name)
                (volumes
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs"))
                (command "bench worker --queue short,default"))
            (container
                (name "redis-cache")
                (image "docker.io/redis:6.2-alpine")
                (volumes
                    ("redis-cache-data" "/data")))
            (container
                (name "redis-queue")
                (image "docker.io/redis:6.2-alpine")
                (command "redis-server --port 6380")
                (volumes
                    ("redis-queue-data" "/data")))
            (container
                (name "scheduler")
                (image ,frappe-image-name)
                (command "bench schedule")
                (volumes
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs")))
            (container
                (name "websocket")
                (image ,frappe-image-name)
                (command "node /home/frappe/frappe-bench/apps/frappe/socketio.js")
                (volumes
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs")))
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_volumes.sh")
                (volumes
                    ("sites" "/home/frappe/frappe-bench/sites")
                    ("logs" "/home/frappe/frappe-bench/logs")
                    ("init_volumes.sh" "/mnt/init_volumes.sh"))))))
