(define-app
  (version "3.7")
  (ports 8000 9000)
  (containers
    (container
      (name "mariadb")
      (image "mariadb:10.8")
      (command 
          "--character-set-server=utf8mb4"
          "--collation-server=utf8mb4_unicode_ci"
          "--skip-character-set-client-handshake"
          "--skip-innodb-read-only-compressed")
      (environment
        ("MYSQL_ROOT_PASSWORD" "123"))
      (volumes
        ("mariadb-data" "/var/lib/mysql")))

    (container
      (name "redis")
      (image "redis:alpine"))

    (container
      (name "frappe")
      (image "frappe/bench:latest")
      (command "bash /workspace/init.sh")
      (environment
        ("SHELL" "/bin/bash"))
      (volumes
        ("init.sh" "/workspace/init.sh")))))
