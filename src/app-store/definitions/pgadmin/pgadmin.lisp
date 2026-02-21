(define-app
    (version "9.3.0")
    (ports 5432 80)
    (let ((postgres_password ,(gen-password))
          (pgadmin_default_password ,(gen-password))) 
        (containers
            (container
                (name "db")
                (image "docker.io/postgres:17.5-bookworm")
                (environment
                    ("POSTGRES_USER" "pgadmin")
                    ("POSTGRES_PASSWORD" ,postgres_password)
		            ("POSTGRES_DB" "postgres"))
                (volumes
                    ("local_pgdata" "/var/lib/postgresql/data")))
            (container
                (name "pgadmin")
                (image "dpage/pgadmin4:9.3.0") 
                (environment
                    ("PGADMIN_DEFAULT_EMAIL" "admin@johnaic.com")
                    ("PGADMIN_DEFAULT_PASSWORD" ,pgadmin_default_password)
                    ("PGADMIN_SERVER_JSON_FILE" "/run/pgadmin/servers.json")
                    ("POSTGRES_PASSWORD" ,postgres_password))
                (entrypoint "/bin/bash")
                (command "/mnt/start.sh")
                (volumes
                    ("start.sh" "/mnt/start.sh")
                    ("pgadmin-data"  "/var/lib/pgadmin"))))))
