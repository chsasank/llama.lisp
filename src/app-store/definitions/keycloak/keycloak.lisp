(define-app
    (version "21.0")
    (ports 8443)
    (let ((db-password ,(gen-password))
          (keyclock-password ,(gen-password)))
        (containers
            (container
                (name "postgres")
                (image "docker.io/library/postgres:13.7")
                (volumes
                    ("psql_data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_DB" "keycloak")
                    ("POSTGRES_USER" "keycloak")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container
                (name "keyclock")
                (build "localhost/johnaic/keycloak:21.0")
                (environment
                    ("KC_HOSTNAME_STRICT" "false")
                    ("KC_HOSTNAME_STRICT_HTTPS" "false")
                    ("KEYCLOAK_ADMIN" "admin")
                    ("KEYCLOAK_ADMIN_PASSWORD" ,keyclock-password)
                    ("KEYCLOAK_USER" "admin")
                    ("KEYCLOAK_PASSWORD" ,keyclock-password)
                    ("KC_DB_USERNAME" "keycloak")
                    ("KC_DB_PASSWORD" ,db-password)
                    ("KC_HEALTH_ENABLED" "true")
                    ("KC_METRICS_ENABLED" "true")
                    ("KC_HOSTNAME" "localhost" ))
                (command "start --optimized")))))

