(define-app
    (version "5.0.1")
    (ports 80)
    (let ((cms_image "docker.io/johnaic/mediacms:4.6.0")
          (db-password ,(gen-password))
          (secret-key ,(gen-password-hex32)))
        (containers
            (container
                (name "migrations")
                (image ,cms_image)
                (volumes
                    ("local_settings.py" "/home/mediacms.io/mediacms/deploy/docker/local_settings.py")
                    ("media_files" "/home/mediacms.io/mediacms/media_files")
                    ("migrate.sh" "/mnt/migrate.sh"))
                (command "bash /mnt/migrate.sh")
                (environment
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("SECRET_KEY" ,secret-key)
                    ("ENABLE_UWSGI" "no")
                    ("ENABLE_NGINX" "no")
                    ("ENABLE_CELERY_SHORT" "no")
                    ("ENABLE_CELERY_LONG" "no")
                    ("ENABLE_CELERY_BEAT" "no")
                    ("ADMIN_USER" "admin")
                    ("ADMIN_EMAIL" "admin@localhost")
                    ("ADMIN_PASSWORD" ,(gen-password))))
            (container
                (name "web")
                (image ,cms_image)
                (volumes
                    ("local_settings.py" "/home/mediacms.io/mediacms/deploy/docker/local_settings.py")
                    ("media_files" "/home/mediacms.io/mediacms/media_files"))
                (environment
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("SECRET_KEY" ,secret-key)
                    ("ENABLE_CELERY_SHORT" "no")
                    ("ENABLE_CELERY_LONG" "no")
                    ("ENABLE_CELERY_BEAT" "no")
                    ("ENABLE_MIGRATIONS" "no")))
            (container
                (name "celery_beat")
                (image ,cms_image)
                (volumes
                    ("local_settings.py" "/home/mediacms.io/mediacms/deploy/docker/local_settings.py")
                    ("media_files" "/home/mediacms.io/mediacms/media_files"))
                (environment
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("SECRET_KEY" ,secret-key)
                    ("ENABLE_UWSGI" "no")
                    ("ENABLE_NGINX" "no")
                    ("ENABLE_CELERY_SHORT" "no")
                    ("ENABLE_CELERY_LONG" "no")
                    ("ENABLE_MIGRATIONS" "no")))
            (container
                (name "celery_worker")
                (image ,cms_image)
                (volumes
                    ("local_settings.py" "/home/mediacms.io/mediacms/deploy/docker/local_settings.py")
                    ("media_files" "/home/mediacms.io/mediacms/media_files"))
                (environment
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("SECRET_KEY" ,secret-key)
                    ("ENABLE_UWSGI" "no")
                    ("ENABLE_NGINX" "no")
                    ("ENABLE_CELERY_BEAT" "no")
                    ("ENABLE_MIGRATIONS" "no")))
            (container
                (name "db")
                (image "docker.io/library/postgres:17.2-alpine")
                (volumes
                    ("postgres_data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_USER" "mediacms")
                    ("POSTGRES_PASSWORD" ,db-password)
                    ("POSTGRES_DB" "mediacms")))
            (container
                (name "redis")
                (image "docker.io/library/redis:alpine")))))
