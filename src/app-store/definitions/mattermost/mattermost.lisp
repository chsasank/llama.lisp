(define-app
    (version "2.4")
    (ports 8065)
    (let
        ((db-password ,(gen-password)))
        (containers
            (container 
                (name "db") 
                (image "docker.io/postgres:13-alpine")
                (volumes
                    ("psql_data" "/var/lib/postgresql/data"))
                (environment
                    ("TZ" "UTC")
                    ("POSTGRES_USER" "mmuser")
                    ("POSTGRES_DB" "mattermost")
                    ("POSTGRES_PASSWORD" ,db-password)))
            (container
                (name "mattermost")
                (image "docker.io/mattermost/mattermost-team-edition:10.11.9")
                (volumes
                    ("mattermost/config" "/mattermost/config") 
                    ("mattermost/data" "/mattermost/data")
                    ("mattermost/logs" "/mattermost/logs")
                    ("mattermost/plugins" "/mattermost/plugins")
                    ("mattermost/client/plugins" "/mattermost/client/plugins")
                    ("mattermost/bleve-indexes" "/mattermost/bleve-indexes"))
                (environment
                    ("DOMAIN" "mm.example.com")
                    ("TZ" "UTC")
                    ("MM_SQLSETTINGS_DRIVERNAME" "postgres")
                    ("MM_SQLSETTINGS_DATASOURCE" 
                        ,(format "postgres://mmuser:{}@localhost:5432/mattermost?sslmode=disable&connect_timeout=10" ,db-password))
                    ("MM_BLEVESETTINGS_INDEXDIR" "/mattermost/bleve-indexes")
                    ("MATTERMOST_CONTAINER_READONLY" "false")))
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init_dirs.sh")
                (volumes
                    ("mattermost/config" "/mattermost/config") 
                    ("mattermost/data" "/mattermost/data")
                    ("mattermost/logs" "/mattermost/logs")
                    ("mattermost/plugins" "/mattermost/plugins")
                    ("mattermost/client/plugins" "/mattermost/client/plugins")
                    ("mattermost/bleve-indexes" "/mattermost/bleve-indexes")
                    ("init_dirs.sh" "/mnt/init_dirs.sh"))))))

