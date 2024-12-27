(define-app
    (version "3.3")
    (ports 3000 3001)
    (let ((postgres_password ,(gen-password))
           (encryption_secret "ltFmwyXgwtJL9hpkCnNFCQPJil4TEvOE"))
        (containers
            (container 
                (name "typebot-db")
                (image "postgres:16")
                (volumes 
                    ("db-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_DB" "typebot")
                    ("POSTGRES_PASSWORD" ,postgres_password)))
            (container 
                (name "typebot-builder")
                (image "baptistearno/typebot-builder:3.2")
                (environment
                    ("DATABASE_URL" ,(format "postgresql://postgres:{}@localhost:5432/typebot" ,postgres_password))
                    ("ENCRYPTION_SECRET" ,encryption_secret)
                    ("NEXTAUTH_URL" "http://localhost:3000")
                    ("NEXT_PUBLIC_VIEWER_URL" "http://localhost:3001")
                    ("ADMIN_EMAIL" "adithya@von-neumann.ai")
                    ("GITHUB_CLIENT_ID" Ov23li8KcJjvL7T6MMiO)
                    ("GITHUB_CLIENT_SECRET" 151223278974fbe515da04fbcda1f3740c28f53f)
                    ("NEXT_PUBLIC_PARTYKIT_HOST" "localhost:1999")))
            (container 
                (name "typebot-viewer")
                (image "baptistearno/typebot-viewer:3.2.0")
                (entrypoint "/bin/bash")
                (command "/app/init.sh")
                (volumes
                    ("init.sh" "/app/init.sh"))
                (environment
                    ("DATABASE_URL" ,(format "postgresql://postgres:{}@localhost:5432/typebot" ,postgres_password))
                    ("ENCRYPTION_SECRET" ,encryption_secret)
                    ("NEXTAUTH_URL" "https://localhost:3000")
                    ("NEXT_PUBLIC_VIEWER_URL" "https://localhost:3001")
                    ("GITHUB_CLIENT_ID" Ov23li8KcJjvL7T6MMiO)
                    ("GITHUB_CLIENT_SECRET" 151223278974fbe515da04fbcda1f3740c28f53f)
                    ("ADMIN_EMAIL" "adithya@von-neumann.ai")
                    ("NEXT_PUBLIC_PARTYKIT_HOST" "localhost:1999"))))))  

;(volumes 
;                    ("init.sh" "app/init.sh"))
;                (entrypoint "/bin/bash")
;                (command "app/init.sh")