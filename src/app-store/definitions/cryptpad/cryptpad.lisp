(define-app
    (version "2026.5.0")
    (ports 80)
    (let ((cryptpad-image "docker.io/cryptpad/cryptpad:latest")
          (onlyoffice-image "docker.io/onlyoffice/documentserver:latest")
          (nginx-image "docker.io/library/nginx:latest")
          (jwt-secret ,(gen-password))
          (main-domain ,(interactive-input "Main Domain " "Enter your server IP. Example: 100.64.1.1"))
          (admin-email ,(interactive-input "Admin Email " "Enter the admin email. Example: admin@gmail.com"))
          (confirm-post-install
                ,(interactive-input
                    "NOTE: After installation, manually run:\n bash ~/.johnny/cryptpad/post-install.sh \n to finish setting up CryptPad and get the admin URL.\n\nContinue? (y) "
                    )))
        (containers
            (container
                (name "cryptpad")
                (image ,cryptpad-image)
                (command "node server.js")
                (environment
                    ("CPAD_MAIN_DOMAIN" ,(format "http://{}" ,main-domain))
		            ("CPAD_SANDBOX_DOMAIN" ,(format "http://{}" ,main-domain))
                    ("CPAD_CONF" "/cryptpad/config/config.js")
                    ("CPAD_CONFIG" "/cryptpad/config/config.js")
                    ("CPAD_INSTALL_ONLYOFFICE" "yes")
                    ("CPAD_ADMIN_EMAIL" ,admin-email)
		            ("CPAD_OFFICE_URL" ,(format "http://{}/onlyoffice/" ,main-domain))
                    ("CPAD_OFFICE_SECRET" ,jwt-secret))
                (volumes
                    ("blob" "/cryptpad/blob")
                    ("block" "/cryptpad/block")
                    ("data" "/cryptpad/data")
                    ("files" "/cryptpad/datastore")
                    ("customize" "/cryptpad/customize")
                    ("config.js" "/cryptpad/config/config.js")
                    ("onlyoffice-dist" "/cryptpad/www/common/onlyoffice/dist")
                    ("onlyoffice-conf" "/cryptpad/onlyoffice-conf")))

            (container
                (name "onlyoffice")
                (image ,onlyoffice-image)
                (environment
                    ("JWT_ENABLED" "true")
                    ("JWT_SECRET" ,jwt-secret)))

            (container
                (name "cryptpad-nginx")
                (image ,nginx-image)
                (volumes
                    ("nginx.conf" "/etc/nginx/conf.d/default.conf")
                    ("ssl" "/etc/ssl"))
                (depends-on "cryptpad"))

            (container
                (name "volume")
                (image "docker.io/library/ubuntu:22.04")
                (command "sh /mnt/init.sh")
                (volumes
                    ("blob" "/cryptpad/blob")
                    ("block" "/cryptpad/block")
                    ("data" "/cryptpad/data")
                    ("files" "/cryptpad/datastore")
                    ("customize" "/cryptpad/customize")
                    ("onlyoffice-dist" "/cryptpad/www/common/onlyoffice/dist")
                    ("onlyoffice-conf" "/cryptpad/onlyoffice-conf")
                    ("init.sh" "/mnt/init.sh")
                    ("post-install.sh" "/mnt/post-install.sh"))))))