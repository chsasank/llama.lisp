(define-app
    (version "latest")
    (ports 3000)

    (let (
            (domain ,(interactive-input
                "Domain: "
                "Enter your CryptPad domain. Example: cryptpad.example.com"))

            (jwt-secret ,(gen-password))
          )

        (containers

            (container
                (name "cryptpad")

                (image "docker.io/cryptpad/cryptpad:latest")

                (command "node server.js")

                (environment
                    ("CPAD_MAIN_DOMAIN"
                        ,(format nil "https://~A" domain))

                    ("CPAD_CONFIG"
                        "/cryptpad/config/config.js")

                    ("CPAD_INSTALL_ONLYOFFICE"
                        "true"))

                (volumes
                    ("blob-data"
                        "/cryptpad/blob")

                    ("block-data"
                        "/cryptpad/block")

                    ("cryptpad-data"
                        "/cryptpad/data")

                    ("datastore-data"
                        "/cryptpad/datastore")

                    ("customize"
                        "/cryptpad/customize")

                    ("config.js"
                        "/cryptpad/config/config.js")

                    ("onlyoffice-dist"
                        "/cryptpad/www/common/onlyoffice/dist")))

            (container
                (name "onlyoffice")

                (image "docker.io/onlyoffice/documentserver:latest")

                (environment
                    ("JWT_ENABLED" "true")
                    ("JWT_SECRET" ,jwt-secret))))))
