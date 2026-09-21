(define-app
    ; name of the app is taken from file name
    (version "2.25.0")
    (ports 5173)
    (url "https://kaneo.app")
    (let ((db-name "kaneo")
          (db-user "kaneo")
          (db-password ,(gen-password))
          (kaneo-url ,(interactive-input "Kaneo URL" "Enter URL where this is going to be deployed. Example: https://kaneo.example.com"))
          (mattermost-server-name ,(interactive-input "Mattermost URL" "Enter your exposed mattermost domain address. We need it for authentication. Example: https://mattermost.example.com")))
        (containers
            (container
                (name "kaneo")
                (image "ghcr.io/usekaneo/kaneo:2.25.0")
                (environment
                    ("KANEO_CLIENT_URL" ,kaneo-url)
                    ("DATABASE_URL" ,(format "postgresql://{}:{}@localhost:5432/{}" ,db-user ,db-password ,db-name))
                    ("AUTH_SECRET" ,(gen-password-hex32))
                    ; disable local sign-in/sign-up, oidc only
                    ("DISABLE_LOGIN_FORM" "true")
                    ("DISABLE_REGISTRATION" "true")
                    ("DISABLE_PASSWORD_REGISTRATION" "true")
                    ; oidc via mattermost
                    ("CUSTOM_OAUTH_CLIENT_ID" ,(interactive-input "OIDC Client ID" ,(format "please enter OIDC_CLIENT_ID from mattermost. Register this redirect/callback URL in the mattermost OAuth app: {}/api/auth/oauth2/callback/custom . For tutorial on how to set the OIDC client_ID using mattermost, look at this guide: https://outline.von-neumann.ai/s/f13351f6-801c-4401-ac1b-839e51dd4aa4" ,kaneo-url)))
                    ("CUSTOM_OAUTH_CLIENT_SECRET" ,(interactive-input "OIDC Secret"))
                    ("CUSTOM_OAUTH_AUTHORIZATION_URL" ,(format "{}/oauth/authorize" ,mattermost-server-name))
                    ("CUSTOM_OAUTH_TOKEN_URL" ,(format "{}/oauth/access_token" ,mattermost-server-name))
                    ("CUSTOM_OAUTH_USER_INFO_URL" ,(format "{}/api/v4/users/me" ,mattermost-server-name))))
            (container
                (name "postgres")
                (image "docker.io/library/postgres:16-alpine")
                (volumes
                    ("postgres-data" "/var/lib/postgresql/data"))
                (environment
                    ("POSTGRES_DB" ,db-name)
                    ("POSTGRES_USER" ,db-user)
                    ("POSTGRES_PASSWORD" ,db-password))))))
