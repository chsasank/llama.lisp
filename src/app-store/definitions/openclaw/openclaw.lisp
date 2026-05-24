; OpenClaw — AI assistant bot for Mattermost
; Connects to an existing Mattermost instance via websocket.
; The gateway control UI is exposed on port 18789.

(define-app
    (version "2026.3.24")
    (ports 18789)
    (url "https://github.com/openclaw/openclaw")
    (let ((gateway-token ,(gen-password))
          (mattermost-url ,(interactive-input "Mattermost URL"
              "Base URL of your Mattermost server. Example: https://mattermost.example.com"))
          (mattermost-bot-token ,(interactive-input "Mattermost bot token"
              "Create a bot account in Mattermost (Integrations > Bot Accounts) and paste the token here."))
          (llm-api-key ,(interactive-input "LLM API key"
              "API key for the LLM provider (Gemini by default). Example: your Gemini API key from https://aistudio.google.com/apikey")))
        (containers
            (container
                (name "init")
                (image "docker.io/library/ubuntu:22.04")
                (command "bash /mnt/init.sh")
                (volumes
                    ("config" "/config")
                    ("workspace" "/home/openclaw/workspace")
                    ("state" "/home/node/.openclaw")
                    ("init.sh" "/mnt/init.sh"))
                (environment
                    ("MATTERMOST_URL" ,mattermost-url)
                    ("MATTERMOST_BOT_TOKEN" ,mattermost-bot-token)
                    ("LLM_API_KEY" ,llm-api-key)
                    ("GATEWAY_TOKEN" ,gateway-token)))
            (container
                (name "openclaw")
                (build "localhost/johnaic/openclaw:2026.3.24")
                (volumes
                    ("config" "/config")
                    ("workspace" "/home/openclaw/workspace")
                    ("state" "/home/node/.openclaw"))
                (environment
                    ("OPENCLAW_CONFIG_PATH" "/config/openclaw.json")
                    ("OPENCLAW_GATEWAY_TOKEN" ,gateway-token))))))
