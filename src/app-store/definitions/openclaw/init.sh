#!/bin/bash
# Generate openclaw.json from environment variables on first run.
# Skips if config already exists (preserves user edits on reinstall).

set -e

CONFIG_DIR="/config"
CONFIG_FILE="$CONFIG_DIR/openclaw.json"

if [ -f "$CONFIG_FILE" ]; then
    echo "Config already exists at $CONFIG_FILE — skipping generation."
    exit 0
fi

echo "Generating $CONFIG_FILE from install inputs..."

cat > "$CONFIG_FILE" <<CONF
{
  "gateway": {
    "mode": "local",
    "bind": "lan",
    "controlUi": {
      "dangerouslyAllowHostHeaderOriginFallback": true
    },
    "auth": {
      "mode": "token",
      "token": "${GATEWAY_TOKEN}"
    }
  },
  "env": {
    "GEMINI_API_KEY": "${LLM_API_KEY}"
  },
  "models": {
    "mode": "merge",
    "providers": {
      "google": {
        "baseUrl": "https://generativelanguage.googleapis.com/v1beta",
        "models": [
          {
            "id": "gemini-3-flash-preview",
            "name": "gemini-3-flash-preview",
            "reasoning": true,
            "contextWindow": 1048576,
            "maxTokens": 8192
          }
        ]
      }
    }
  },
  "agents": {
    "defaults": {
      "model": {
        "primary": "google/gemini-3-flash-preview"
      },
      "thinkingDefault": "medium",
      "models": {
        "google/gemini-3-flash-preview": {
          "params": {
            "maxTokens": 16384
          }
        }
      }
    }
  },
  "channels": {
    "mattermost": {
      "enabled": true,
      "botToken": "${MATTERMOST_BOT_TOKEN}",
      "baseUrl": "${MATTERMOST_URL}",
      "dmPolicy": "open",
      "allowFrom": ["*"],
      "chatmode": "onmessage"
    }
  },
  "plugins": {
    "entries": {
      "mattermost": {
        "enabled": true
      }
    }
  }
}
CONF

echo "Config written to $CONFIG_FILE"

# Ensure workspace and state dirs have correct ownership (node = uid 1000)
chown -R 1000:1000 /home/openclaw/workspace 2>/dev/null || true
chown -R 1000:1000 /home/node/.openclaw 2>/dev/null || true
