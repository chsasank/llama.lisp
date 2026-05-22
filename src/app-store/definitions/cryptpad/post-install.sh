#!/bin/bash
set -e

echo "==> CryptPad Post-Install Setup"

ENV_FILE="$HOME/.johnny/cryptpad/cryptpad.env"
APP_DB="$HOME/.johnny/app_db.json"

# Step 1 - Get host port from app_db.json (the actual external port)
PORT=$(python3 -c "
import json
with open('$APP_DB') as f:
    db = json.load(f)
for entry in db['_default'].values():
    if entry['name'] == 'cryptpad':
        print(entry['ports']['80'])
        break
")
echo "==> Detected host port: $PORT"

# Validate port was found
if [ -z "$PORT" ]; then
    echo "ERROR: Could not detect port from app_db.json!"
    exit 1
fi

# Step 2 - Get domain/IP user gave during install (strip any existing port)
DOMAIN=$(grep "CPAD_MAIN_DOMAIN" $ENV_FILE | cut -d= -f2 | sed 's|http://||' | cut -d: -f1 | sed 's|^|http://|')
echo "==> Domain from install: $DOMAIN"

# Step 3 - Update cryptpad.env with correct domain:port
sed -i "s|CPAD_MAIN_DOMAIN=.*|CPAD_MAIN_DOMAIN=$DOMAIN:$PORT|g" $ENV_FILE
sed -i "s|CPAD_SANDBOX_DOMAIN=.*|CPAD_SANDBOX_DOMAIN=$DOMAIN:$PORT|g" $ENV_FILE
sed -i "s|CPAD_OFFICE_URL=.*|CPAD_OFFICE_URL=$DOMAIN:$PORT/onlyoffice/|g" $ENV_FILE
echo "==> Updated cryptpad.env ✅"
echo ""
echo "==> Current env:"
cat $ENV_FILE
echo ""

# Step 4 - Restart cryptpad
echo "==> Restarting CryptPad..."
systemctl --user restart cryptpad-cryptpad.service
echo "==> Restarted ✅"

# Step 5 - Wait for startup
echo "==> Waiting 20 seconds for CryptPad to start..."
sleep 20

# Step 6 - Show URLs
HASH=$(journalctl --user -u cryptpad-cryptpad.service --no-pager | grep "install/#" | tail -1 | awk -F'install/#' '{print $2}')
ADMIN_URL="$DOMAIN:$PORT/install/#$HASH"
echo ""
echo "==========================================="
echo "==> CryptPad is ready!"
echo "==> Access at : $DOMAIN:$PORT"
echo "==> Admin URL : $ADMIN_URL"
echo "==========================================="
