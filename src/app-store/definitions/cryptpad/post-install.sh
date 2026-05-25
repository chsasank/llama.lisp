#!/bin/bash
set -e

echo "==> CryptPad Post-Install Setup"

ENV_FILE="$HOME/.johnny/cryptpad/cryptpad.env"
APP_DB="$HOME/.johnny/app_db.json"

#host port
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

#Get domain/IP user gave during install (strip any existing port)
DOMAIN=$(grep "CPAD_MAIN_DOMAIN" $ENV_FILE | cut -d= -f2 | sed 's|http://||' | cut -d: -f1 | sed 's|^|http://|')
echo "==> Domain from install: $DOMAIN"

#Update cryptpad.env with correct domain:port
sed -i "s|CPAD_MAIN_DOMAIN=.*|CPAD_MAIN_DOMAIN=$DOMAIN:$PORT|g" $ENV_FILE
sed -i "s|CPAD_SANDBOX_DOMAIN=.*|CPAD_SANDBOX_DOMAIN=$DOMAIN:$PORT|g" $ENV_FILE
sed -i "s|CPAD_OFFICE_URL=.*|CPAD_OFFICE_URL=$DOMAIN:$PORT/onlyoffice/|g" $ENV_FILE
echo "==> Updated cryptpad.env "
echo ""
echo "==> Current env:"
cat $ENV_FILE


# Restart cryptpad
echo "==> Restarting CryptPad..."
systemctl --user restart cryptpad-cryptpad.service
echo "==> Restarted"


#Show URLs
echo ""
echo "==========================================="
echo "==> CryptPad is ready!"
echo "==> Access at : $DOMAIN:$PORT"
echo "==========================================="
