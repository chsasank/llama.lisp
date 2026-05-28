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

#Getting domain/IP user gave during install (strip any existing port)
DOMAIN=$(grep "^CPAD_MAIN_DOMAIN=" $ENV_FILE | cut -d= -f2 | sed 's|http://||' | cut -d: -f1)
FULL_URL="http://${DOMAIN}:${PORT}"
echo "==> Access URL: $FULL_URL"
#Update cryptpad.env with correct domain:port
sed -i "s|^CPAD_MAIN_DOMAIN=.*|CPAD_MAIN_DOMAIN=$FULL_URL|" $ENV_FILE
sed -i "s|^CPAD_SANDBOX_DOMAIN=.*|CPAD_SANDBOX_DOMAIN=$FULL_URL|" $ENV_FILE
sed -i "s|^CPAD_OFFICE_URL=.*|CPAD_OFFICE_URL=$FULL_URL/onlyoffice/|" $ENV_FILE
echo "==> Updated cryptpad.env"
echo ""
echo "==> Current env:"
cat $ENV_FILE


# Restart cryptpad
echo "==> Restarting CryptPad..."
systemctl --user restart cryptpad-cryptpad.service
echo "==> Restarted"


#Show URLs
HASH=$(journalctl --user -u cryptpad-cryptpad.service --no-pager | grep "install/#" | tail -1 | grep -o 'install/#[a-f0-9]*' | cut -d# -f2)
echo ""
echo "==========================================="
echo "==> CryptPad is ready!"
echo "==> Access at : $FULL_URL"
if [ -n "$HASH" ]; then
echo "==> Admin URL : $FULL_URL/install/#$HASH"
fi
echo "==========================================="
