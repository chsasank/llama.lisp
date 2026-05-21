#!/bin/sh
set -e

echo "Waiting for DB..."
sleep 20

cd /var/www/html

if [ -f "data/config.php" ]; then
  echo "EspoCRM already installed. Skipping."
  exit 0
fi

echo "Installing EspoCRM..."

php command.php install \
  --db-host="$ESPOCRM_DATABASE_HOST" \
  --db-name="$ESPOCRM_DATABASE_NAME" \
  --db-user="$ESPOCRM_DATABASE_USER" \
  --db-password="$ESPOCRM_DATABASE_PASSWORD" \
  --admin-username="$ESPOCRM_ADMIN_USERNAME" \
  --admin-password="$ESPOCRM_ADMIN_PASSWORD" \
  --site-url="http://localhost"

echo "Init completed"
