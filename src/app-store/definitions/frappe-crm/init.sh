#!/bin/bash
set -e

bash <<'EOF'
cd /home/frappe/frappe-bench/sites

cat > apps.txt <<EOT
frappe
crm
EOT

cat > common_site_config.json <<EOT
{
  "redis_cache": "redis://localhost:6379",
  "redis_queue": "redis://localhost:6380"
}
EOT

exit
EOF

echo "Waiting 10 seconds for services to initialize..."
sleep 10 

bench new-site ${SITENAME} \
  --db-host localhost \
  --db-port 3306 \
  --db-name "$DB_USER" \
  --db-root-username root \
  --db-root-password "$MYSQL_ROOT_PASSWORD" \
  --admin-password "$ADMIN_PASSWORD" \
  --set-default

bench --site ${SITENAME} install-app crm

bench --site ${SITENAME} enable-scheduler

tail -f /dev/null