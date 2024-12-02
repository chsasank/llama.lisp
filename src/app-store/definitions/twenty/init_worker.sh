chown -R 1000:1000 /mnt/server-local-data
chown -R 1000:1000 /mnt/docker-data
chmod +x /mnt/entrypoint.sh
tail -f /dev/null