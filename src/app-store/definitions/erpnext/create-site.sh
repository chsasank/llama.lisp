set -e
wait-for-it -t 120 $DB_HOST:$DB_PORT;
wait-for-it -t 120 $REDIS_CACHE;
wait-for-it -t 120 $REDIS_QUEUE;
export start=`date +%s`;
until [[ -n `grep -hs ^ sites/common_site_config.json | jq -r ".db_host // empty"` ]] && \
    [[ -n `grep -hs ^ sites/common_site_config.json | jq -r ".redis_cache // empty"` ]] && \
    [[ -n `grep -hs ^ sites/common_site_config.json | jq -r ".redis_queue // empty"` ]];
do
    echo "Waiting for sites/common_site_config.json to be created";
    sleep 5;
    if (( `date +%s`-start > 120 )); then
    echo "could not find sites/common_site_config.json with required keys";
    exit 1
    fi
done;
echo "sites/common_site_config.json found";
bench new-site --mariadb-user-host-login-scope='%' --admin-password=$ADMIN_PASSWORD --db-root-username=root --db-root-password=$DB_PASSWORD --install-app erpnext --install-app payments --install-app hrms --install-app print_designer  --install-app india_compliance --install-app webshop   --set-default frontend;
tail -f /dev/null
