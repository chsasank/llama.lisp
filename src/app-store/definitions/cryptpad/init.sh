#!/bin/sh
set -e
chown -R 4001:4001 /cryptpad/blob
chown -R 4001:4001 /cryptpad/block
chown -R 4001:4001 /cryptpad/data
chown -R 4001:4001 /cryptpad/datastore
chown -R 4001:4001 /cryptpad/customize
chown -R 4001:4001 /cryptpad/www/common/onlyoffice/dist
chown -R 4001:4001 /cryptpad/onlyoffice-conf
tail -f /dev/null
