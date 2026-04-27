#!/bin/sh
set -e

echo "Setting up volumes..."

mkdir -p /var/www/html/data
mkdir -p /var/www/html/custom

chown -R 33:33 /var/www/html

echo "Volumes ready"
