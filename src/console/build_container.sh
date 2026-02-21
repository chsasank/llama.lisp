#!/usr/bin/env bash
cd "$(dirname "$0")"
set -e

# first build etl container
pushd ../database/etl
podman build -t johnaic/etl .
popd

podman build -t johnaic/console .
