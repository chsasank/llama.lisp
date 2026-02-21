#!/usr/bin/env bash
cd "$(dirname "$0")"

set -e
rm -rf build
mkdir build
cd build
git clone https://github.com/chsasank/mediacms.git -b add-social-logins
cd mediacms
podman build -t docker.io/johnaic/mediacms:4.6.0 .
podman push docker.io/johnaic/mediacms:4.6.0
