#!/usr/bin/env bash
cd "$(dirname "$0")"

set -e
rm -rf build
mkdir build
cd build
tee frappe_apps.json << END
[
  {
    "url": "https://github.com/frappe/erpnext",
    "branch": "version-15"
  },
  {
    "url": "https://github.com/frappe/hrms",
    "branch": "version-15"
  },
  {
    "url": "https://github.com/frappe/webshop",
    "branch": "version-15"
  },
  {
    "url": "https://github.com/frappe/payments",
    "branch": "version-15"
  },
  {
    "url": "https://github.com/resilient-tech/india-compliance",
    "branch": "version-15"
  },
  {
    "url": "https://github.com/frappe/print_designer",
    "branch": "v1.5.1"
  }
]
END
APPS_JSON_BASE64=$(base64 -w 0 frappe_apps.json)
git clone https://github.com/frappe/frappe_docker
cd frappe_docker
podman build \
  --build-arg=FRAPPE_PATH=https://github.com/frappe/frappe \
  --build-arg=FRAPPE_BRANCH=version-15 \
  --build-arg=APPS_JSON_BASE64=$APPS_JSON_BASE64 \
  --tag=docker.io/johnaic/erpnext:15 \
  --file=images/layered/Containerfile .
podman push docker.io/johnaic/erpnext:15
