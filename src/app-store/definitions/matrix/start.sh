#!/bin/bash
set -e

if [ ! -f /data/homeserver.yaml ]; then
    echo "Config not found. Generating"
    python /start.py generate
    echo "Applying postgres config"
    python << END
import os
import yaml

with open("/data/homeserver.yaml") as f:
     synapse_config = yaml.safe_load(f)

synapse_config['database'] = {
     'name': 'psycopg2', 
     'args': {
          'user': 'synapse',
          'password': os.environ["POSTGRES_PASSWORD"],
          'database': 'synapse',
          'host': 'localhost',
          'cp_min': 5,
          'cp_max': 10
     }}

with open("/data/homeserver.yaml", "w") as f:
     yaml.dump(synapse_config, f, default_flow_style=False, sort_keys=False)
END
fi

python /start.py
