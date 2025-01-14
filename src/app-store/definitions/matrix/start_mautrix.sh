set -e
if [ ! -f /data/config.yaml  ]; then
    echo "Config not found. Generating"
    /docker-run.sh
    
    apk add --no-cache py-yaml
    python << END
import os
import yaml

with open("/data/config.yaml") as f:
    mautrix_config = yaml.safe_load(f)

mautrix_config['homeserver']['address'] = "http://localhost:8008"
mautrix_config['homeserver']['domain'] = "$SYNAPSE_SERVER_NAME"
mautrix_config['database']['uri'] = "postgres://mautrix:$MAUTRIX_POSTGRES_PASSWORD@localhost:5433/mautrix?sslmode=disable"
mautrix_config['appservice']['address'] = "http://localhost:29318"
mautrix_config['bridge']['permissions'] = {
    '*': 'relay', '$SYNAPSE_SERVER_NAME': 'user', '@admin:$SYNAPSE_SERVER_NAME': 'admin'
}

with open("/data/config.yaml", "w") as f:
    yaml.dump(mautrix_config, f, default_flow_style=False, sort_keys=False)
END

    # generate registration
    /docker-run.sh
fi

/docker-run.sh
