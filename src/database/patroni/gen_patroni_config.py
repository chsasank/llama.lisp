from pathlib import Path
import os

# ---- NODE IPs ----
NODE0_IP = "192.168.10.131"
NODE1_IP = "192.168.10.131"
NODE2_IP = "192.168.10.131"

# ---- etcd ports ----
ETCD0_CLIENT, ETCD0_PEER = 3379, 3380
ETCD1_CLIENT, ETCD1_PEER = 4379, 4380
ETCD2_CLIENT, ETCD2_PEER = 5379, 5380

# ---- Patroni ports ----
NODE1_DB_PORT = 6432
NODE1_REST_PORT = 6008

NODE2_DB_PORT = 7432
NODE2_REST_PORT = 7008

# ---- PSQL Data Dir --- 

NODE1_DB_DATA_DIR = './psql-datadir'
NODE2_DB_DATA_DIR = './psql-datadir'

# ---- HAProxy ports ----
HAPROXY_PORT = 5000
HAPROXY_STATS_PORT = 7000

# ---- Authentication ----
SUPERUSER = "postgres"
SUPERPASS = "PleaseChangeMe_123"   # change the password

REPL_USER = "replicator"
REPL_PASS = "PleaseChangeMe_456"   # change the password

REWIND_USER = "rewind_user"
REWIND_PASS = "PleaseChangeMe_789"   # change the password

RESTAPI_USER = "username"
RESTAPI_PASS = "PleaseChangeMe_111"   # change the password

ETCD_PASS = "PleaseChangeMe_222"   # change the password 

# ---- Images ----
ETCD_IMAGE = "quay.io/coreos/etcd:v3.4.37"
PATRONI_IMAGE = "johnaic/patroni:17"
HAPROXY_IMAGE = "haproxy"

# ---- Base folder ----
BASE_DIR = Path(__file__).parent.resolve()



def compose_node0():
    return f"""services:
  etcd:
    image: {ETCD_IMAGE}
    ports:
      - {ETCD0_CLIENT}:2379
      - {ETCD0_PEER}:2380
    command: >
      /usr/local/bin/etcd
      --name etcd-node-0
      --data-dir=/etcd-data
      --initial-advertise-peer-urls http://{NODE0_IP}:{ETCD0_PEER}
      --listen-peer-urls http://0.0.0.0:2380
      --advertise-client-urls http://{NODE0_IP}:{ETCD0_CLIENT}
      --listen-client-urls http://0.0.0.0:2379
      --initial-cluster
      etcd-node-0=http://{NODE0_IP}:{ETCD0_PEER},etcd-node-1=http://{NODE1_IP}:{ETCD1_PEER},etcd-node-2=http://{NODE2_IP}:{ETCD2_PEER}
      --initial-cluster-state new
      --initial-cluster-token my-etcd-token
    volumes:
      - ./etcd-data:/etcd-data

  haproxy:
    image: {HAPROXY_IMAGE}
    ports:
      - {HAPROXY_STATS_PORT}:{HAPROXY_STATS_PORT}
      - {HAPROXY_PORT}:{HAPROXY_PORT}
    volumes:
      - ./haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg

"""

def haproxy_cfg():
    return f"""global
    maxconn 100
defaults
    log global
    mode tcp
    retries 2
    timeout client 30m
    timeout connect 4s
    timeout server 30m
    timeout check 5s

listen stats
    mode http
    bind *:{HAPROXY_STATS_PORT}
    stats enable
    stats uri /

listen postgres
    bind *:{HAPROXY_PORT}
    option httpchk
    http-check expect status 200
    default-server inter 3s fall 3 rise 2 on-marked-down shutdown-sessions
    server postgresql0 {NODE1_IP}:{NODE1_DB_PORT} maxconn 100 check port {NODE1_REST_PORT}
    server postgresql1 {NODE2_IP}:{NODE2_DB_PORT} maxconn 100 check port {NODE2_REST_PORT}


"""

def compose_patroni(node_ip, etcd_client, etcd_peer, etcd_name, db_port, rest_port, postgres_dir):
    return f"""services:
  etcd:
    image: {ETCD_IMAGE}
    ports:
      - {etcd_client}:2379
      - {etcd_peer}:2380
    command: >
      /usr/local/bin/etcd
      --name {etcd_name}
      --data-dir=/etcd-data
      --initial-advertise-peer-urls http://{node_ip}:{etcd_peer}
      --listen-peer-urls http://0.0.0.0:2380
      --advertise-client-urls http://{node_ip}:{etcd_client}
      --listen-client-urls http://0.0.0.0:2379
      --initial-cluster
      etcd-node-0=http://{NODE0_IP}:{ETCD0_PEER},etcd-node-1=http://{NODE1_IP}:{ETCD1_PEER},etcd-node-2=http://{NODE2_IP}:{ETCD2_PEER}
      --initial-cluster-state new
      --initial-cluster-token my-etcd-token
    volumes:
      - ./etcd-data:/etcd-data

  patroni:
    image: {PATRONI_IMAGE}
    depends_on:
      - init
    ports:
      - {db_port}:5432
      - {rest_port}:8008
    volumes:
      - ./patroni.yaml:/patroni.yaml
      - {postgres_dir}:/var/lib/postgresql/data

  init:
    image: ubuntu
    volumes:
      - {postgres_dir}:/data/
    command: bash -c "chown -R 101:103 /data && chmod 700 /data && tail -f /dev/null"
"""

def patroni_yaml(node_name, node_ip, db_port, rest_port):
    return f"""scope: johnaic
name: {node_name}

restapi:
  listen: 0.0.0.0:8008
  connect_address: {node_ip}:{rest_port}
  authentication:
    username: {RESTAPI_USER}
    password: {RESTAPI_PASS}

etcd3:
  hosts: {NODE0_IP}:{ETCD0_CLIENT},{NODE1_IP}:{ETCD1_CLIENT},{NODE2_IP}:{ETCD2_CLIENT}
  username: root
  password: {ETCD_PASS}

bootstrap:
  dcs:
    ttl: 30
    loop_wait: 10
    retry_timeout: 10
    maximum_lag_on_failover: 1048576
    postgresql:
      use_pg_rewind: true
      pg_hba:
      - host replication {REPL_USER} 0.0.0.0/0 md5
      - host all all 0.0.0.0/0 md5
      parameters:
  initdb:
  - encoding: UTF8
  - data-checksums

postgresql:
  listen: 0.0.0.0:5432
  connect_address: {node_ip}:{db_port}
  data_dir: data/postgresql
  pgpass: /tmp/pgpass0
  authentication:
    replication:
      username: {REPL_USER}
      password: {REPL_PASS}
    superuser:
      username: {SUPERUSER}
      password: {SUPERPASS}
    rewind:
      username: {REWIND_USER}
      password: {REWIND_PASS}
  parameters:
    unix_socket_directories: '..'

tags:
  noloadbalance: false
  clonefrom: false
  nostream: false
"""

def write_file(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w") as f:
        f.write(content)
    print(f"Created: {path}")

def generate_all():
    print(f"\n Generating Patroni cluster configuration at: {BASE_DIR}\n")

    node0_dir = BASE_DIR / "node0"
    write_file(node0_dir / "compose.yml", compose_node0())
    write_file(node0_dir / "haproxy.cfg", haproxy_cfg())

    node1_dir = BASE_DIR / "node1"
    write_file(node1_dir / "compose.yml",
               compose_patroni(NODE1_IP, ETCD1_CLIENT, ETCD1_PEER, "etcd-node-1",
                               NODE1_DB_PORT, NODE1_REST_PORT, NODE1_DB_DATA_DIR))
    write_file(node1_dir / "patroni.yaml",
               patroni_yaml("postgresql1", NODE1_IP, NODE1_DB_PORT, NODE1_REST_PORT))

    node2_dir = BASE_DIR / "node2"
    write_file(node2_dir / "compose.yml",
               compose_patroni(NODE2_IP, ETCD2_CLIENT, ETCD2_PEER, "etcd-node-2",
                               NODE2_DB_PORT, NODE2_REST_PORT, NODE2_DB_DATA_DIR))
    write_file(node2_dir / "patroni.yaml",
               patroni_yaml("postgresql2", NODE2_IP, NODE2_DB_PORT, NODE2_REST_PORT))

    print("\nAll configuration files have been generated successfully!\n")
    print("Next Steps (Run these commands one by one):\n")
    print("1. Build the Patroni image (only required once):")
    print(f"   cd {BASE_DIR} && podman build -t {PATRONI_IMAGE} .\n")
    print("2. Transfer node dirs to node and start the cluster containers on each of the nodes:")
    print(f"   podman-compose up -d")
    print("3. Verify cluster status from node1:")
    print(f"   podman-compose exec patroni /patroni-venv/bin/patronictl -c /patroni.yaml list\n")


if __name__ == "__main__":
    generate_all()