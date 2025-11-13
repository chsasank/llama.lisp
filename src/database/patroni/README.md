# Heading 1 Patroni HA Cluster Setup (Fully Automated)

This project provides a fully automated way to generate and deploy a PostgreSQL High Availability (HA) cluster using Patroni, Etcd, and HAProxy — all managed with Podman containers. This is a 3 node configuration to do the following:

1. node0: Etcd + HAProxy
2. node1: Etcd + PostgreSQL Leader/Replica
3. node2: Etcd + PostgreSQL Leader/Replica

## Generate configuration

With a single Python script, all configuration files are generated automatically for 3 nodes (node0, node1, and node2), and the cluster can be launched using simple commands. Modify the variables in the script to your requirements. 

```python
# ---- Patroni ports ----
NODE1_DB_PORT = 5432
NODE1_REST_PORT = 8008

NODE2_DB_PORT = 5432
NODE2_REST_PORT = 8008

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

```

Now run the generator

```
python gen_patroni_config.py
```

After running the Python generator, your directory will look like this:

```
patroni/
├── Dockerfile
├── gen-patroni-config.py
├── node0/
│   ├── compose.yml
│   └── haproxy.cfg
├── node1/
│   ├── compose.yml
│   └── patroni.yaml
└── node2/
    ├── compose.yml
    └── patroni.yaml
```

## Build Patroni Image

Next, build the Patroni Image. Build your custom Patroni image using the included Dockerfile:

```
podman build -t johnaic/patroni:17 .
```

This image installs:

1. PostgreSQL 17
2. Patroni with Etcd3 support
3. Psycopg2
4. Ubuntu base image (24.04)


## Start the Cluster

SCP node folders to each of the nodes and run `podman-compose up -d` in each of them:
```
scp ./node0 node0:~/patroni/
scp ./node1 node1:~/patroni/
scp ./node1 node2:~/patroni/
```

Repeat this for each of node 0, 1, 2
```
ssh nodex
cd ~/patroni
podman-compose up -d
podman ps 
```


## Check Cluster Status

Once all containers are running, you can check Patroni’s cluster health:

```
podman-compose exec patroni /patroni-venv/bin/patronictl -c /patroni.yaml list
```
Example output:
```
+ Cluster: johnaic (7571813802550083606) -----+-----------+----+-------------+-----+------------+-----+
| Member      | Host                | Role    | State     | TL | Receive LSN | Lag | Replay LSN | Lag |
+-------------+---------------------+---------+-----------+----+-------------+-----+------------+-----+
| postgresql1 | 192.168.10.131:6432 | Leader  | running   |  1 |             |     |            |     |
| postgresql2 | 192.168.10.131:7432 | Replica | streaming |  1 |   0/3000060 |   0 |  0/3000060 |   0 |
+-------------+---------------------+---------+-----------+----+-------------+-----+------------+-----+
```
