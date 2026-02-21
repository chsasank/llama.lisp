FROM ubuntu:24.04

RUN apt-get update && apt-get install -y \
    postgresql-common python3-venv python3-pip gnupg curl \
 && yes | /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh \
 && apt-get update

RUN apt-get install -y \
    postgresql-17 \
    postgresql-17-postgis-3 \
    postgresql-17-postgis-3-scripts \
    postgresql-contrib \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN python3 -m venv /patroni-venv && \
    /patroni-venv/bin/pip install patroni[etcd3,psycopg2-binary]

USER postgres
ENV PATH=/usr/lib/postgresql/17/bin/:$PATH
WORKDIR /var/lib/postgresql

CMD ["/patroni-venv/bin/patroni", "/patroni.yaml"]
