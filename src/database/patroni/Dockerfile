FROM ubuntu:24.04
RUN apt-get update && apt-get install -y sudo postgresql-common python3-venv python3-pip && yes | /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh && apt install -y postgresql-17 
RUN python3 -m venv /patroni-venv &&  /patroni-venv/bin/pip install patroni[etcd3,psycopg2-binary]
USER postgres
ENV PATH=/usr/lib/postgresql/17/bin/:$PATH
WORKDIR /var/lib/postgresql
CMD ["/patroni-venv/bin/patroni", "/patroni.yaml"]

