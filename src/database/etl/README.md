# ETL Pipelines

How to build the image

```
podman build -t etl .
```

How to run tests

```
bash tests/data/create_testing_db.sh
podman run -it -v `pwd`:/app/etl/ etl python tests/test_sources.py
```
