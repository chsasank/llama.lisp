# ETL Pipelines

How to build the image

```
podman build -t etl .
```

How to run tests

```
bash tests/data/create_testing_db.sh
# to run only one test
podman run -it -v `pwd`:/app/etl/ etl python tests/test_sources.py

# run all tests
podman run -it -v `pwd`:/app/etl/ etl pytest
```
