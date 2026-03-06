# Console

Test:

```
python manage.py test
```

Run worker:

```
python manage.py startworker
```

Create superuser

```
python manage.py createsuperuser
```

## Build Docker

```
bash build_container.sh
```

Fill the secrets in `deploy/.pg.env` from the example:

```
cp deploy/example.pg.env deploy/.pg.env
```

Single liner to deploy

```
bash build_container.sh  && podman-compose down && podman-compose up -d
```
