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

Fill the secrets in `deploy/.pg.env`:

```
POSTGRES_PASSWORD=xxxxxxxxxx
POSTGRES_DB=console
POSTGRES_USER=postgres
```

Single liner to deploy

```
bash build_container.sh  && podman-compose down && podman-compose up
```
