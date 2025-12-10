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

Single liner to deploy

```
bash build_container.sh  && podman-compose down && podman-compose up
```
