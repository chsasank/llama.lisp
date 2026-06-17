# Forgejo

Forgejo with PostgreSQL, based on the official Docker installation guide:
https://forgejo.org/docs/latest/admin/installation/docker/

## Containers

- `forgejo` — Forgejo server
- `postgres` — PostgreSQL database

## Ports

- `3000` — Web UI
- `22` — Git SSH

## First install

The Forgejo data volume must be writable by UID/GID 1000 inside the container. After installation, ensure `~/.johnny/forgejo/forgejo-data` is owned by `1000:1000`:

```bash
chown -R 1000:1000 ~/.johnny/forgejo/forgejo-data
systemctl --user restart forgejo-forgejo
```

Then open `http://localhost:3000` (using the host port shown by `johnny ports forgejo`) to complete onboarding.
