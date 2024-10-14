import subprocess


def reload_units():
    cmd = ["systemctl", "--user", "daemon-reload"]
    subprocess.run(cmd, check=True)
    cmd = ["systemctl", "--user", "reset-failed"]
    subprocess.run(cmd, check=True)
    print(f"==> systemd reloaded ✅")


def status_units(app_name):
    print(f"==> systemd status")
    cmd = [
        "systemctl",
        "--user",
        "--no-pager",
        "--all",
        "status",
        f"{app_name}*",
        "--lines=0",
    ]
    subprocess.run(cmd)


def start_units(app_name):
    cmd = ["systemctl", "--user", "--all", "start", f"{app_name}*"]
    subprocess.run(cmd, check=True)
    print(f"==> {app_name} started ✅")


def stop_units(app_name):
    cmd = ["systemctl", "--user", "--all", "stop", f"{app_name}*"]
    subprocess.run(cmd, check=True)
    print(f"==> {app_name} stopped ✅")


def restart_units(app_name):
    cmd = ["systemctl", "--user", "--all", "restart", f"{app_name}*"]
    subprocess.run(cmd, check=True)
    print(f"==> {app_name} restarted ✅")


def log_units(app_name):
    try:
        cmd = ["journalctl", "--user", "-n", "100", "-f", "-u", f"{app_name}*"]
        subprocess.run(cmd, check=True)
    except KeyboardInterrupt:
        pass


def podman_pull(image_name):
    print(f"==> pulling image {image_name}")
    cmd = ["podman", "pull", image_name]
    subprocess.run(cmd, check=True)


def podman_build(image_name, definitions_dir):
    print(f"==> building image {image_name}")
    cmd = ["podman", "build", "-t", image_name, definitions_dir]
    subprocess.run(cmd, check=True)
