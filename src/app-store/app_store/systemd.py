import subprocess


def reload_units():
    cmd = ["systemctl", "--user", "daemon-reload"]
    subprocess.run(cmd, check=True)
    cmd = ["systemctl", "--user", "reset-failed"]
    subprocess.run(cmd, check=True)
    print(f"==> systemd reloaded ✅")


def status_units(app_name):
    print(f"==> systemd status")
    cmd = ["systemctl", "--user", "--all", "status", f"{app_name}*"]
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
