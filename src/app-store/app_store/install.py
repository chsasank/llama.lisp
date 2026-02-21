import os
import argparse
import glob
import socket
import tinydb
import shutil
import subprocess
import datetime
from parser import (
    parse_sexp,
    generate_systemd,
    lookup_sexp,
    parse_env_file,
    generate_env_file,
)
from systemd import (
    reload_units,
    status_units,
    start_units,
    stop_units,
    restart_units,
    log_units,
    podman_pull,
    podman_build,
)
from interpreter import config_lisp

definitions_path = os.path.join(
    os.path.dirname(os.path.realpath(__file__)), "../definitions/*/*.lisp"
)
definitions = {
    os.path.basename(f)[: -len(".lisp")]: f for f in glob.glob(definitions_path)
}

app_dir = os.path.join(os.path.expanduser("~"), ".johnny")
backups_dir = os.path.join(os.path.expanduser("~"), ".backup_johnny")
os.makedirs(app_dir, exist_ok=True)
app_db = tinydb.TinyDB(os.path.join(app_dir, "app_db.json"))

systemd_dir = os.path.join(os.path.expanduser("~"), ".config/containers/systemd/")
os.makedirs(systemd_dir, exist_ok=True)


def get_random_free_port():
    sock = socket.socket()
    sock.bind(("", 0))
    return sock.getsockname()[1]


def gen_pod(app_name, ports):
    App = tinydb.Query()
    app_data = app_db.search(App.name == app_name)

    port_mapping = {str(p): str(get_random_free_port()) for p in ports}
    if app_data:
        # if app_data exists, overwrite with old ports
        old_ports = app_data[0]["ports"]
        port_mapping = {k: old_ports.get(k, v) for k, v in port_mapping.items()}
        app_db.update({"ports": port_mapping}, App.name == app_name)
    else:
        app_db.insert({"name": app_name, "ports": port_mapping})

    pod_service_data = [
        [
            "Pod",
            [
                ["PublishPort", f"{p_host}:{p_container}"]
                for p_container, p_host in port_mapping.items()
            ],
        ],
        ["Install", [["WantedBy", "default.target"]]],
    ]
    return generate_systemd(pod_service_data)


def gen_container(app_name, container):
    definitions_dir = os.path.dirname(definitions[app_name])
    try:
        image = lookup_sexp(container, "image")[0]
        podman_pull(image)
    except KeyError:
        image = lookup_sexp(container, "build")[0]
        podman_build(image, definitions_dir)

    try:
        volumes = lookup_sexp(container, "volumes")
    except KeyError:
        volumes = []
    container_name = lookup_sexp(container, "name")[0]

    # volumes
    volume_mapping = {}
    app_data_dir = os.path.join(app_dir, app_name)
    os.makedirs(app_data_dir, exist_ok=True)
    for name, container_path in volumes:
        # check if name is found in definitions_path
        # if so copy it to app_data_dir
        defns_path = os.path.join(definitions_dir, name)
        host_path = os.path.join(app_data_dir, name)
        if os.path.exists(host_path):
            # do nothing
            pass
        elif os.path.isfile(defns_path):
            shutil.copy(defns_path, host_path)
        elif os.path.isdir(defns_path):
            shutil.copytree(defns_path, host_path)
        else:
            # create a new dir if nothing exists
            os.makedirs(host_path)

        volume_mapping[host_path] = container_path

    # environment
    try:
        env = lookup_sexp(container, "environment")
    except KeyError:
        env = []
    env = {k: v for k, v in env}
    env_file_name = os.path.join(app_data_dir, f"{container_name}.env")
    if os.path.isfile(env_file_name):
        with open(env_file_name) as f:
            env_existing = parse_env_file(f.read())

        # keep existing values
        env.update(env_existing)

    with open(env_file_name, "w") as f:
        f.write(generate_env_file(env))

    # additional flags
    try:
        additional_flags = lookup_sexp(container, "additional-flags")
    except KeyError:
        additional_flags = []

    additional_flags = " ".join(additional_flags)

    # command
    try:
        command = lookup_sexp(container, "command")[0]
    except (KeyError, IndexError):
        command = ""

    # entrypoint
    try:
        entrypoint = lookup_sexp(container, "entrypoint")[0]
    except (KeyError, IndexError):
        entrypoint = ""

    container_options = [
        ["Image", image],
        ["Pod", f"{app_name}.pod"],
        ["EnvironmentFile", env_file_name],
        ["PodmanArgs", additional_flags],
        ["Exec", command],
    ]
    for host_dir, container_dir in volume_mapping.items():
        container_options.append(["Volume", f"{host_dir}:{container_dir}"])

    if entrypoint:
        container_options.append(["Entrypoint", entrypoint])

    container_service_data = [
        ["Container", container_options],
        [
            "Service",
            [["Restart", "always"]],
        ],
        ["Install", [["WantedBy", "default.target"]]],
    ]
    return generate_systemd(container_service_data)


def install(app_name):
    with open(definitions[app_name]) as f:
        app_defn = config_lisp(parse_sexp(f.read()))

    # TODO: write a validator, possibly based on a schema/grammar
    ports = lookup_sexp(app_defn, "ports")

    pod_fname = os.path.join(systemd_dir, f"{app_name}.pod")
    pod_service = gen_pod(app_name, ports)
    with open(pod_fname, "w") as f:
        f.write(pod_service)
        print(f"==> installed {pod_fname} ✅")

    containers = lookup_sexp(app_defn, "containers")
    for container in containers:
        container_name = lookup_sexp(container, "name")[0]
        container_fname = os.path.join(
            systemd_dir, f"{app_name}-{container_name}.container"
        )
        container_service = gen_container(app_name, container)
        with open(container_fname, "w") as f:
            f.write(container_service)
            print(f"==> installed {container_fname} ✅")

    reload_units()
    restart_units(app_name)
    show_ports(app_name)
    status_units(app_name)


def uninstall(app_name):
    stop_units(app_name)
    app_systemd_files = glob.glob(os.path.join(systemd_dir, f"{app_name}*"))
    for fname in app_systemd_files:
        os.remove(fname)
        print(f"==> removed {fname} ✅")

    reload_units()


def show_ports(app_name):
    App = tinydb.Query()
    app_data = app_db.search(App.name == app_name)
    port_mapping = app_data[0]["ports"]
    for p_container, p_host in port_mapping.items():
        print(f"==> host port 🌐 {p_host} was mapped to app port 📦 {p_container}")


def printenv(app_name):
    app_data_dir = os.path.join(app_dir, app_name)
    env_files = glob.glob(os.path.join(app_data_dir, "*.env"))
    for fname in env_files:
        print(f"==> {os.path.abspath(fname)}:")
        with open(fname) as f:
            print(f.read())


def backup(app_name):
    if app_name == "all":
        for app in app_db.all():
            backup(app["name"])
    else:
        print(f"==> backing up {app_name}")
        app_data_dir = os.path.join(app_dir, app_name)
        if os.path.isdir(app_data_dir):
            backup_data_dir = os.path.join(backups_dir, app_name)
            os.makedirs(backup_data_dir, exist_ok=True)
            now = datetime.datetime.now().strftime("%Y_%m_%d_%H_%M")

            cmd = [
                "podman",
                "run",
                "--rm",
                "-it",
                "-v",
                f"{app_data_dir}:/data/src/",
                "-v",
                f"{backup_data_dir}:/data/tgt/",
                "-w",
                "/data/src/",
                "ubuntu:24.04",
                "tar",
                "-czf",
                f"/data/tgt/backup_{now}.tar.gz",
                ".",
            ]
            subprocess.run(cmd, check=True)
            print(f"==> {app_name} backed up ✅")


def list_apps(app_name):
    if app_name == "installed":
        for app in app_db.all():
            print(app["name"])
    elif app_name == "all":
        for app_name in definitions:
            print(app_name)
    else:
        show_ports(app_name)
        status_units(app_name)

def main():
    examples_text = """
Examples:
---------

Install an app called thelounge

    johnny install thelounge

Check status of the app once installed

    johnny status thelounge

Find open ports

    johnny ports thelounge

Print environment used

    johnny printenv thelounge

Stop the app

    johnny stop thelounge

Restart it

    johnny restart thelounge

Get logs of an app

    johnny logs thelounge

Backup an app

    johnny backup thelounge

List installed apps

    johnny list all|installed|thelounge

"""

    parser = argparse.ArgumentParser(
        description="JOHNAIC package manager",
        prog="johnny",
        epilog=examples_text,
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        "action",
        type=str,
        help="One of install|uninstall|start|stop|restart|ports|printenv|logs|backup|status",
    )
    parser.add_argument("app_name", type=str, help="Name of the app")
    args = parser.parse_args()

    action = args.action
    app_name = args.app_name

    if action == "install":
        install(app_name)
    elif action == "uninstall":
        uninstall(app_name)
    elif action == "start":
        start_units(app_name)
    elif action == "stop":
        stop_units(app_name)
    elif action == "restart":
        restart_units(app_name)
    elif action == "status":
        status_units(app_name)
    elif action == "ports":
        show_ports(app_name)
    elif action == "printenv":
        printenv(app_name)
    elif action == "logs":
        log_units(app_name)
    elif action == "backup":
        backup(app_name)
    elif action == "list":
        list_apps(app_name)
    else:
        raise RuntimeError(f"Unknown action {action}")


if __name__ == "__main__":
    main()
