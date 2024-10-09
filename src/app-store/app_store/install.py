import os
import argparse
import glob
import socket
import tinydb
import shutil
from parser import (
    parse_sexp,
    generate_systemd,
    lookup_sexp,
    parse_env_file,
    generate_env_file,
)
from systemd import reload_units, status_units, start_units, stop_units, restart_units
from interpreter import config_lisp

definitions_path = os.path.join(
    os.path.dirname(os.path.realpath(__file__)), "../definitions/*/*.lisp"
)
definitions = {
    os.path.basename(f)[: -len(".lisp")]: f for f in glob.glob(definitions_path)
}

app_dir = os.path.join(os.path.expanduser("~"), ".johnny")
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
    image = lookup_sexp(container, "image")[0]
    volumes = lookup_sexp(container, "volumes")
    container_name = lookup_sexp(container, "name")[0]

    # volumes
    volume_mapping = {}
    app_data_dir = os.path.join(app_dir, app_name)
    definitions_dir = os.path.dirname(definitions[app_name])
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

    container_service_data = [
        [
            "Container",
            [
                ["Image", image],
                ["Pod", f"{app_name}.pod"],
                ["EnvironmentFile", env_file_name],
                ["PodmanArgs", additional_flags],
                ["Exec", command]
            ]
            + [
                ["Volume", f"{host_dir}:{container_dir}"]
                for host_dir, container_dir in volume_mapping.items()
            ],
        ],
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
        print(f"# {fname}:")
        with open(fname) as f:
            print(f.read())


def main():
    parser = argparse.ArgumentParser(description="JOHNAIC package manager")
    parser.add_argument(
        "action",
        type=str,
        help="One of install|uninstall|start|stop|restart|ports|status",
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
    else:
        raise RuntimeError(f"Unknown action {action}")


if __name__ == "__main__":
    main()
