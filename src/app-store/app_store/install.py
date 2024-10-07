import os
import argparse
import glob
import socket
import pysondb
from parser import parse_sexp, generate_systemd
from systemd import reload_units, status_units, start_units, stop_units, restart_units

definitions_path = os.path.join(
    os.path.dirname(os.path.realpath(__file__)), "../definitions/*.lisp"
)
definitions = {
    os.path.basename(f)[: -len(".lisp")]: f for f in glob.glob(definitions_path)
}

app_dir = os.path.join(os.path.expanduser("~"), ".johnny")
os.makedirs(app_dir, exist_ok=True)
app_db = pysondb.db.getDb(os.path.join(app_dir, "app_db.json"))

systemd_dir = os.path.join(os.path.expanduser("~"), ".config/containers/systemd/")
os.makedirs(systemd_dir, exist_ok=True)


def lookup_sexp(sexp, key):
    """Look up key value in a config list"""
    for elem in sexp:
        if elem[0] == key:
            return elem[1:]

    raise KeyError("Key not found")


def get_random_free_port():
    sock = socket.socket()
    sock.bind(("", 0))
    return sock.getsockname()[1]


def gen_pod(app_name, ports):
    app_data = app_db.getByQuery({"name": app_name})

    port_mapping = {str(p): str(get_random_free_port()) for p in ports}
    if app_data:
        # if app_data exists, overwrite with old ports
        old_ports = app_data[0]["ports"]
        port_mapping.update(old_ports)
        app_db.updateByQuery({"name": app_name}, {"ports": port_mapping})
    else:
        app_db.add({"name": app_name, "ports": port_mapping})

    pod_service_data = [
        [
            "Pod",
            [
                ["PublishPort", f"{p_host}:{p_container}"]
                for p_container, p_host in port_mapping.items()
            ],
        ],
        [
            "Install",
                [["WantedBy", "default.target"]]
        ]
    ]
    return generate_systemd(pod_service_data)


def gen_container(app_name, container):
    image = lookup_sexp(container, "image")[0]
    volumes = lookup_sexp(container, "volumes")

    volume_mapping = {}
    app_data_dir = os.path.join(app_dir, app_name)
    for name, container_dir in volumes:
        host_dir = os.path.join(app_data_dir, name)
        os.makedirs(host_dir, exist_ok=True)
        volume_mapping[host_dir] = container_dir

    container_service_data = [
        [
            "Container",
            [["Image", image], ["Pod", f"{app_name}.pod"]]
            + [
                ["Volume", f"{host_dir}:{container_dir}"]
                for host_dir, container_dir in volume_mapping.items()
            ],
        ],
        [
            "Service",
            [["Restart", "always"]],
        ],
        [
            "Install",
                [["WantedBy", "default.target"]]
        ]
    ]
    return generate_systemd(container_service_data)


def install(app_name):
    with open(definitions[app_name]) as f:
        app_defn = parse_sexp(f.read())

    # TODO: write a validator, possibly based on a schema/grammar
    ports = lookup_sexp(app_defn, "ports")

    pod_fname = os.path.join(systemd_dir, f"{app_name}.pod")
    pod_service = gen_pod(app_name, ports)
    with open(pod_fname, "w") as f:
        f.write(pod_service)
        print(f'==> installed {pod_fname} ✅')

    containers = lookup_sexp(app_defn, "containers")
    for container in containers:
        container_name = lookup_sexp(container, "name")[0]
        container_fname = os.path.join(
            systemd_dir, f"{app_name}-{container_name}.container"
        )
        container_service = gen_container(app_name, container)
        with open(container_fname, "w") as f:
            f.write(container_service)
            print(f'==> installed {container_fname} ✅')

    reload_units()
    start_units(app_name)
    show_ports(app_name)
    status_units(app_name)


def uninstall(app_name):
    stop_units(app_name)
    app_systemd_files = glob.glob(os.path.join(systemd_dir, f'{app_name}*'))
    for fname in app_systemd_files:
        os.remove(fname)
        print(f'==> removed {fname} ✅')
    
    reload_units()

def show_ports(app_name):
    app_data = app_db.getByQuery({"name": app_name})
    port_mapping = app_data[0]["ports"]
    for p_container, p_host in port_mapping.items():
        print(f"==> host port 🌐 {p_host} was mapped to app port 📦 {p_container}")

def main():
    parser = argparse.ArgumentParser(description='JOHNAIC package manager')
    parser.add_argument('action',type=str,
                    help='One of install|uninstall|start|stop|restart|ports|status')
    parser.add_argument('app_name',type=str,
                    help='Name of the app')
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
    elif action == 'restart':
        restart_units(app_name)
    elif action == "status":
        status_units(app_name)
    elif action == "ports":
        show_ports(app_name)
    else:
        raise RuntimeError(f"Unknown action {action}")


if __name__ == '__main__':
    main()