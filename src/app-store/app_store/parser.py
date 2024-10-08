import subprocess
import os
import json
import sys


def lookup_sexp(sexp, key):
    """Look up key value in a config list"""
    for elem in sexp:
        if elem[0] == key:
            return elem[1:]

    raise KeyError("Key not found")


def parse_sexp(sexp_data):
    parser_file = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "sexp-json.scm"
    )
    cmd = ["guile", parser_file]
    result = subprocess.run(
        cmd, stdout=subprocess.PIPE, input=sexp_data, text=True, check=True
    )
    output = json.loads(result.stdout)
    return output


def parse_systemd(service_file):
    """Systemd syntax parser. Have not found any utlity to make this easy.

    multiline configs are not handled.

    Reference: https://www.freedesktop.org/software/systemd/man/latest/systemd.syntax.html
    """

    def is_section_line(l):
        return l.startswith("[") and l.endswith("]")

    def parse_section_line(l):
        return l[1:-1]

    def is_config_line(l):
        return "=" in l

    def parse_config_line(l):
        left, right = l.split("=", maxsplit=1)
        return [left.strip(), right.strip()]

    data = []

    lines = [
        l.strip()
        for l in service_file.splitlines()
        if not (l.startswith((";", "#")) or l.strip() == "")
    ]

    try:
        assert is_section_line(lines[0])
        section_name = parse_section_line(lines[0])
    except:
        raise RuntimeError("Failed to parse")

    data.append([section_name, []])

    for l in lines[1:]:
        if is_config_line(l):
            config = parse_config_line(l)
            data[-1][1].append(config)
        elif is_section_line(l):
            section_name = parse_section_line(l)
            data.append([section_name, []])
        else:
            raise RuntimeError("Failed to parse")

    return data


def generate_systemd(service_data):
    """Generate systemd file"""

    def generate_section_line(section):
        return f"[{section}]"

    def generate_config_line(config):
        assert len(config) == 2
        return f"{config[0]}={config[1]}"

    service_lines = []
    for section, config_data in service_data:
        service_lines.append(generate_section_line(section))
        for config in config_data:
            service_lines.append(generate_config_line(config))

        service_lines.append("")

    return "\n".join(service_lines)


def parse_env_file(env_data):
    lines = [
        l.strip()
        for l in env_data.splitlines()
        if not (l.startswith("#") or l.strip() == "")
    ]

    env = {}
    for l in lines:
        k, v = l.split("=", maxsplit=1)
        env[k] = v

    return env


def generate_env_file(env):
    return "\n".join([f"{k}={v}" for k, v in env.items()]) + "\n"
