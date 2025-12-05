# Create your models here.
from django.db import models
import subprocess


# Create your models here.


class Container(models.Model):
    container_id = models.CharField()
    image = models.CharField()
    name = models.CharField()
    command = models.CharField()
    created = models.CharField()
    status = models.CharField()

    @classmethod
    def create_containers(cls, image="ubuntu:24.04"):
        # command to create a podman container is podman run -d -it image_name program to run.
        container_object = subprocess.run(
            ["podman", "run", "-d", "-it", f"{image}", "bash"], capture_output=True
        )
        container_id = container_object.stdout.decode("utf-8").split("\n")[0]

        name_object = subprocess.run(
            ["podman", "inspect", "--format='{{.Name}}", f"{container_id}"],
            capture_output=True,
        )
        name = name_object.stdout.decode("utf-8").split("\n")[0].split("'")[1]

        created_object = subprocess.run(
            ["podman", "inspect", "--format='{{.Created}}'", f"{container_id}"],
            capture_output=True,
        )
        created = created_object.stdout.decode("utf-8").split("\n")[0].split("'")[1]

        status_object = subprocess.run(
            ["podman", "inspect", "--format='{{.State.Status}}'", f"{container_id}"],
            capture_output=True,
        )
        status = status_object.stdout.decode("utf-8").split("\n")[0].split("'")[1]

        container = cls.objects.create(
            container_id=container_id, name=name, created=created, status=status
        )
        container.save()
        return container

    def stop_containers(container):
        container_id_object = subprocess.run(
            ["podman", "stop", "-t=5", f"{container.container_id}"], capture_output=True
        )
        return_code = container_id_object.returncode
        command_output = subprocess.run(
            [
                "podman",
                "inspect",
                "--format='{{.State.Status}}'",
                f"{container.container_id}",
            ],
            capture_output=True,
        )
        status = command_output.stdout.decode("utf-8").split("\n")[0].split("'")[1]
        if status == "exited" and return_code == 0:
            print("sucessfully excited")
            container.status = status
            return container
        else:
            print("\n\n", command_output, "\n\n")
            print("exit not successfull")
            print(f"Status is {status}")
            return container
