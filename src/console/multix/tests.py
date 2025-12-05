from django.test import TestCase
from .models import Container
import subprocess


# Create your tests here.
class TaskCreateContainer(TestCase):
    def test_createDelete_containers(self):
        container = Container.create_containers()

        assert container.status == "running"
        print(f"contaniner id is {container.container_id}")

        output = subprocess.run(
            ["podman", "exec", "-it", container.container_id, "ls"], capture_output=True
        )
        print(output)
        if output.returncode == 0:
            print("containers created successfully")
            container = Container.stop_containers(container)
            if container.status == 0:
                print("container excited successfully")
            elif container.status == 1:
                print("contaner not excited successfully")

        else:
            print(
                f"containers not created successfully and return code is {output.returncode}"
            )
