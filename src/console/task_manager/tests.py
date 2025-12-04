from django.test import TestCase

from .models import Task


def add(x, y):
    return x + y


# Create your tests here.
class TaskModelTestCase(TestCase):
    def test_create_task(self):
        task = Task.create_task(fn=add, args={"x": 2, "y": 6})
        assert task.fn == "task_manager.tests.add"
        assert task.args == {"x": 2, "y": 6}
        assert task.state == Task.TaskState.QUEUED

        for queued_task in Task.objects.filter(state=Task.TaskState.QUEUED):
            out = queued_task.run()
            assert out == 8
            assert queued_task.state == Task.TaskState.SUCCESS
