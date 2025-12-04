import importlib
import inspect

from django.db import models


def fn_to_string(fn):
    module = inspect.getmodule(fn)
    return f"{module.__name__}.{fn.__name__}"


def string_to_fn(fn_str):
    module_name, fn_name = fn_str.rsplit(".", 1)
    module = importlib.import_module(module_name)
    fn = getattr(module, fn_name)
    return fn


# Create your models here.
class Task(models.Model):
    class TaskState(models.TextChoices):
        QUEUED = "queued"
        RUNNING = "running"
        SUCCESS = "success"
        FAILED = "failed"

    fn = models.CharField()
    args = models.JSONField()
    ret_val = models.JSONField(null=True)
    state = models.CharField(choices=TaskState.choices, default=TaskState.QUEUED)

    @classmethod
    def create_task(cls, fn, args):
        task = cls.objects.create(
            fn=fn_to_string(fn), args=args, state=cls.TaskState.QUEUED
        )
        task.save()
        return task

    def run(self):
        self.state = self.TaskState.RUNNING
        self.save()

        try:
            fn = string_to_fn(self.fn)
            self.ret_val = fn(**self.args)
            self.state = self.TaskState.SUCCESS
            self.save()
        except:
            self.state = self.TaskState.FAILED
            self.save()

        return self.ret_val
