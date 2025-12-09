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


class Graph(models.Model):
    name = models.CharField(unique=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)


class Task(models.Model):
    class TaskState(models.TextChoices):
        QUEUED = "queued"
        RUNNING = "running"
        SUCCESS = "success"
        FAILED = "failed"

    graph = models.ForeignKey(Graph, on_delete=models.CASCADE)
    fn = models.CharField()
    args = models.JSONField()
    ret_val = models.JSONField(null=True)
    state = models.CharField(choices=TaskState.choices, default=TaskState.QUEUED)

    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    @classmethod
    def create_task(cls, fn, args, graph):
        task = cls.objects.create(
            fn=fn_to_string(fn), args=args, state=cls.TaskState.QUEUED, graph=graph
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
            return self.ret_val
        except:
            self.state = self.TaskState.FAILED
            self.save()
            return
