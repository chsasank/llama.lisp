import datetime
import importlib
import inspect
import logging

from django.db import models
from django.utils import timezone

logger = logging.getLogger(__name__)


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

    graph = models.ForeignKey(Graph, on_delete=models.CASCADE, related_name="tasks")
    fn = models.CharField()
    args = models.JSONField()
    ret_val = models.JSONField(null=True)
    state = models.CharField(choices=TaskState.choices, default=TaskState.QUEUED)

    # run every periodic_interval seconds if not null
    periodic_interval = models.FloatField(null=True)

    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    next_run_at = models.DateTimeField(auto_now_add=True)

    @classmethod
    def create_task(cls, fn, args, graph, periodic_interval=None):
        task = cls.objects.create(
            fn=fn_to_string(fn),
            args=args,
            state=cls.TaskState.QUEUED,
            graph=graph,
            periodic_interval=periodic_interval,
            next_run_at=timezone.now(),
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
            logger.info(f"Task {self.fn} ran successfully")
        except:
            self.state = self.TaskState.FAILED
            logger.exception(f"Task {self.fn} failed to run")

        if self.periodic_interval is not None:
            self.state = self.TaskState.QUEUED
            self.next_run_at = timezone.now() + datetime.timedelta(
                seconds=self.periodic_interval
            )
            logger.info(f"Task {self.fn} scheduled to run again at {self.next_run_at}")

        self.save()
        return self.ret_val
