import logging
import sys
import time

from django.core.management.base import BaseCommand
from django.utils import timezone
from task_manager.models import Task

logging.basicConfig(stream=sys.stderr, level=logging.INFO)
logger = logging.getLogger(__name__)


class Command(BaseCommand):
    help = "Run task manager"

    def add_arguments(self, parser):
        parser.add_argument("--polling_time", type=int, default=5)

    def handle(self, *args, **options):
        while True:
            try:
                task_to_run = (
                    Task.objects.filter(
                        state=Task.TaskState.QUEUED, next_run_at__lte=timezone.now()
                    )
                    .order_by("created_at")
                    .get()
                )
                now = time.time()
                task_to_run.run()
                run_time = time.time() - now
                logger.info(f"Ran {task_to_run.fn} task in {run_time} seconds")
            except Task.DoesNotExist:
                polling_time = options["polling_time"]
                logger.info(f"No queued tasks. Waiting {polling_time} seconds.")
                time.sleep(polling_time)
