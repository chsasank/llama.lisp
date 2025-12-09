import logging
import sys

from django.test import TestCase

from . import tasks
from .models import DatabaseConfiguration, ETLConfiguration

logging.basicConfig(stream=sys.stderr, level=logging.INFO)
testing_database_host = "localhost"


# Create your tests here.
class DatabaseConfigurationModelTests(TestCase):
    def test_model_creation(self):
        db_config = DatabaseConfiguration.objects.create(
            etl_type="source",
            database_type="postgres",
            connection_config={"host": "localhost", "blah": "blah"},
        )
        db_config.save()

        for existing_db_config in DatabaseConfiguration.objects.all():
            assert existing_db_config.database_type == "postgres"

        # note that choices are just for documentation
        db_config = DatabaseConfiguration.objects.create(
            etl_type="source_llololol",
            database_type="postgres",
            connection_config={"host": "localhost", "blah": "blah"},
        )
        db_config.save()


class ETLConfigurationModelTests(TestCase):
    def test_model_creation(self):
        source_db_config = DatabaseConfiguration.objects.create(
            etl_type="source",
            database_type="postgres",
            connection_config={"host": "localhost", "blah": "blah"},
        )
        target_db_config = DatabaseConfiguration.objects.create(
            etl_type="target",
            database_type="postgres",
            connection_config={"host": "localhost", "blah": "blah"},
        )
        etl_config = ETLConfiguration.objects.create(
            source_database=source_db_config,
            target_database=target_db_config,
            source_table="table_from",
            target_table="table_to",
        )
        etl_config.save()
        assert etl_config.source_database.etl_type == "source"

    def test_run_etl(self):
        source_db_config = DatabaseConfiguration.objects.create(
            etl_type="source",
            database_type="postgres",
            connection_config={
                "host": testing_database_host,
                "port": 5511,
                "user": "testing",
                "password": "intelarc",
                "database": "github",
            },
        )
        target_db_config = DatabaseConfiguration.objects.create(
            etl_type="target",
            database_type="postgres",
            connection_config={
                "host": testing_database_host,
                "port": 5512,
                "user": "testing",
                "password": "intelarc",
                "database": "github",
            },
        )
        etl_config = ETLConfiguration.objects.create(
            source_database=source_db_config,
            target_database=target_db_config,
            source_table="tap_github.commits",
            target_table="tap_github.commits",
        )

        tasks.run_etl(etl_config.id)

    def test_run_etl_replication_key(self):
        source_db_config = DatabaseConfiguration.objects.create(
            etl_type="source",
            database_type="postgres",
            connection_config={
                "host": testing_database_host,
                "port": 5511,
                "user": "testing",
                "password": "intelarc",
                "database": "github",
            },
        )
        target_db_config = DatabaseConfiguration.objects.create(
            etl_type="target",
            database_type="postgres",
            connection_config={
                "host": testing_database_host,
                "port": 5512,
                "user": "testing",
                "password": "intelarc",
                "database": "github",
            },
        )
        etl_config = ETLConfiguration.objects.create(
            source_database=source_db_config,
            target_database=target_db_config,
            source_table="tap_github.commits",
            target_table="tap_github.commits",
            replication_key="commit_timestamp",
        )

        tasks.run_etl(etl_config.id)

        # verify if etl_config is updated
        etl_config.refresh_from_db()
        assert etl_config.replication_state == {
            "replication_value": "2025-11-25 04:36:31"
        }
