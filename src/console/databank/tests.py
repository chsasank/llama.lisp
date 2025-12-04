from django.test import TestCase

from .models import DatabaseConfiguration, ETLConfiguration


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
