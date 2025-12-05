from django.test import TestCase, Client

from .models import DatabaseConfiguration, ETLConfiguration
from django.urls import reverse
from unittest.mock import patch


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


class DatabaseViewsTests(TestCase):
    def setUp(self):
        self.client = Client()

        self.db_source = DatabaseConfiguration.objects.create(
            etl_type="source",
            database_type="postgres",
            connection_config={"host": "localhost", "port": 5432},
        )

        self.db_target = DatabaseConfiguration.objects.create(
            etl_type="target",
            database_type="postgres",
            connection_config={"host": "hello", "port": 5433},
        )

    def test_home_view(self):
        response = self.client.get(reverse("home"))
        self.assertEqual(response.status_code, 200)
        self.assertTemplateUsed(response, "databank/base.html")

    def test_database_list_view(self):
        response = self.client.get(reverse("database_list"))
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.status_code, 200)

        print("--- database list view ---")
        print(response.context.keys())
        print("Databases:", response.context["databases"])

    def test_database_create_view(self):
        response = self.client.get(reverse("database_create"))
        self.assertEqual(response.status_code, 200)
        self.assertIn("form", response.context)
        self.assertEqual(
            response.context["form"].__class__.__name__,
            "DatabaseConfigurationForm"
        )

        print("--- form fields ---")
        print("Form fields:", response.context["form"].fields.keys())

        post_data = {
            "etl_type": "source",
            "database_type": "postgres",
            "connection_config": '{"host": "x", "port": 1111}',
        }
        response = self.client.post(reverse("database_create"), post_data)
        self.assertEqual(response.status_code, 302)

        # Confirms the entry created, in setup we are already creating 2 db's, so total 3 db's
        self.assertEqual(DatabaseConfiguration.objects.count(), 3)

    def test_database_edit_view_get(self):
        url = reverse("database_edit", args=[self.db_source.id])
        response = self.client.get(url)

        # Status + template
        self.assertEqual(response.status_code, 200)
        self.assertTemplateUsed(response, "databank/database_form.html")

        print("--- context for edit view (GET) ---")
        print("Context keys:", response.context.keys())
        print("Form initial data:", response.context["form"].initial)

        # form should contain the below existing values
        form = response.context["form"]
        self.assertEqual(form.initial["etl_type"], "source")
        self.assertEqual(form.initial["database_type"], "postgres")
        self.assertEqual(form.initial["connection_config"], {"host": "localhost", "port": 5432})

    def test_database_edit_view_post(self):
        url = reverse("database_edit", args=[self.db_source.id])

        print("Before edit:", self.db_source.connection_config)

        post_data = {
            "etl_type": "source",
            "database_type": "postgres",
            "connection_config": '{"host": "newhost", "port": 5432}',
        }
        response = self.client.post(url, post_data)

        # it will redirect after edit
        self.assertEqual(response.status_code, 302)

        updated = DatabaseConfiguration.objects.get(id=self.db_source.id)

        print("After edit:", updated.connection_config)
        self.assertEqual(updated.connection_config["host"], "newhost")


class ETLViewsTests(TestCase):
    def setUp(self):
        self.client = Client()

        self.db_source = DatabaseConfiguration.objects.create(
            etl_type="source",
            database_type="postgres",
            connection_config={"host": "localhost", "port": 5432},
        )

        self.db_target = DatabaseConfiguration.objects.create(
            etl_type="target",
            database_type="postgres",
            connection_config={"host": "localhost", "port": 5433},
        )

        self.etl = ETLConfiguration.objects.create(
            source_database=self.db_source,
            target_database=self.db_target,
            source_table="src_table",
            target_table="tgt_table",
        )

    def test_etl_list_view(self):
        response = self.client.get(reverse("etl_list"))

        print("--- etl list view ---")
        print("Status:", response.status_code)
        print("Context keys:", response.context.keys())
        print("ETLs:", list(response.context["etls"]))

        self.assertEqual(response.status_code, 200)
        self.assertContains(response, "src_table")

    def test_etl_create_view(self):
        # GET method
        response = self.client.get(reverse("etl_create"))
        self.assertEqual(response.status_code, 200)

        print("--- etl create view (GET) ---")
        print("Context keys:", response.context.keys())
        print("Form fields:", response.context["form"].fields.keys())

        # POST method
        post_data = {
            "source_database": self.db_source.id,
            "target_database": self.db_target.id,
            "source_table": "new_src",
            "target_table": "new_tgt",
            "replication_key": "",
            "replication_state": "{}",
        }
        response = self.client.post(reverse("etl_create"), post_data)

        print("--- etl create view (POST) ---")
        print("Status:", response.status_code)
        print("Total ETLs in DB:", ETLConfiguration.objects.count())

        self.assertEqual(response.status_code, 302)

    def test_etl_edit_view(self):
        url = reverse("etl_edit", args=[self.etl.id])

        print("--- before edit ---")
        print("Old Source Table:", self.etl.source_table)

        post_data = {
            "source_database": self.db_source.id,
            "target_database": self.db_target.id,
            "source_table": "updated_src",
            "target_table": "updated_tgt",
            "replication_key": "",
            "replication_state": "{}",
        }

        response = self.client.post(url, post_data)

        updated = ETLConfiguration.objects.get(id=self.etl.id)

        print("--- after edit ---")
        print("Updated Source Table:", updated.source_table)
        print("Status:", response.status_code)

        self.assertEqual(response.status_code, 302)
        self.assertEqual(updated.source_table, "updated_src")

    # RUN SINGLE ETL WITH MOCK
    @patch("databank.views.run_etl")
    def test_run_single_etl(self, mock_run_etl):
        url = reverse("run_single_etl", args=[self.etl.id])

        response = self.client.get(url)

        print("--- run single etl ---")
        print("Mock called:", mock_run_etl.called)
        print("Called with:", mock_run_etl.call_args)

        mock_run_etl.assert_called_once_with(self.etl.id)
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, "completed")

    # RUN ALL ETLS WITH MOCK
    @patch("databank.views.run_etl")
    def test_run_all_etls(self, mock_run_etl):
        url = reverse("run_all_etls")
        response = self.client.get(url)

        print("--- run all etls ---")
        print("Mock called:", mock_run_etl.called)
        print("Call count:", mock_run_etl.call_count)
        print("Response JSON:", response.json())

        mock_run_etl.assert_called()
        self.assertEqual(response.status_code, 200)