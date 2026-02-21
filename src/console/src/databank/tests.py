import logging
import sys
from unittest.mock import patch

from django.contrib.auth.models import User
from django.test import Client, TestCase
from django.urls import reverse

from . import tasks
from .models import DatabaseConfiguration, ETLConfiguration
from .tasks import DBStateManager

logging.basicConfig(stream=sys.stderr, level=logging.INFO)
logger = logging.getLogger(__name__)
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

    @patch("databank.tasks.PostgresSource")
    @patch("databank.tasks.PostgresTarget")
    def test_run_etl(self, mock_target_cls, mock_source_cls):
        mock_source = mock_source_cls.return_value
        mock_target = mock_target_cls.return_value

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

    @patch("databank.tasks.PostgresSource")
    @patch("databank.tasks.PostgresTarget")
    def test_run_etl_replication_key(self, mock_target_cls, mock_source_cls):
        mock_source = mock_source_cls.return_value
        mock_target = mock_target_cls.return_value

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

        assert ETLConfiguration.objects.filter(id=etl_config.id).exists()


class DatabaseViewsTests(TestCase):
    def setUp(self):
        self.client = Client()

        self.user = User.objects.create_user(
            username="testuser",
            password="testpass",
        )

        logged_in = self.client.login(
            username="testuser",
            password="testpass",
        )
        assert logged_in is True

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

        logger.info("--- database list view ---")
        logger.info(f"{response.context.keys()}")
        logger.info(f"Databases: {response.context['databases']}")

    def test_database_create_view(self):
        response = self.client.get(reverse("database_create"))
        self.assertEqual(response.status_code, 200)
        self.assertIn("form", response.context)
        self.assertEqual(
            response.context["form"].__class__.__name__, "DatabaseConfigurationForm"
        )

        logger.info("--- form fields ---")
        logger.info(
            f"Form fields: {response.context['form'].fields.keys()}",
        )

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

        logger.info("--- context for edit view (GET) ---")
        logger.info(f"Context keys: {response.context.keys()}")
        logger.info(f"Form initial data: {response.context['form'].initial}")

        # form should contain the below existing values
        form = response.context["form"]
        self.assertEqual(form.initial["etl_type"], "source")
        self.assertEqual(form.initial["database_type"], "postgres")
        self.assertEqual(
            form.initial["connection_config"], {"host": "localhost", "port": 5432}
        )

    def test_database_edit_view_post(self):
        url = reverse("database_edit", args=[self.db_source.id])

        logger.info(f"Before edit: {self.db_source.connection_config}")

        post_data = {
            "etl_type": "source",
            "database_type": "postgres",
            "connection_config": '{"host": "newhost", "port": 5432}',
        }
        response = self.client.post(url, post_data)

        # it will redirect after edit
        self.assertEqual(response.status_code, 302)

        updated = DatabaseConfiguration.objects.get(id=self.db_source.id)

        logger.info(f"After edit: {updated.connection_config}")
        self.assertEqual(updated.connection_config["host"], "newhost")


class ETLViewsTests(TestCase):
    def setUp(self):
        self.client = Client()

        self.user = User.objects.create_user(
            username="testuser",
            password="testpass",
        )

        logged_in = self.client.login(
            username="testuser",
            password="testpass",
        )
        assert logged_in is True

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

        logger.info("--- etl list view ---")
        logger.info(
            f"Status: {response.status_code}",
        )
        logger.info(f"Context keys: {response.context.keys()}")
        logger.info(f"ETLs: {list(response.context['etls'])}")

        self.assertEqual(response.status_code, 200)
        self.assertContains(response, "src_table")

    def test_etl_create_view(self):
        # GET method
        response = self.client.get(reverse("etl_create"))
        self.assertEqual(response.status_code, 200)

        logger.info("--- etl create view (GET) ---")
        logger.info(f"Context keys: {response.context.keys()}")
        logger.info(
            f"Form fields: {response.context['form'].fields.keys()}",
        )

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

        logger.info("--- etl create view (POST) ---")
        logger.info(f"Status: {response.status_code}")
        logger.info(f"Total ETLs in DB: {ETLConfiguration.objects.count()}")

        self.assertEqual(response.status_code, 302)

    def test_etl_edit_view(self):
        url = reverse("etl_edit", args=[self.etl.id])

        logger.info("--- before edit ---")
        logger.info(f"Old Source Table: {self.etl.source_table}")

        post_data = {
            "source_database": self.db_source.id,
            "target_database": self.db_target.id,
            "source_table": "updated_src",
            "target_table": "updated_tgt",
            "run_interval": 0,
            "replication_key": "",
            "replication_state": "{}",
        }

        response = self.client.post(url, post_data)

        updated = ETLConfiguration.objects.get(id=self.etl.id)

        logger.info("--- after edit ---")
        logger.info(f"Updated Source Table: {updated.source_table}")
        logger.info(f"Status: {response.status_code}")

        self.assertEqual(response.status_code, 302)
        self.assertEqual(updated.source_table, "updated_src")


class DBStateManagerTests(TestCase):
    def setUp(self):
        self.source_db = DatabaseConfiguration.objects.create(
            etl_type="source",
            database_type="postgres",
            connection_config={"host": "localhost"},
        )
        self.target_db = DatabaseConfiguration.objects.create(
            etl_type="target",
            database_type="postgres",
            connection_config={"host": "localhost"},
        )

    def test_get_state_with_backfill(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={
                "replication_value": "2025-12-15 14:30:00",
                "backfill": 3600,
            },
        )

        state = DBStateManager(etl_config).get_state()

        assert state == "2025-12-15 13:30:00"

    def test_get_state_without_backfill(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={
                "replication_value": "2025-12-15 16:34:30",
            },
        )

        state = DBStateManager(etl_config).get_state()

        assert state == "2025-12-15 16:34:30"

    def test_get_state_with_backfill_and_invalid_datetime(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={
                "replication_value": "not-a-datetime",
                "backfill": 3600,
            },
        )

        with self.assertRaises(AssertionError):
            DBStateManager(etl_config).get_state()

    def test_get_state_without_replication_value(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={},
        )

        state = DBStateManager(etl_config).get_state()

        assert state is None

    def test_get_state_with_zero_backfill(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={
                "replication_value": "2025-12-15 16:34:30",
                "backfill": 0,
            },
        )

        state = DBStateManager(etl_config).get_state()

        assert state == "2025-12-15 16:34:30"

    def test_get_state_first_run_returns_none(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={},
        )

        state = DBStateManager(etl_config).get_state()
        assert state is None

    def test_get_state_with_set_state(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={
                "replication_value": "2025-12-15 16:34:30",
                "backfill": 3600,
            },
        )

        state_manager = DBStateManager(etl_config)
        state_manager.set_state("2025-12-15 15:34:30")
        state = state_manager.get_state()
        assert state == "2025-12-15 14:34:30"

    def test_get_state_rounds_off_microseconds_with_backfill(self):
        etl_config = ETLConfiguration.objects.create(
            source_database=self.source_db,
            target_database=self.target_db,
            source_table="src_table",
            target_table="tgt_table",
            replication_key="commit_timestamp",
            replication_state={
                "replication_value": "2025-12-15 16:34:30.987654",
                "backfill": 60,
            },
        )

        state = DBStateManager(etl_config).get_state()

        # microseconds should be removed AND backfill applied
        assert state == "2025-12-15 16:33:30"

