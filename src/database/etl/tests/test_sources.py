import logging
import sys

import psycopg
import pyodbc
from common import testing_database_host
from etl.common import ETLDataTypes
from etl.sources import MssqlSource, PostgresSource

logging.basicConfig(stream=sys.stderr, level=logging.INFO)
logger = logging.getLogger(__name__)

# psql source configuration
test_psql_config = {
    "connection": {
        "host": testing_database_host,
        "port": 5511,
        "user": "testing",
        "password": "intelarc",
        "database": "github",
    },
    "table": "tap_github.commits",
}

# mssql source configuration
test_mssql_config = {
    "connection": {
        "host": testing_database_host,
        "port": 1433,
        "user": "SA",
        "password": "Intelarc@123",
        "database": "WideWorldImporters",
    },
    "table": "Sales.Customers",
}

# Testing psql source funcctions


def test_psql_init():
    src = PostgresSource(test_psql_config)
    assert isinstance(src.conn, psycopg.Connection)


def test_psql_schema():
    expected_schemas = {
        "tap_github.commits": {
            "columns": [
                ("org", ETLDataTypes.STRING),
                ("repo", ETLDataTypes.STRING),
                ("repo_id", ETLDataTypes.INTEGER),
                ("node_id", ETLDataTypes.STRING),
                ("url", ETLDataTypes.STRING),
                ("sha", ETLDataTypes.STRING),
                ("html_url", ETLDataTypes.STRING),
                ("commit_timestamp", ETLDataTypes.DATE_TIME),
                ("commit", ETLDataTypes.JSON),
                ("author", ETLDataTypes.JSON),
                ("committer", ETLDataTypes.JSON),
                ("_sdc_extracted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_received_at", ETLDataTypes.DATE_TIME),
                ("_sdc_batched_at", ETLDataTypes.DATE_TIME),
                ("_sdc_deleted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_sequence", ETLDataTypes.INTEGER),
                ("_sdc_table_version", ETLDataTypes.INTEGER),
                ("_sdc_sync_started_at", ETLDataTypes.INTEGER),
            ],
            "primary_keys": ["node_id"],
        },
        "public.actor": {
            "columns": [
                ("actor_id", ETLDataTypes.INTEGER),
                ("first_name", ETLDataTypes.STRING),
                ("last_name", ETLDataTypes.STRING),
                ("last_update", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["actor_id"],
        },
        "public.staff": {
            "columns": [
                ("staff_id", ETLDataTypes.INTEGER),
                ("first_name", ETLDataTypes.STRING),
                ("last_name", ETLDataTypes.STRING),
                ("address_id", ETLDataTypes.INTEGER),
                ("email", ETLDataTypes.STRING),
                ("store_id", ETLDataTypes.INTEGER),
                ("active", ETLDataTypes.BOOLEAN),
                ("username", ETLDataTypes.STRING),
                ("password", ETLDataTypes.STRING),
                ("last_update", ETLDataTypes.DATE_TIME),
                ("picture", ETLDataTypes.BYTES),
            ],
            "primary_keys": ["staff_id"],
        },
        "public.payment_p2022_07": {
            "columns": [
                ("payment_id", ETLDataTypes.INTEGER),
                ("customer_id", ETLDataTypes.INTEGER),
                ("staff_id", ETLDataTypes.INTEGER),
                ("rental_id", ETLDataTypes.INTEGER),
                ("amount", ETLDataTypes.FLOAT),
                ("payment_date", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["payment_date", "payment_id"],
        },
        "tap_github.dependencies": {
            "columns": [
                ("repo", ETLDataTypes.STRING),
                ("org", ETLDataTypes.STRING),
                ("repo_id", ETLDataTypes.INTEGER),
                ("dependency_repo_id", ETLDataTypes.INTEGER),
                ("package_name", ETLDataTypes.STRING),
                ("package_manager", ETLDataTypes.STRING),
                ("requirements", ETLDataTypes.STRING),
                ("has_dependencies", ETLDataTypes.BOOLEAN),
                ("dependency", ETLDataTypes.JSON),
                ("_sdc_extracted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_received_at", ETLDataTypes.DATE_TIME),
                ("_sdc_batched_at", ETLDataTypes.DATE_TIME),
                ("_sdc_deleted_at", ETLDataTypes.DATE_TIME),
                ("_sdc_sequence", ETLDataTypes.INTEGER),
                ("_sdc_table_version", ETLDataTypes.INTEGER),
                ("_sdc_sync_started_at", ETLDataTypes.INTEGER),
            ],
            "primary_keys": [
                "repo_id",
                "package_name",
                "package_manager",
                "requirements",
            ],
        },
    }

    for table_name, expected_schema in expected_schemas.items():
        src = PostgresSource(
            {"connection": test_psql_config["connection"], "table": table_name}
        )
        assert src.get_etl_schema() == expected_schema, (
            f"{table_name} schema didn't match"
        )
        logger.info(f"schema matched for {table_name}")


def test_psql_stream_batches():
    src = PostgresSource(test_psql_config, batch_size=100)
    batches = src.stream_batches()
    first_batch = next(batches)
    assert len(first_batch) == 100
    row = first_batch[0]
    assert row[0] == "meltano"
    assert row[1] == "meltano"
    assert len(row) == len(src.get_etl_schema()["columns"])


def test_psql_stream_batches_replication():
    src = PostgresSource(
        {**test_psql_config, "replication_key": "commit_timestamp"},
        state_id="test_run_psql_source",
        batch_size=100,
    )
    for batch in src.stream_batches():
        assert len(batch) <= 100
        row = batch[0]
        assert row[0] == "meltano"
        assert row[1] == "meltano"
        assert len(row) == len(src.get_etl_schema()["columns"])

    assert src.state_manager.get_state() is not None


# Testing mssql source functions


def test_mssql_init():
    src = MssqlSource(test_mssql_config)
    assert isinstance(src.conn, pyodbc.Connection)


def test_mssql_schema():
    expected_schemas = {
        "Sales.Customers": {
            "columns": [
                ("CustomerID", ETLDataTypes.INTEGER),
                ("CustomerName", ETLDataTypes.STRING),
                ("BillToCustomerID", ETLDataTypes.INTEGER),
                ("CustomerCategoryID", ETLDataTypes.INTEGER),
                ("BuyingGroupID", ETLDataTypes.INTEGER),
                ("PrimaryContactPersonID", ETLDataTypes.INTEGER),
                ("AlternateContactPersonID", ETLDataTypes.INTEGER),
                ("DeliveryMethodID", ETLDataTypes.INTEGER),
                ("DeliveryCityID", ETLDataTypes.INTEGER),
                ("PostalCityID", ETLDataTypes.INTEGER),
                ("CreditLimit", ETLDataTypes.FLOAT),
                ("AccountOpenedDate", ETLDataTypes.DATE_TIME),
                ("StandardDiscountPercentage", ETLDataTypes.FLOAT),
                ("IsStatementSent", ETLDataTypes.BOOLEAN),
                ("IsOnCreditHold", ETLDataTypes.BOOLEAN),
                ("PaymentDays", ETLDataTypes.INTEGER),
                ("PhoneNumber", ETLDataTypes.STRING),
                ("FaxNumber", ETLDataTypes.STRING),
                ("DeliveryRun", ETLDataTypes.STRING),
                ("RunPosition", ETLDataTypes.STRING),
                ("WebsiteURL", ETLDataTypes.STRING),
                ("DeliveryAddressLine1", ETLDataTypes.STRING),
                ("DeliveryAddressLine2", ETLDataTypes.STRING),
                ("DeliveryPostalCode", ETLDataTypes.STRING),
                ("DeliveryLocation", ETLDataTypes.STRING),
                ("PostalAddressLine1", ETLDataTypes.STRING),
                ("PostalAddressLine2", ETLDataTypes.STRING),
                ("PostalPostalCode", ETLDataTypes.STRING),
                ("LastEditedBy", ETLDataTypes.INTEGER),
                ("ValidFrom", ETLDataTypes.DATE_TIME),
                ("ValidTo", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["CustomerID"],
        },
        "Purchasing.Suppliers": {
            "columns": [
                ("SupplierID", ETLDataTypes.INTEGER),
                ("SupplierName", ETLDataTypes.STRING),
                ("SupplierCategoryID", ETLDataTypes.INTEGER),
                ("PrimaryContactPersonID", ETLDataTypes.INTEGER),
                ("AlternateContactPersonID", ETLDataTypes.INTEGER),
                ("DeliveryMethodID", ETLDataTypes.INTEGER),
                ("DeliveryCityID", ETLDataTypes.INTEGER),
                ("PostalCityID", ETLDataTypes.INTEGER),
                ("SupplierReference", ETLDataTypes.STRING),
                ("BankAccountName", ETLDataTypes.STRING),
                ("BankAccountBranch", ETLDataTypes.STRING),
                ("BankAccountCode", ETLDataTypes.STRING),
                ("BankAccountNumber", ETLDataTypes.STRING),
                ("BankInternationalCode", ETLDataTypes.STRING),
                ("PaymentDays", ETLDataTypes.INTEGER),
                ("InternalComments", ETLDataTypes.STRING),
                ("PhoneNumber", ETLDataTypes.STRING),
                ("FaxNumber", ETLDataTypes.STRING),
                ("WebsiteURL", ETLDataTypes.STRING),
                ("DeliveryAddressLine1", ETLDataTypes.STRING),
                ("DeliveryAddressLine2", ETLDataTypes.STRING),
                ("DeliveryPostalCode", ETLDataTypes.STRING),
                ("DeliveryLocation", ETLDataTypes.STRING),  # geography → STRING
                ("PostalAddressLine1", ETLDataTypes.STRING),
                ("PostalAddressLine2", ETLDataTypes.STRING),
                ("PostalPostalCode", ETLDataTypes.STRING),
                ("LastEditedBy", ETLDataTypes.INTEGER),
                ("ValidFrom", ETLDataTypes.DATE_TIME),
                ("ValidTo", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["SupplierID"],
        },
    }

    for table_name, expected_schema in expected_schemas.items():
        src = MssqlSource(
            {"connection": test_mssql_config["connection"], "table": table_name}
        )
        assert src.get_etl_schema() == expected_schema, (
            f"{table_name} schema didn't match"
        )
        logger.info(f"schema matched for {table_name}")


def test_mssql_stream_batches():
    src = MssqlSource(test_mssql_config, batch_size=100)
    schema = src.get_etl_schema()
    batches = src.stream_batches()
    first_batch = next(batches)
    assert len(first_batch) == 100
    row = first_batch[0]
    assert row[0] == 1
    assert row[1] == "Tailspin Toys (Head Office)"
    assert len(row) == len(schema["columns"])


def test_mssql_stream_batches_replication():
    src = MssqlSource(
        {**test_mssql_config, "replication_key": "CustomerID"},
        state_id="test_run_mssql_source",
        batch_size=100,
    )

    schema = src.get_etl_schema()
    col_count = len(schema["columns"])

    for batch in src.stream_batches():
        assert len(batch) <= 100
        row = batch[0]
        assert len(row) == col_count

    assert src.state_manager.get_state() is not None


if __name__ == "__main__":
    test_psql_init()
    test_psql_schema()
    test_psql_stream_batches()
    test_psql_stream_batches_replication()
    test_mssql_init()
    test_mssql_schema()
    test_mssql_stream_batches()
    test_mssql_stream_batches_replication()
