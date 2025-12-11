import logging
import sys

import oracledb
import psycopg
import pyodbc
from common import testing_database_host
from etl.common import ETLDataTypes, JSONStateManager
from etl.sources import MssqlSource, OracleSource, PostgresSource

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

# oracle source configuration
test_oracle_config = {
    "connection": {
        "host": testing_database_host,
        "port": 1521,
        "user": "SYSTEM",
        "password": "Intelarc123",
        "service": "FREEPDB1",
    },
    "table": "HR.EMPLOYEES",
}

# Testing psql source funcctions


def test_psql_init():
    src = PostgresSource(test_psql_config)
    assert isinstance(src.conn, psycopg.Connection)


def test_psql_get_tables():
    src = PostgresSource(test_psql_config)
    assert src.get_all_tables() == [
        "public.actor",
        "public.address",
        "public.category",
        "public.city",
        "public.country",
        "public.customer",
        "public.film",
        "public.film_actor",
        "public.film_category",
        "public.inventory",
        "public.language",
        "public.payment",
        "public.payment_p2022_01",
        "public.payment_p2022_02",
        "public.payment_p2022_03",
        "public.payment_p2022_04",
        "public.payment_p2022_05",
        "public.payment_p2022_06",
        "public.payment_p2022_07",
        "public.rental",
        "public.staff",
        "public.store",
        "tap_github.anonymous_contributors",
        "tap_github.assignees",
        "tap_github.branches",
        "tap_github.collaborators",
        "tap_github.commit_comments",
        "tap_github.commit_diffs",
        "tap_github.commits",
        "tap_github.community_profile",
        "tap_github.contributors",
        "tap_github.dependencies",
        "tap_github.dependents",
        "tap_github.deployment_statuses",
        "tap_github.deployments",
        "tap_github.repositories",
    ]


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
    state_manager = JSONStateManager(
        json_path="testing.json",
        state_id="test_run_psql_source",
        replication_key="commit_timestamp",
    )
    src = PostgresSource(
        test_psql_config,
        state_manager,
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


def test_mssql_get_tables():
    src = MssqlSource(test_mssql_config)
    assert src.get_all_tables() == [
        "Application.Cities",
        "Application.Cities_Archive",
        "Application.Countries",
        "Application.Countries_Archive",
        "Application.DeliveryMethods",
        "Application.DeliveryMethods_Archive",
        "Application.PaymentMethods",
        "Application.PaymentMethods_Archive",
        "Application.People",
        "Application.People_Archive",
        "Application.StateProvinces",
        "Application.StateProvinces_Archive",
        "Application.SystemParameters",
        "Application.TransactionTypes",
        "Application.TransactionTypes_Archive",
        "Purchasing.PurchaseOrderLines",
        "Purchasing.PurchaseOrders",
        "Purchasing.SupplierCategories",
        "Purchasing.SupplierCategories_Archive",
        "Purchasing.Suppliers",
        "Purchasing.Suppliers_Archive",
        "Purchasing.SupplierTransactions",
        "Sales.BuyingGroups",
        "Sales.BuyingGroups_Archive",
        "Sales.CustomerCategories",
        "Sales.CustomerCategories_Archive",
        "Sales.Customers",
        "Sales.Customers_Archive",
        "Sales.CustomerTransactions",
        "Sales.InvoiceLines",
        "Sales.Invoices",
        "Sales.OrderLines",
        "Sales.Orders",
        "Sales.SpecialDeals",
        "Warehouse.ColdRoomTemperatures",
        "Warehouse.ColdRoomTemperatures_Archive",
        "Warehouse.Colors",
        "Warehouse.Colors_Archive",
        "Warehouse.PackageTypes",
        "Warehouse.PackageTypes_Archive",
        "Warehouse.StockGroups",
        "Warehouse.StockGroups_Archive",
        "Warehouse.StockItemHoldings",
        "Warehouse.StockItems",
        "Warehouse.StockItems_Archive",
        "Warehouse.StockItemStockGroups",
        "Warehouse.StockItemTransactions",
        "Warehouse.VehicleTemperatures",
    ]


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
                ("AccountOpenedDate", ETLDataTypes.DATE),
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


def test_mssql_stream_batches_replication():
    state_manager = JSONStateManager(
        json_path="testing.json",
        state_id="test_run_mssql_source",
        replication_key="CustomerID",
    )
    src = MssqlSource(
        test_mssql_config,
        state_manager,
        batch_size=100,
    )

    schema = src.get_etl_schema()
    col_count = len(schema["columns"])

    for batch in src.stream_batches():
        assert len(batch) <= 100
        row = batch[0]
        assert len(row) == col_count

    assert src.state_manager.get_state() is not None


def test_oracle_init():
    src = OracleSource(test_oracle_config)
    assert isinstance(src.conn, oracledb.Connection)


def test_oracle_get_tables():
    src = OracleSource(test_oracle_config)
    assert src.get_all_tables() == [
        "CO.CUSTOMERS",
        "CO.INVENTORY",
        "CO.ORDERS",
        "CO.ORDER_ITEMS",
        "CO.PRODUCTS",
        "CO.SHIPMENTS",
        "CO.STORES",
        "HR.COUNTRIES",
        "HR.DEPARTMENTS",
        "HR.EMPLOYEES",
        "HR.JOBS",
        "HR.JOB_HISTORY",
        "HR.LOCATIONS",
        "HR.REGIONS",
    ]


def test_oracle_schema():
    expected_schemas = {
        "HR.EMPLOYEES": {
            "columns": [
                ("EMPLOYEE_ID", ETLDataTypes.INTEGER),
                ("FIRST_NAME", ETLDataTypes.STRING),
                ("LAST_NAME", ETLDataTypes.STRING),
                ("EMAIL", ETLDataTypes.STRING),
                ("PHONE_NUMBER", ETLDataTypes.STRING),
                ("HIRE_DATE", ETLDataTypes.DATE_TIME),
                ("JOB_ID", ETLDataTypes.STRING),
                ("SALARY", ETLDataTypes.FLOAT),
                ("COMMISSION_PCT", ETLDataTypes.FLOAT),
                ("MANAGER_ID", ETLDataTypes.INTEGER),
                ("DEPARTMENT_ID", ETLDataTypes.INTEGER),
            ],
            "primary_keys": ["EMPLOYEE_ID"],
        },
        "HR.DEPARTMENTS": {
            "columns": [
                ("DEPARTMENT_ID", ETLDataTypes.INTEGER),
                ("DEPARTMENT_NAME", ETLDataTypes.STRING),
                ("MANAGER_ID", ETLDataTypes.INTEGER),
                ("LOCATION_ID", ETLDataTypes.INTEGER),
            ],
            "primary_keys": ["DEPARTMENT_ID"],
        },
    }

    for table_name, expected_schema in expected_schemas.items():
        src = OracleSource(
            {"connection": test_oracle_config["connection"], "table": table_name}
        )
        assert src.get_etl_schema() == expected_schema, (
            f"{table_name} schema didn't match"
        )
        logger.info(f"schema matched for {table_name}")


def test_oracle_stream_batches():
    src = OracleSource(test_oracle_config, batch_size=5)
    schema = src.get_etl_schema()
    batches = src.stream_batches()
    first_batch = next(batches)
    assert len(first_batch) == 5


def test_oracle_stream_batches_replication():
    state_manager = JSONStateManager(
        json_path="testing.json",
        state_id="test_run_oracle_source",
        replication_key="EMPLOYEE_ID",
    )
    src = OracleSource(
        test_oracle_config,
        state_manager,
        batch_size=5,
    )

    schema = src.get_etl_schema()
    col_count = len(schema["columns"])

    for batch in src.stream_batches():
        assert len(batch) <= 5
        row = batch[0]
        assert len(row) == col_count

    assert src.state_manager.get_state() is not None


if __name__ == "__main__":
    test_psql_init()
    test_psql_get_tables()
    test_psql_schema()
    test_psql_stream_batches()
    test_psql_stream_batches_replication()
    test_mssql_init()
    test_mssql_get_tables()
    test_mssql_schema()
    test_mssql_stream_batches()
    test_mssql_stream_batches_replication()
    test_oracle_init()
    test_oracle_get_tables()
    test_oracle_schema()
    test_oracle_stream_batches()
    test_oracle_stream_batches_replication()
