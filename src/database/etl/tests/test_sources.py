import logging
import sys

import oracledb
import psycopg
import pyodbc
import MySQLdb
from common import testing_database_host
from etl.common import ETLDataTypes, JSONStateManager
from etl.sources import MssqlSource, OracleSource, PostgresSource, MysqlSource

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

# mysql source configuration
test_mysql_config = {
    "connection": {
        "host": testing_database_host,
        "port": 3306,
        "user": "root",
        "password": "Intelarc123",
        "database": "employees",
    },
    "table": "employees.departments",
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


def test_oracle_schema():
    expected_schemas = {
        "HR.EMPLOYEES": {
            "columns": [
                ("EMPLOYEE_ID", ETLDataTypes.INTEGER),
                ("FIRST_NAME", ETLDataTypes.STRING),
                ("LAST_NAME", ETLDataTypes.STRING),
                ("EMAIL", ETLDataTypes.STRING),
                ("PHONE_NUMBER", ETLDataTypes.STRING),
                ("HIRE_DATE", ETLDataTypes.DATE),
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


def test_mysql_init():
    src = MysqlSource(test_mysql_config)
    assert isinstance(src.conn, MySQLdb.connections.Connection)

def test_mysql_get_tables():
    src = MysqlSource(test_mysql_config)
    assert src.get_all_tables() == [
        "employees.departments",
        "employees.dept_emp",
        "employees.dept_manager",
        "employees.employees",
        "employees.salaries",
        "employees.titles",
        "sakila.actor",
        "sakila.address",
        "sakila.category",
        "sakila.city",
        "sakila.country",
        "sakila.customer",
        "sakila.film",
        "sakila.film_actor",
        "sakila.film_category",
        "sakila.film_text",
        "sakila.inventory",
        "sakila.language",
        "sakila.payment",
        "sakila.rental",
        "sakila.staff",
        "sakila.store",
    ]


def test_mysql_schema():
    expected_schemas = {
        "employees.departments": {
            "columns": [
                ("dept_no", ETLDataTypes.STRING),
                ("dept_name", ETLDataTypes.STRING),
            ],
            "primary_keys": ["dept_no"],
        },

        "employees.current_dept_emp": {
            "columns": [
                ("emp_no", ETLDataTypes.INTEGER),
                ("dept_no", ETLDataTypes.STRING),
                ("from_date", ETLDataTypes.DATE),
                ("to_date", ETLDataTypes.DATE),
            ],
            "primary_keys": [],
        },

        "employees.dept_emp_latest_date": {
            "columns": [
                ("emp_no", ETLDataTypes.INTEGER),
                ("from_date", ETLDataTypes.DATE),
                ("to_date", ETLDataTypes.DATE),
            ],
            "primary_keys": [],
        },

        "sakila.actor": {
            "columns": [
                ("actor_id", ETLDataTypes.INTEGER),
                ("first_name", ETLDataTypes.STRING),
                ("last_name", ETLDataTypes.STRING),
                ("last_update", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["actor_id"],
        },

        "sakila.inventory": {
            "columns": [
                ("inventory_id", ETLDataTypes.INTEGER),
                ("film_id", ETLDataTypes.INTEGER),
                ("store_id", ETLDataTypes.INTEGER),
                ("last_update", ETLDataTypes.DATE_TIME),
            ],
            "primary_keys": ["inventory_id"],
        },

        "sakila.sales_by_store": {
            "columns": [
                ("store", ETLDataTypes.STRING),
                ("manager", ETLDataTypes.STRING),
                ("total_sales", ETLDataTypes.FLOAT),
            ],
            "primary_keys": [],
        },
    }


    for table_name, expected_schema in expected_schemas.items():
        src = MysqlSource(
            {"connection": test_mysql_config["connection"], "table": table_name}
        )
        assert src.get_etl_schema() == expected_schema, (
            f"{table_name} schema didn't match"
        )
        logger.info(f"MySQL schema matched!")

def test_mysql_stream_batches():
    src = MysqlSource(test_mysql_config, batch_size=5)
    batches = src.stream_batches()
    first_batch = next(batches)

    assert len(first_batch) <= 5
    assert len(first_batch[0]) == len(src.get_etl_schema()["columns"])

def test_mysql_stream_batches_replication():
    state_manager = JSONStateManager(
        json_path="testing.json",
        state_id="test_run_mysql_source",
        replication_key="dept_no",
    )

    src = MysqlSource(
        test_mysql_config,
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
    test_psql_schema()
    test_psql_stream_batches()
    test_psql_stream_batches_replication()
    test_mssql_init()
    test_mssql_schema()
    test_mssql_stream_batches()
    test_mssql_stream_batches_replication()
    test_oracle_init()
    test_oracle_schema()
    test_oracle_stream_batches()
    test_oracle_stream_batches_replication()
    test_mysql_init()
    test_mysql_schema()
    test_mysql_stream_batches()
    test_mysql_stream_batches_replication()
    test_mysql_get_tables()
