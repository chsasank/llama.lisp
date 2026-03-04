from django.urls import path

from . import views

urlpatterns = [
    path("", views.home, name="home"),
    # Dashboard CRUD
    path("dashboard/", views.dashboard, name="dashboard_view"),
    # Database CRUD
    path("databases/", views.database_list, name="database_list"),
    path("databases/add/", views.database_create, name="database_create"),
    path("databases/<int:pk>/edit/", views.database_edit, name="database_edit"),
    path("databases/<int:pk>/delete/", views.database_delete, name="database_delete"),
    # ETL CRUD
    path("etls/", views.etl_list, name="etl_list"),
    path("etls/add/", views.etl_create, name="etl_create"),
    path("etls/<int:pk>/edit/", views.etl_edit, name="etl_edit"),
    path("etls/<int:pk>/delete/", views.etl_delete, name="etl_delete"),
    # File Download
    path("download/etls/", views.download_etls, name="download_etls"),
    # Test Connection
    path("test-connection/", views.test_connection, name="test_connection"),
    # Toggle ETL
    path("etl/<int:pk>/toggle/", views.toggle_etl_status, name="toggle_etl_status"),
    # Get all tables
    path("etl/get-tables/<int:db_id>/", views.get_tables, name="get_tables"),
    # ETL status api url
    path("etl/status/", views.etl_status_api, name="etl_status_api"),
]
