from django.urls import path
from . import views

urlpatterns = [
    path("", views.home, name="home"),
    
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

    # Run ETL (single + all)
    path("run-etl/<int:etl_id>/", views.run_single_etl, name="run_single_etl"),
    path("run-all-etls/", views.run_all_etls, name="run_all_etls"),
]
