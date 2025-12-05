from django import forms
from .models import DatabaseConfiguration, ETLConfiguration

class DatabaseConfigurationForm(forms.ModelForm):
    class Meta:
        model = DatabaseConfiguration
        fields = ["etl_type", "database_type", "connection_config"]


class ETLConfigurationForm(forms.ModelForm):
    class Meta:
        model = ETLConfiguration
        fields = [
            "source_database",
            "target_database",
            "source_table",
            "target_table",
            "replication_key",
            "replication_state",
        ]
