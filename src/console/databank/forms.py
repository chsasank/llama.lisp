from django import forms
from .models import DatabaseConfiguration, ETLConfiguration

class DatabaseConfigurationForm(forms.ModelForm):
    class Meta:
        model = DatabaseConfiguration
        fields = ["etl_type", "database_type", "connection_config"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        # Applying the same Tailwind styling to every field in this form
        for field in self.fields.values():
            field.widget.attrs.update({
                "class": "w-full bg-black/60 border border-consoleBorder rounded px-3 py-2 text-sm "
                         "text-slate-100 focus:outline-none focus:border-consoleAccent"
            })


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

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        # Styling
        for field in self.fields.values():
            field.widget.attrs.update({
                "class": "w-full bg-black/60 border border-consoleBorder rounded px-3 py-2 text-sm "
                         "text-slate-100 focus:outline-none focus:border-consoleAccent"
            })

        # If replication_key is empty in DB, show "Null" in the form
        if self.instance and (self.instance.replication_key in [None, ""]):
            self.fields["replication_key"].initial = "Null"

        if "replication_state" in self.fields:
            # If the field is empty, show empty JSON {} by default
            if self.instance and (self.instance.replication_state is None or self.instance.replication_state == ""):
                self.fields["replication_state"].initial = {}

            self.fields["replication_state"].widget.attrs.update({
                "rows": 5,
                "class": "w-full bg-black/60 border border-consoleBorder rounded px-3 py-2 text-sm "
                         "font-mono text-slate-100 focus:outline-none focus:border-consoleAccent"
            })
