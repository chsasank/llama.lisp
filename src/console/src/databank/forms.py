from django import forms
from .models import DatabaseConfiguration, ETLConfiguration


class DatabaseConfigurationForm(forms.ModelForm):
    class Meta:
        model = DatabaseConfiguration
        fields = ["id", "etl_type", "database_type", "connection_config"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        # Applying the same Tailwind styling to every field in this form
        for field in self.fields.values():
            field.widget.attrs.update(
                {
                    "class": (
                        "w-full rounded-md px-3 py-2 text-sm font-mono "
                        "bg-black/70 border border-consoleBorder text-slate-100 "
                        "hover:border-consoleAccent/60 "
                        "focus:outline-none focus:ring-1 focus:ring-consoleAccent "
                        "focus:border-consoleAccent "
                        "transition duration-150 ease-in-out"
                    )
                }
            )


class ETLConfigurationForm(forms.ModelForm):
    class Meta:
        model = ETLConfiguration
        fields = [
            "source_database",
            "target_database",
            "source_table",
            "target_table",
            "run_interval",
            "replication_key",
            "replication_state",
        ]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        
        self.fields["source_database"].queryset = DatabaseConfiguration.objects.filter(etl_type="source")
        self.fields["target_database"].queryset = DatabaseConfiguration.objects.filter(etl_type="target")

        # Styling
        for field in self.fields.values():
            field.widget.attrs.update(
                {
                    "class": (
                        "w-full rounded-md px-3 py-2 text-sm font-mono "
                        "bg-black/70 border border-consoleBorder text-slate-100 "
                        "hover:border-consoleAccent/60 "
                        "focus:outline-none focus:ring-1 focus:ring-consoleAccent "
                        "focus:border-consoleAccent "
                        "transition duration-150 ease-in-out"
                    )
                }
            )

        if "replication_state" in self.fields:
            # If the field is empty, show empty JSON {} by default
            if self.instance and (
                self.instance.replication_state is None
                or self.instance.replication_state == ""
            ):
                self.fields["replication_state"].initial = {}

            self.fields["replication_state"].widget.attrs.update(
                {
                    "rows": 5,
                    "class": "w-full bg-black/60 border border-consoleBorder rounded px-3 py-2 text-sm "
                    "font-mono text-slate-100 focus:outline-none focus:border-consoleAccent",
                }
            )
