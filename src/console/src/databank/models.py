from django.db import models


# Create your models here.
class DatabaseConfiguration(models.Model):
    class ETLType(models.TextChoices):
        SOURCE = "source"
        TARGET = "target"

    class DBTypes(models.TextChoices):
        POSTGRES = "postgres"
        CLICKHOUSE = "clickhouse"
        MSSQL = "mssql"
        ORACLE = "oracle"

    etl_type = models.CharField(choices=ETLType.choices)
    database_type = models.CharField(choices=DBTypes.choices)
    connection_config = models.JSONField()

    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    def __str__(self):
        host = self.connection_config.get("host")
        port = self.connection_config.get("port")

        name = self.connection_config.get("database") or self.connection_config.get(
            "user"
        )

        return f"#{self.id} • {name} ({self.database_type.upper()}) • [{host}:{port}]"


class ETLConfiguration(models.Model):

    class Status(models.TextChoices):
        ACTIVE = "active"
        PAUSED = "paused"

    source_database = models.ForeignKey(
        DatabaseConfiguration, on_delete=models.CASCADE, related_name="source"
    )
    target_database = models.ForeignKey(
        DatabaseConfiguration, on_delete=models.CASCADE, related_name="target"
    )

    source_table = models.CharField()
    target_table = models.CharField()
    replication_key = models.CharField(null=True, blank=True)
    replication_state = models.JSONField(null=True, blank=True)

    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    status = models.CharField(
        max_length=10, choices=Status.choices, default=Status.ACTIVE
    )

    run_interval = models.FloatField(null=True, blank=True)

    class ReplicationMode(models.TextChoices):
        INCREMENTAL = "incremental"
        FULL_REFRESH = "full_refresh"
        ONE_TIME = "one_time"

    replication_mode = models.CharField(
        max_length=20,
        choices=ReplicationMode.choices,
        default=ReplicationMode.INCREMENTAL,
    )
