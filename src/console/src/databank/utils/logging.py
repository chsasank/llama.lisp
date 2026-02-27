def log_with_etl(logger, message, etl_config=None, level="info", extra=None):
    attributes = {}

    if etl_config:
        attributes.update({
            "etl_id": str(etl_config.id),
            "source_table": etl_config.source_table,
            "target_table": etl_config.target_table,
        })

    if extra:
        attributes.update(extra)

    getattr(logger, level)(message, extra=attributes)