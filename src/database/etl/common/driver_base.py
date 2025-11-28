class SourceDriver:
    def __init__(self, config):
        self.config = config

    def get_columns(self):
        raise NotImplementedError

    def stream_rows(self, last_ts, last_msn, batch_size):
        raise NotImplementedError


class TargetDriver:
    def __init__(self, config):
        self.config = config

    def ensure_schema(self, columns):
        raise NotImplementedError

    def load_batch(self, columns, rows):
        raise NotImplementedError
