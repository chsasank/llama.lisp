import json

from tinydb import Query, TinyDB


class StateManagerDriver:
    def __init__(self):
        assert self.replication_key is not None

    def set_state(self, replication_value):
        raise NotImplementedError

    def get_state(self):
        raise NotImplementedError


class JSONStateManager(StateManagerDriver):
    def __init__(self, json_path, state_id, replication_key):
        self.state_id = state_id
        self.replication_key = replication_key
        self.json_path = json_path
        self.db = TinyDB(self.json_path)
        super().__init__()

    def set_state(self, replication_value):
        State = Query()
        try:
            json.dumps(replication_value)
        except TypeError:
            replication_value = str(replication_value)

        self.db.upsert(
            {
                "state_id": self.state_id,
                "replication_key": self.replication_key,
                "replication_value": replication_value,
            },
            State.state_id == self.state_id,
        )

    def get_state(self):
        State = Query()
        states = self.db.search(State.state_id == self.state_id)
        assert len(states) < 2, (
            f"Unexpected number of states for state_id={self.state_id}"
        )
        if len(states) == 0:
            return None
        else:
            return states[0]["replication_value"]
