import json
import os

from tinydb import Query, TinyDB

here = os.path.dirname(os.path.realpath(__file__))
db = TinyDB(os.path.join(here, "state.json"))


class StateManager:
    def __init__(self, state_id, replication_key):
        self.state_id = state_id
        self.replication_key = replication_key

    def set_state(self, replication_value):
        State = Query()
        try:
            json.dumps(replication_value)
        except TypeError:
            replication_value = str(replication_value)

        db.upsert(
            {
                "state_id": self.state_id,
                "replication_key": self.replication_key,
                "replication_value": replication_value,
            },
            State.state_id == self.state_id,
        )

    def get_state(self):
        State = Query()
        states = db.search(State.state_id == self.state_id)
        assert (
            len(states) < 2
        ), f"Unexpected number of states for state_id={self.state_id}"
        if len(states) == 0:
            return None
        else:
            return states[0]["replication_value"]
