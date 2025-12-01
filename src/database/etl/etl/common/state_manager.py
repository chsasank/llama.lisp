import json
import os

STATE_PATH = "state/state.json"

class StateManager:

    @staticmethod
    def load():
        if not os.path.exists(STATE_PATH):
            return {}
        with open(STATE_PATH, "r") as f:
            return json.load(f)

    @staticmethod
    def save(state):
        with open(STATE_PATH, "w") as f:
            json.dump(state, f, indent=2)
