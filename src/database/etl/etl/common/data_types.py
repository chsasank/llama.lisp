from enum import Enum


class ETLDataTypes(Enum):
    INTEGER = 1
    FLOAT = 2
    BOOLEAN = 3

    STRING = 4
    BYTES = 5
    JSON = 6

    DATE_TIME = 7
    DATE = 8
    TIME = 9
    TIME_INTERVAL = 10

    LIST = 11
