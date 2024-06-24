import random
import string


def random_label(prefix="", extra_prefixes=[], length=10):
    """
    Return a random string. If prefix and extra_prefix are given, they will
    be used as prefixes to the random string.
    """
    prefixes = ".".join([*extra_prefixes, prefix])
    suffix = "".join([random.choice(string.ascii_lowercase) for i in range(length)])
    if prefixes:
        return f"{prefixes}.{suffix}"
    else:
        return suffix
