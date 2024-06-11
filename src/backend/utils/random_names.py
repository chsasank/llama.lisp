import random
import string


def random_label(prefix="", length=10):
    """
    Return a random string of the form `prefix_XXXXXXXXXX`
    """
    return (
        prefix
        + "_"
        + "".join([random.choice(string.ascii_letters) for i in range(length)])
    )
