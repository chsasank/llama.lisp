import random
import string


def random_label(prefix="", *extra_prefixes, length=10):
    """
    Syntax: random_label(prefix1, [prefix2, prefix3 ... ], [length = 10])
    Return random string(s) of the form `prefix_XXXXXXXXXX`
    If multiple prefixes are given, return a list with one string for each prefix,
    all with the same random suffix
    If a one or zero prefixes are given, return a single string
    """
    labels = [
        (
            prefix
            + "_"
            + "".join([random.choice(string.ascii_letters) for i in range(length)])
        )
        for prefix in [prefix, *extra_prefixes]
    ]
    if extra_prefixes:
        return labels
    else:
        return labels[0]
