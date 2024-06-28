def verify_shape(obj, template):
    """
    Verify the shape of `lst` with `template`.
    For example, ["set", "a", 5] has shape [str, str, int].
    `None` can be used as a wildcard; so ["set", "a", ["add", 5, 1]] matches [str, str, None]
    """
    if template is None:
        return True
    elif not isinstance(template, list):
        return isinstance(obj, template)
    else:
        if not isinstance(obj, list):
            return False
        for c in range(len(obj)):
            if not verify_shape(obj[c], template[c]):
                return False
    return True
