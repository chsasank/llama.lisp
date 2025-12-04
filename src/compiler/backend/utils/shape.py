def verify_shape(obj, template):
    """
    Verify the shape of `lst` with `template`.
    For example, ["set", "a", 5] has shape [str, str, int].
    `None` can be used as a wildcard; so ["set", "a", ["add", 5, 1]] matches [str, str, None]
    """
    if template is None:
        # Wildcard pattern
        return True
    elif isinstance(template, type):
        # Verify object type
        return isinstance(obj, template)
    elif isinstance(template, list):
        # Recursively verify children
        if not isinstance(obj, list):
            return False
        if len(obj) != len(template):
            return False
        for c in range(len(obj)):
            if not verify_shape(obj[c], template[c]):
                return False
    else:
        # Verify object value
        return obj == template
    return True
