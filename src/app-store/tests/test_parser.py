from app_store.parser import parse_sexp


def test_basic():
    sexp = parse_sexp(
        """(
            hello (world) how (are ("you"))
        )"""
    )
    assert sexp == ["hello", ["world"], "how", ["are", ["you"]]]


def test_unquote():
    sexp = parse_sexp(
        """(
            hello ,(world) ,how (are ("you"))
        )"""
    )
    assert ["hello", ["unquote", ["world"]], ["unquote", "how"], ["are", ["you"]]]


if __name__ == "__main__":
    test_basic()
    test_unquote()
