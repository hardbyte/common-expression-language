import rustycel
import pytest


def test_rustycel():

    assert rustycel.evaluate("1 + 2") == 3
    assert rustycel.evaluate("null") is None
    assert rustycel.evaluate("true") == True
    assert rustycel.evaluate("!true") == False


def test_invalid_expression():
    with pytest.raises(ValueError):
        rustycel.evaluate("1invalidprogram")
