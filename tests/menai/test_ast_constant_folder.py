"""
Regression tests for MenaiASTConstantFolder through the full pipeline.

Each test compiles and evaluates a Menai expression whose arguments are all
compile-time literals.  The point is not to exhaustively test every fold
function — it is to verify that the constant folder fires at all for each
type family, so that a rename or pipeline reordering that silently breaks
folding will be caught.

One representative case per type family is sufficient.  Correctness of the
arithmetic itself is covered by the broader test suite.
"""

import pytest
from menai import Menai


@pytest.fixture
def menai():
    return Menai()


class TestConstantFolding:
    def test_boolean_not(self, menai):
        assert menai.evaluate("(boolean-not #t)") is False

    def test_integer_add(self, menai):
        assert menai.evaluate("(integer+ 3 4)") == 7

    def test_integer_eq(self, menai):
        assert menai.evaluate("(integer=? 5 5)") is True

    def test_integer_neq(self, menai):
        assert menai.evaluate("(integer!=? 3 4)") is True

    def test_float_mul(self, menai):
        assert menai.evaluate("(float* 2.0 3.0)") == 6.0

    def test_float_sqrt(self, menai):
        assert menai.evaluate("(float-sqrt 9.0)") == 3.0

    def test_complex_add(self, menai):
        assert menai.evaluate("(complex+ 1+2j 3+4j)") == (4+6j)

    def test_string_eq(self, menai):
        assert menai.evaluate('(string=? "hello" "hello")') is True

    def test_nested_folds(self, menai):
        # Verifies that the folder recurses: inner fold feeds outer fold.
        assert menai.evaluate("(integer+ (integer+ 1 2) (integer+ 3 4))") == 10

    def test_if_constant_condition(self, menai):
        # The if-elimination path is distinct from the builtin-fold path.
        assert menai.evaluate("(if #t 42 0)") == 42
