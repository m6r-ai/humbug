"""
Comprehensive tests for the Menai bytecode compiler and VM.

This test suite validates:
1. Correct bytecode execution across all language features
2. Error handling and error messages
3. Edge cases and corner cases
"""

import pytest
from menai import Menai
from menai.menai_error import MenaiEvalError


@pytest.fixture
def menai():
    """Fixture that provides an Menai instance."""
    return Menai()


class TestBasicArithmetic:
    """Test basic arithmetic operations."""

    def test_simple_addition(self, menai):
        assert menai.evaluate("(integer+ 1 2)") == 3

    def test_multiple_addition(self, menai):
        assert menai.evaluate("(integer+ 1 2 3 4 5)") == 15

    def test_subtraction(self, menai):
        assert menai.evaluate("(integer- 10 3)") == 7

    def test_multiplication(self, menai):
        assert menai.evaluate("(integer* 2 3 4)") == 24

    def test_division(self, menai):
        assert menai.evaluate("(float/ 12.0 3.0)") == 4.0

    def test_nested_arithmetic(self, menai):
        assert menai.evaluate("(integer+ (integer* 2 3) (integer- 10 5))") == 11


class TestConditionals:
    """Test conditional operations."""

    def test_if_true(self, menai):
        assert menai.evaluate("(if #t 42 99)") == 42

    def test_if_false(self, menai):
        assert menai.evaluate("(if #f 42 99)") == 99

    def test_if_with_condition(self, menai):
        assert menai.evaluate("(if (integer>? 10 5) 1 0)") == 1

    def test_nested_if(self, menai):
        assert menai.evaluate("(if (integer>? 5 3) (if (integer<? 2 4) 1 2) 3)") == 1


class TestLet:
    """Test let binding operations."""

    def test_simple_let(self, menai):
        assert menai.evaluate("(let ((x 5)) x)") == 5

    def test_let_with_operation(self, menai):
        assert menai.evaluate("(let ((x 5) (y 10)) (integer+ x y))") == 15

    def test_let_with_expression(self, menai):
        assert menai.evaluate("(let ((x (integer+ 2 3))) (integer* x 2))") == 10

    def test_nested_let(self, menai):
        result = menai.evaluate("(let ((x 5)) (let ((y 10)) (integer+ x y)))")
        assert result == 15


class TestLambda:
    """Test lambda function operations."""

    def test_simple_lambda(self, menai):
        assert menai.evaluate("((lambda (x) (integer* x x)) 5)") == 25

    def test_lambda_with_multiple_params(self, menai):
        assert menai.evaluate("((lambda (x y) (integer+ x y)) 3 4)") == 7

    def test_lambda_in_let(self, menai):
        result = menai.evaluate("""
            (let ((square (lambda (x) (integer* x x))))
              (square 6))
        """)
        assert result == 36

    def test_recursive_lambda(self, menai):
        """Test recursive functions (critical test!)"""
        result = menai.evaluate("""
            (letrec ((factorial (lambda (n)
                (if (integer=? n 0)
                    1
                    (integer* n (factorial (integer- n 1)))))))
              (factorial 5))
        """)
        assert result == 120


class TestLists:
    """Test list operations."""

    def test_list_creation(self, menai):
        assert menai.evaluate("(list 1 2 3)") == [1, 2, 3]

    def test_first(self, menai):
        assert menai.evaluate("(list-first (list 1 2 3))") == 1

    def test_rest(self, menai):
        assert menai.evaluate("(list-rest (list 1 2 3))") == [2, 3]

    def test_cons(self, menai):
        assert menai.evaluate("(list-prepend (list 2 3) 1)") == [1, 2, 3]

    def test_append(self, menai):
        assert menai.evaluate("(list-concat (list 1 2) (list 3 4))") == [1, 2, 3, 4]

    def test_length(self, menai):
        assert menai.evaluate("(list-length (list 1 2 3))") == 3


class TestHigherOrder:
    """Test higher-order function operations."""

    def test_map(self, menai):
        result = menai.evaluate("(map-list (lambda (x) (integer* x 2)) (list 1 2 3))")
        assert result == [2, 4, 6]

    def test_filter(self, menai):
        result = menai.evaluate("(filter-list (lambda (x) (integer>? x 2)) (list 1 2 3 4))")
        assert result == [3, 4]

    def test_fold(self, menai):
        result = menai.evaluate("(fold-list integer+ 0 (list 1 2 3 4))")
        assert result == 10


class TestStrings:
    """Test string operations."""

    def test_string_append(self, menai):
        assert menai.evaluate('(string-concat "hello" " " "world")') == "hello world"

    def test_string_length(self, menai):
        assert menai.evaluate('(string-length "hello")') == 5

    def test_string_upcase(self, menai):
        assert menai.evaluate('(string-upcase "hello")') == "HELLO"


class TestDicts:
    """Test dict operations."""

    def test_dict_creation(self, menai):
        result = menai.evaluate('(dict (list "name" "Alice") (list "age" 30))')
        # Result is an dict, check it's dict-like
        assert isinstance(result, dict)
        assert result["name"] == "Alice"
        assert result["age"] == 30

    def test_dict_get(self, menai):
        result = menai.evaluate('(dict-get (dict (list "name" "Alice")) "name")')
        assert result == "Alice"


class TestErrors:
    """Test error handling - this is critical!"""

    def test_division_by_zero(self, menai):
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(integer/ 1 0)")
        assert "zero" in str(exc_info.value).lower()

    def test_undefined_variable(self, menai):
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("undefined_var")
        assert "undefined" in str(exc_info.value).lower()

    def test_wrong_arity(self, menai):
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("((lambda (x) x) 1 2)")
        # Should mention arity/argument mismatch
        error_msg = str(exc_info.value).lower()
        assert "expect" in error_msg or "argument" in error_msg

    def test_type_error(self, menai):
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(integer+ 1 "string")')
        # Should mention type error
        error_msg = str(exc_info.value).lower()
        assert "number" in error_msg or "numeric" in error_msg or "type" in error_msg or "integer" in error_msg


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
