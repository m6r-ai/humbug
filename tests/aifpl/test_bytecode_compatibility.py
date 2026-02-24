"""
Comprehensive tests for the AIFPL bytecode compiler and VM.

This test suite validates:
1. Correct bytecode execution across all language features
2. Error handling and error messages
3. Edge cases and corner cases
"""

import pytest
from aifpl import AIFPL
from aifpl.aifpl_error import AIFPLEvalError


@pytest.fixture
def aifpl():
    """Fixture that provides an AIFPL instance."""
    return AIFPL()


class TestBasicArithmetic:
    """Test basic arithmetic operations."""

    def test_simple_addition(self, aifpl):
        assert aifpl.evaluate("(integer+ 1 2)") == 3

    def test_multiple_addition(self, aifpl):
        assert aifpl.evaluate("(integer+ 1 2 3 4 5)") == 15

    def test_subtraction(self, aifpl):
        assert aifpl.evaluate("(integer- 10 3)") == 7

    def test_multiplication(self, aifpl):
        assert aifpl.evaluate("(integer* 2 3 4)") == 24

    def test_division(self, aifpl):
        assert aifpl.evaluate("(float/ 12.0 3.0)") == 4.0

    def test_nested_arithmetic(self, aifpl):
        assert aifpl.evaluate("(integer+ (integer* 2 3) (integer- 10 5))") == 11


class TestConditionals:
    """Test conditional operations."""

    def test_if_true(self, aifpl):
        assert aifpl.evaluate("(if #t 42 99)") == 42

    def test_if_false(self, aifpl):
        assert aifpl.evaluate("(if #f 42 99)") == 99

    def test_if_with_condition(self, aifpl):
        assert aifpl.evaluate("(if (integer>? 10 5) 1 0)") == 1

    def test_nested_if(self, aifpl):
        assert aifpl.evaluate("(if (integer>? 5 3) (if (integer<? 2 4) 1 2) 3)") == 1


class TestLet:
    """Test let binding operations."""

    def test_simple_let(self, aifpl):
        assert aifpl.evaluate("(let ((x 5)) x)") == 5

    def test_let_with_operation(self, aifpl):
        assert aifpl.evaluate("(let ((x 5) (y 10)) (integer+ x y))") == 15

    def test_let_with_expression(self, aifpl):
        assert aifpl.evaluate("(let ((x (integer+ 2 3))) (integer* x 2))") == 10

    def test_nested_let(self, aifpl):
        result = aifpl.evaluate("(let ((x 5)) (let ((y 10)) (integer+ x y)))")
        assert result == 15


class TestLambda:
    """Test lambda function operations."""

    def test_simple_lambda(self, aifpl):
        assert aifpl.evaluate("((lambda (x) (integer* x x)) 5)") == 25

    def test_lambda_with_multiple_params(self, aifpl):
        assert aifpl.evaluate("((lambda (x y) (integer+ x y)) 3 4)") == 7

    def test_lambda_in_let(self, aifpl):
        result = aifpl.evaluate("""
            (let ((square (lambda (x) (integer* x x))))
              (square 6))
        """)
        assert result == 36

    def test_recursive_lambda(self, aifpl):
        """Test recursive functions (critical test!)"""
        result = aifpl.evaluate("""
            (letrec ((factorial (lambda (n)
                (if (integer=? n 0)
                    1
                    (integer* n (factorial (integer- n 1)))))))
              (factorial 5))
        """)
        assert result == 120


class TestLists:
    """Test list operations."""

    def test_list_creation(self, aifpl):
        assert aifpl.evaluate("(list 1 2 3)") == [1, 2, 3]

    def test_first(self, aifpl):
        assert aifpl.evaluate("(list-first (list 1 2 3))") == 1

    def test_rest(self, aifpl):
        assert aifpl.evaluate("(list-rest (list 1 2 3))") == [2, 3]

    def test_cons(self, aifpl):
        assert aifpl.evaluate("(list-cons 1 (list 2 3))") == [1, 2, 3]

    def test_append(self, aifpl):
        assert aifpl.evaluate("(list-append (list 1 2) (list 3 4))") == [1, 2, 3, 4]

    def test_length(self, aifpl):
        assert aifpl.evaluate("(list-length (list 1 2 3))") == 3


class TestHigherOrder:
    """Test higher-order function operations."""

    def test_map(self, aifpl):
        result = aifpl.evaluate("(map (lambda (x) (integer* x 2)) (list 1 2 3))")
        assert result == [2, 4, 6]

    def test_filter(self, aifpl):
        result = aifpl.evaluate("(filter (lambda (x) (integer>? x 2)) (list 1 2 3 4))")
        assert result == [3, 4]

    def test_fold(self, aifpl):
        result = aifpl.evaluate("(fold integer+ 0 (list 1 2 3 4))")
        assert result == 10


class TestStrings:
    """Test string operations."""

    def test_string_append(self, aifpl):
        assert aifpl.evaluate('(string-append "hello" " " "world")') == "hello world"

    def test_string_length(self, aifpl):
        assert aifpl.evaluate('(string-length "hello")') == 5

    def test_string_upcase(self, aifpl):
        assert aifpl.evaluate('(string-upcase "hello")') == "HELLO"


class TestALists:
    """Test alist operations."""

    def test_alist_creation(self, aifpl):
        result = aifpl.evaluate('(alist (list "name" "Alice") (list "age" 30))')
        # Result is an alist, check it's dict-like
        assert isinstance(result, dict)
        assert result["name"] == "Alice"
        assert result["age"] == 30

    def test_alist_get(self, aifpl):
        result = aifpl.evaluate('(alist-get (alist (list "name" "Alice")) "name")')
        assert result == "Alice"


class TestErrors:
    """Test error handling - this is critical!"""

    def test_division_by_zero(self, aifpl):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(integer/ 1 0)")
        assert "zero" in str(exc_info.value).lower()

    def test_undefined_variable(self, aifpl):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("undefined_var")
        assert "undefined" in str(exc_info.value).lower()

    def test_wrong_arity(self, aifpl):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("((lambda (x) x) 1 2)")
        # Should mention arity/argument mismatch
        error_msg = str(exc_info.value).lower()
        assert "expect" in error_msg or "argument" in error_msg

    def test_type_error(self, aifpl):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(integer+ 1 "string")')
        # Should mention type error
        error_msg = str(exc_info.value).lower()
        assert "number" in error_msg or "numeric" in error_msg or "type" in error_msg or "integer" in error_msg


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
