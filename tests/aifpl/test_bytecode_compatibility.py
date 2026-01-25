"""
Test bytecode compatibility by running representative tests with both interpreter and bytecode.

This helps identify:
1. Functional bugs in bytecode
2. Error message differences
3. Edge cases
"""

import pytest
from aifpl import AIFPL
from aifpl.aifpl_error import AIFPLEvalError


@pytest.fixture(params=[False, True], ids=["interpreter", "bytecode"])
def aifpl_both(request):
    """Fixture that provides both interpreter and bytecode versions."""
    return AIFPL(use_bytecode=request.param)


class TestBasicArithmetic:
    """Test basic arithmetic with both backends."""

    def test_simple_addition(self, aifpl_both):
        assert aifpl_both.evaluate("(+ 1 2)") == 3

    def test_multiple_addition(self, aifpl_both):
        assert aifpl_both.evaluate("(+ 1 2 3 4 5)") == 15

    def test_subtraction(self, aifpl_both):
        assert aifpl_both.evaluate("(- 10 3)") == 7

    def test_multiplication(self, aifpl_both):
        assert aifpl_both.evaluate("(* 2 3 4)") == 24

    def test_division(self, aifpl_both):
        assert aifpl_both.evaluate("(/ 12 3)") == 4.0

    def test_nested_arithmetic(self, aifpl_both):
        assert aifpl_both.evaluate("(+ (* 2 3) (- 10 5))") == 11


class TestConditionals:
    """Test conditionals with both backends."""

    def test_if_true(self, aifpl_both):
        assert aifpl_both.evaluate("(if #t 42 99)") == 42

    def test_if_false(self, aifpl_both):
        assert aifpl_both.evaluate("(if #f 42 99)") == 99

    def test_if_with_condition(self, aifpl_both):
        assert aifpl_both.evaluate("(if (> 10 5) 1 0)") == 1

    def test_nested_if(self, aifpl_both):
        assert aifpl_both.evaluate("(if (> 5 3) (if (< 2 4) 1 2) 3)") == 1


class TestLet:
    """Test let bindings with both backends."""

    def test_simple_let(self, aifpl_both):
        assert aifpl_both.evaluate("(let ((x 5)) x)") == 5

    def test_let_with_operation(self, aifpl_both):
        assert aifpl_both.evaluate("(let ((x 5) (y 10)) (+ x y))") == 15

    def test_let_with_expression(self, aifpl_both):
        assert aifpl_both.evaluate("(let ((x (+ 2 3))) (* x 2))") == 10

    def test_nested_let(self, aifpl_both):
        result = aifpl_both.evaluate("(let ((x 5)) (let ((y 10)) (+ x y)))")
        assert result == 15


class TestLambda:
    """Test lambda functions with both backends."""

    def test_simple_lambda(self, aifpl_both):
        assert aifpl_both.evaluate("((lambda (x) (* x x)) 5)") == 25

    def test_lambda_with_multiple_params(self, aifpl_both):
        assert aifpl_both.evaluate("((lambda (x y) (+ x y)) 3 4)") == 7

    def test_lambda_in_let(self, aifpl_both):
        result = aifpl_both.evaluate("""
            (let ((square (lambda (x) (* x x))))
              (square 6))
        """)
        assert result == 36

    def test_recursive_lambda(self, aifpl_both):
        """Test recursive functions (critical test!)"""
        result = aifpl_both.evaluate("""
            (let ((factorial (lambda (n)
                (if (= n 0)
                    1
                    (* n (factorial (- n 1)))))))
              (factorial 5))
        """)
        assert result == 120


class TestLists:
    """Test list operations with both backends."""

    def test_list_creation(self, aifpl_both):
        assert aifpl_both.evaluate("(list 1 2 3)") == [1, 2, 3]

    def test_first(self, aifpl_both):
        assert aifpl_both.evaluate("(first (list 1 2 3))") == 1

    def test_rest(self, aifpl_both):
        assert aifpl_both.evaluate("(rest (list 1 2 3))") == [2, 3]

    def test_cons(self, aifpl_both):
        assert aifpl_both.evaluate("(cons 1 (list 2 3))") == [1, 2, 3]

    def test_append(self, aifpl_both):
        assert aifpl_both.evaluate("(append (list 1 2) (list 3 4))") == [1, 2, 3, 4]

    def test_length(self, aifpl_both):
        assert aifpl_both.evaluate("(length (list 1 2 3))") == 3


class TestHigherOrder:
    """Test higher-order functions with both backends."""

    def test_map(self, aifpl_both):
        result = aifpl_both.evaluate("(map (lambda (x) (* x 2)) (list 1 2 3))")
        assert result == [2, 4, 6]

    def test_filter(self, aifpl_both):
        result = aifpl_both.evaluate("(filter (lambda (x) (> x 2)) (list 1 2 3 4))")
        assert result == [3, 4]

    def test_fold(self, aifpl_both):
        result = aifpl_both.evaluate("(fold + 0 (list 1 2 3 4))")
        assert result == 10


class TestStrings:
    """Test string operations with both backends."""

    def test_string_append(self, aifpl_both):
        assert aifpl_both.evaluate('(string-append "hello" " " "world")') == "hello world"

    def test_string_length(self, aifpl_both):
        assert aifpl_both.evaluate('(string-length "hello")') == 5

    def test_string_upcase(self, aifpl_both):
        assert aifpl_both.evaluate('(string-upcase "hello")') == "HELLO"


class TestALists:
    """Test alist operations with both backends."""

    def test_alist_creation(self, aifpl_both):
        result = aifpl_both.evaluate('(alist (list "name" "Alice") (list "age" 30))')
        # Result is an alist, check it's dict-like
        assert isinstance(result, dict)
        assert result["name"] == "Alice"
        assert result["age"] == 30

    def test_alist_get(self, aifpl_both):
        result = aifpl_both.evaluate('(alist-get (alist (list "name" "Alice")) "name")')
        assert result == "Alice"


class TestErrors:
    """Test error handling with both backends - this is critical!"""

    def test_division_by_zero(self, aifpl_both):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl_both.evaluate("(/ 1 0)")
        assert "zero" in str(exc_info.value).lower()

    def test_undefined_variable(self, aifpl_both):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl_both.evaluate("undefined_var")
        assert "undefined" in str(exc_info.value).lower()

    def test_wrong_arity(self, aifpl_both):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl_both.evaluate("((lambda (x) x) 1 2)")
        # Should mention arity/argument mismatch
        error_msg = str(exc_info.value).lower()
        assert "expect" in error_msg or "argument" in error_msg

    def test_type_error(self, aifpl_both):
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl_both.evaluate('(+ 1 "string")')
        # Should mention type error
        error_msg = str(exc_info.value).lower()
        assert "number" in error_msg or "numeric" in error_msg or "type" in error_msg


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
