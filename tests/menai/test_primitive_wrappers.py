"""Test variadic wrapper functions for primitive operations."""

import pytest
from menai import Menai
from menai.menai_error import MenaiEvalError


class TestPrimitiveWrappers:
    """Test that primitive operations work correctly as first-class values."""

    @pytest.fixture
    def menai(self):
        """Create Menai instance for testing."""
        return Menai()

    def test_wrapper_variadic_calls(self, menai):
        """Test that primitive wrappers accept variadic arguments."""
        # Test via let binding
        assert menai.evaluate("(let ((add integer+)) (add 1 2))") == 3
        assert menai.evaluate("(let ((add integer+)) (add 1 2 3))") == 6
        assert menai.evaluate("(let ((add integer+)) (add 1 2 3 4))") == 10

        # Test via lambda
        assert menai.evaluate("((lambda (f) (f 1 2)) integer+)") == 3
        assert menai.evaluate("((lambda (f) (f 1 2 3)) integer+)") == 6
        assert menai.evaluate("((lambda (f) (f 1 2 3 4)) integer+)") == 10

    def test_wrapper_in_tail_position(self, menai):
        """Test that primitive wrappers work in tail call position."""
        # Tail position - should not raise "Cannot call native function"
        result = menai.evaluate("((lambda (f) (f 1 2 3 4)) integer+)")
        assert result == 10

        # Tail position with subtraction
        result = menai.evaluate("((lambda (f) (f 10 3 2)) integer-)")
        assert result == 5  # (10 - 3) - 2 = 5

        # Tail position with multiplication
        result = menai.evaluate("((lambda (f) (f 2 3 4)) integer*)")
        assert result == 24

        # Tail position with division (using floats for float/)
        result = menai.evaluate("((lambda (f) (f 24.0 2.0 3.0)) float/)")
        assert result == 4.0  # (24.0 / 2.0) / 3.0 = 4.0

    def test_wrapper_with_higher_order_functions(self, menai):
        """Test that primitive wrappers work with higher-order functions."""
        # fold with integer+
        assert menai.evaluate("(fold-list integer+ 0 (list 1 2 3 4))") == 10

        # fold with integer*
        assert menai.evaluate("(fold-list integer* 1 (list 2 3 4))") == 24

        # map with lambda that uses integer+
        assert menai.evaluate("(map-list (lambda (x) (integer+ x 10)) (list 1 2 3))") == [11, 12, 13]

    def test_wrapper_special_cases(self, menai):
        """Test special cases like zero-arg and single-arg."""
        # Zero-arg cases
        assert menai.evaluate("(let ((add integer+)) (add))") == 0
        assert menai.evaluate("(let ((mul integer*)) (mul))") == 1

        # Single-arg cases
        assert menai.evaluate("(let ((add integer+)) (add 5))") == 5   # Identity
        assert menai.evaluate("(let ((mul integer*)) (mul 5))") == 5   # Identity

        # integer/ requires exactly 2 args — single arg should error
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer/ 5)")

    def test_wrapper_error_messages(self, menai):
        """Test that wrappers give good error messages."""
        # Type error - should mention the operation name or "number"
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(let ((add integer+)) (add 1 "hello"))')
        error_msg = str(exc_info.value)
        assert "integer+" in error_msg or "number" in error_msg.lower()

        # Division by zero
        with pytest.raises(MenaiEvalError, match="Division by zero"):
            menai.evaluate("(let ((div float/)) (div 10.0 0.0))")

    def test_wrapper_all_operations(self, menai):
        """Test that all primitive operations work as first-class values."""
        # Addition
        assert menai.evaluate("((lambda (op) (op 1 2 3)) integer+)") == 6

        # Subtraction
        assert menai.evaluate("((lambda (op) (op 10 3)) integer-)") == 7

        # Multiplication
        assert menai.evaluate("((lambda (op) (op 2 3 4)) integer*)") == 24

        # Division (integer->float)
        assert menai.evaluate("((lambda (op) (op 12.0 3.0)) float/)") == 4.0

    def test_wrapper_nested_usage(self, menai):
        """Test wrappers in nested contexts."""
        # Wrapper in nested lambda
        result = menai.evaluate("""
            (let ((apply-op (lambda (op a b c) (op a b c))))
              (apply-op integer+ 1 2 3))
        """)
        assert result == 6

        # Multiple integer wrappers (float/ excluded due to type mismatch with integers)
        result = menai.evaluate("""
            (let ((ops (list integer+ integer- integer*)))
              (map-list (lambda (op) (op 10 2)) ops))
        """)
        assert result == [12, 8, 20]
