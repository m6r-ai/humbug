"""Test variadic wrapper functions for primitive operations."""

import pytest
from aifpl import AIFPL
from aifpl.aifpl_error import AIFPLEvalError


class TestPrimitiveWrappers:
    """Test that primitive operations work correctly as first-class values."""

    @pytest.fixture
    def aifpl(self):
        """Create AIFPL instance for testing."""
        return AIFPL()

    def test_wrapper_variadic_calls(self, aifpl):
        """Test that primitive wrappers accept variadic arguments."""
        # Test via let binding
        assert aifpl.evaluate("(let ((add integer+)) (add 1 2))") == 3
        assert aifpl.evaluate("(let ((add integer+)) (add 1 2 3))") == 6
        assert aifpl.evaluate("(let ((add integer+)) (add 1 2 3 4))") == 10

        # Test via lambda
        assert aifpl.evaluate("((lambda (f) (f 1 2)) integer+)") == 3
        assert aifpl.evaluate("((lambda (f) (f 1 2 3)) integer+)") == 6
        assert aifpl.evaluate("((lambda (f) (f 1 2 3 4)) integer+)") == 10

    def test_wrapper_in_tail_position(self, aifpl):
        """Test that primitive wrappers work in tail call position."""
        # Tail position - should not raise "Cannot call native function"
        result = aifpl.evaluate("((lambda (f) (f 1 2 3 4)) integer+)")
        assert result == 10

        # Tail position with subtraction
        result = aifpl.evaluate("((lambda (f) (f 10 3 2)) integer-)")
        assert result == 5  # (10 - 3) - 2 = 5

        # Tail position with multiplication
        result = aifpl.evaluate("((lambda (f) (f 2 3 4)) integer*)")
        assert result == 24

        # Tail position with division (using floats for float/)
        result = aifpl.evaluate("((lambda (f) (f 24.0 2.0 3.0)) float/)")
        assert result == 4.0  # (24.0 / 2.0) / 3.0 = 4.0

    def test_wrapper_with_higher_order_functions(self, aifpl):
        """Test that primitive wrappers work with higher-order functions."""
        # fold with integer+
        assert aifpl.evaluate("(fold integer+ 0 (list 1 2 3 4))") == 10

        # fold with integer*
        assert aifpl.evaluate("(fold integer* 1 (list 2 3 4))") == 24

        # map with lambda that uses integer+
        assert aifpl.evaluate("(map (lambda (x) (integer+ x 10)) (list 1 2 3))") == [11, 12, 13]

    def test_wrapper_special_cases(self, aifpl):
        """Test special cases like zero-arg and single-arg."""
        # Zero-arg cases
        assert aifpl.evaluate("(let ((add integer+)) (add))") == 0
        assert aifpl.evaluate("(let ((mul integer*)) (mul))") == 1

        # Single-arg cases
        assert aifpl.evaluate("(let ((add integer+)) (add 5))") == 5   # Identity
        assert aifpl.evaluate("(let ((mul integer*)) (mul 5))") == 5   # Identity

        # integer/ requires exactly 2 args — single arg should error
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(integer/ 5)")

    def test_wrapper_error_messages(self, aifpl):
        """Test that wrappers give good error messages."""
        # Type error - should mention the operation name or "number"
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(let ((add integer+)) (add 1 "hello"))')
        error_msg = str(exc_info.value)
        assert "integer+" in error_msg or "number" in error_msg.lower()

        # Division by zero
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(let ((div float/)) (div 10.0 0.0))")

    def test_wrapper_all_operations(self, aifpl):
        """Test that all primitive operations work as first-class values."""
        # Addition
        assert aifpl.evaluate("((lambda (op) (op 1 2 3)) integer+)") == 6

        # Subtraction
        assert aifpl.evaluate("((lambda (op) (op 10 3)) integer-)") == 7

        # Multiplication
        assert aifpl.evaluate("((lambda (op) (op 2 3 4)) integer*)") == 24

        # Division (integer->float)
        assert aifpl.evaluate("((lambda (op) (op 12.0 3.0)) float/)") == 4.0

    def test_wrapper_nested_usage(self, aifpl):
        """Test wrappers in nested contexts."""
        # Wrapper in nested lambda
        result = aifpl.evaluate("""
            (let ((apply-op (lambda (op a b c) (op a b c))))
              (apply-op integer+ 1 2 3))
        """)
        assert result == 6

        # Multiple integer wrappers (float/ excluded due to type mismatch with integers)
        result = aifpl.evaluate("""
            (let ((ops (list integer+ integer- integer*)))
              (map (lambda (op) (op 10 2)) ops))
        """)
        assert result == [12, 8, 20]
