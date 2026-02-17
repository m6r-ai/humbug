"""Tests for comparison operators edge cases and missing coverage."""

import pytest

from aifpl import AIFPLEvalError


class TestComparisonEdgeCases:
    """Test comparison operators edge cases."""

    def test_inequality_all_equal_case(self, aifpl):
        """Test != operator when all arguments are equal (should return False)."""
        # This specifically tests the missing line 190 in _builtin_bang_eq
        result = aifpl.evaluate("(!= 5 5 5)")
        assert result is False

        result = aifpl.evaluate("(!= 1.5 1.5)")
        assert result is False

        result = aifpl.evaluate('(!= "test" "test" "test")')
        assert result is False

        result = aifpl.evaluate("(!= #t #t)")
        assert result is False

        # Test with many equal arguments
        result = aifpl.evaluate("(!= 42 42 42 42 42)")
        assert result is False

    def test_inequality_mixed_cases(self, aifpl):
        """Test != operator with mixed equal and unequal arguments."""
        # These should return True (some arguments are different)
        result = aifpl.evaluate("(!= 1 2 1)")
        assert result is True

        result = aifpl.evaluate("(!= 5 5 6)")
        assert result is True

        result = aifpl.evaluate('(!= "a" "b" "a")')
        assert result is True

    def test_comparison_operators_complex_rejection(self, aifpl):
        """Test that <, >, <=, >= properly reject complex numbers."""
        complex_expressions = [
            "(complex 1 2)",
            "1j",
            "(+ 1 1j)",
            "(* 2 1j)"
        ]

        comparison_ops = ["<", ">", "<=", ">="]

        for op in comparison_ops:
            for complex_expr in complex_expressions:
                # Test complex as first argument
                with pytest.raises(AIFPLEvalError, match=f"Function '{op}' does not support complex numbers"):
                    aifpl.evaluate(f"({op} {complex_expr} 5)")

                # Test complex as second argument
                with pytest.raises(AIFPLEvalError, match=f"Function '{op}' does not support complex numbers"):
                    aifpl.evaluate(f"({op} 5 {complex_expr})")

                # Test complex in middle of chain
                with pytest.raises(AIFPLEvalError, match=f"Function '{op}' does not support complex numbers"):
                    aifpl.evaluate(f"({op} 1 {complex_expr} 10)")

    def test_comparison_chains_with_complex_numbers(self, aifpl):
        """Test comparison chains that include complex numbers."""
        # Test longer chains with complex numbers
        with pytest.raises(AIFPLEvalError, match="Function '<' does not support complex numbers"):
            aifpl.evaluate("(< 1 2 (complex 3 1) 4)")

        with pytest.raises(AIFPLEvalError, match="Function '>' does not support complex numbers"):
            aifpl.evaluate("(> 10 (+ 5 1j) 3 1)")

        with pytest.raises(AIFPLEvalError, match="Function '<=' does not support complex numbers"):
            aifpl.evaluate("(<= 1 1j 3)")

        with pytest.raises(AIFPLEvalError, match="Function '>=' does not support complex numbers"):
            aifpl.evaluate("(>= 1j 0)")

    def test_comparison_operators_type_errors(self, aifpl):
        """Test comparison operators with non-numeric types."""
        numeric_ops = ["<", ">", "<=", ">="]

        for op in numeric_ops:
            # String arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires real number arguments"):
                aifpl.evaluate(f'({op} "hello" 5)')

            # Boolean arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires real number arguments"):
                aifpl.evaluate(f"({op} #t 1)")

            # List arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires real number arguments"):
                aifpl.evaluate(f"({op} (list 1 2) 3)")

    def test_comparison_operators_minimum_arguments(self, aifpl):
        """Test that comparison operators require minimum arguments."""
        all_comparison_ops = ["=", "!=", "<", ">", "<=", ">="]

        for op in all_comparison_ops:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires at least 2 arguments"):
                aifpl.evaluate(f"({op})")

            # One argument
            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires at least 2 arguments"):
                aifpl.evaluate(f"({op} 5)")

    def test_comparison_chains_early_termination(self, aifpl):
        """Test that comparison chains terminate early when condition fails."""
        # These test the early return paths in comparison functions

        # Test < operator early termination
        result = aifpl.evaluate("(< 5 3 10)")  # Should return False immediately
        assert result is False

        result = aifpl.evaluate("(< 1 2 1)")  # Should return False at second comparison
        assert result is False

        # Test > operator early termination
        result = aifpl.evaluate("(> 3 5 1)")  # Should return False immediately
        assert result is False

        result = aifpl.evaluate("(> 5 4 6)")  # Should return False at second comparison
        assert result is False

        # Test <= operator early termination
        result = aifpl.evaluate("(<= 5 3 10)")  # Should return False immediately
        assert result is False

        # Test >= operator early termination
        result = aifpl.evaluate("(>= 3 5 1)")  # Should return False immediately
        assert result is False

    def test_successful_comparison_chains(self, aifpl):
        """Test comparison chains that succeed (don't terminate early)."""
        # These test the success paths that return True at the end

        result = aifpl.evaluate("(< 1 2 3 4)")
        assert result is True

        result = aifpl.evaluate("(> 10 8 5 2)")
        assert result is True

        result = aifpl.evaluate("(<= 1 2 2 3)")
        assert result is True

        result = aifpl.evaluate("(>= 10 8 8 5)")
        assert result is True

        result = aifpl.evaluate("(= 5 5 5 5)")
        assert result is True
