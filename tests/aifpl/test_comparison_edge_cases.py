"""Tests for comparison operator edge cases and missing coverage."""

import pytest

from aifpl import AIFPLEvalError


class TestComparisonEdgeCases:
    """Test comparison operators edge cases."""

    def test_inequality_all_equal_case(self, aifpl):
        """Test != operator when all arguments are equal (should return False)."""
        # This specifically tests the missing line 190 in _builtin_bang_eq
        result = aifpl.evaluate("(integer!=? 5 5 5)")
        assert result is False

        result = aifpl.evaluate("(float!=? 1.5 1.5)")
        assert result is False

        result = aifpl.evaluate('(string!=? "test" "test" "test")')
        assert result is False

        result = aifpl.evaluate("(boolean!=? #t #t)")
        assert result is False

        # Test with many equal arguments
        result = aifpl.evaluate("(integer!=? 42 42 42 42 42)")
        assert result is False

    def test_inequality_mixed_cases(self, aifpl):
        """Test != operator with mixed equal and unequal arguments."""
        # These should return True (some arguments are different)
        result = aifpl.evaluate("(integer!=? 1 2 1)")
        assert result is True

        result = aifpl.evaluate("(integer!=? 5 5 6)")
        assert result is True

        result = aifpl.evaluate('(string!=? "a" "b" "a")')
        assert result is True

    def test_comparison_operators_complex_rejection(self, aifpl):
        """Test that typed ordered comparison operators reject complex numbers."""
        complex_expressions = [
            "(complex 1 2)",
            "1j",
            "(complex+ (complex 1 0) 1j)",
            "(complex* (complex 2 0) 1j)"
        ]

        integer_ops = ["integer<?", "integer>?", "integer<=?", "integer>=?"]
        float_ops   = ["float<?",   "float>?",   "float<=?",   "float>=?"]

        for op in integer_ops:
            for complex_expr in complex_expressions:
                with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*complex"):
                    aifpl.evaluate(f"({op} {complex_expr} 5)")

                with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*complex"):
                    aifpl.evaluate(f"({op} 5 {complex_expr})")

        for op in float_ops:
            for complex_expr in complex_expressions:
                with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*complex"):
                    aifpl.evaluate(f"({op} {complex_expr} 1.0)")

                with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*complex"):
                    aifpl.evaluate(f"({op} 1.0 {complex_expr})")

    def test_comparison_operators_type_errors(self, aifpl):
        """Test that typed ordered comparison operators reject wrong types."""
        # integer ops reject floats, strings, and booleans
        for op in ("integer<?", "integer>?", "integer<=?", "integer>=?"):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*float"):
                aifpl.evaluate(f"({op} 1 2.0)")

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*string"):
                aifpl.evaluate(f'({op} "hello" 5)')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires integer arguments.*boolean"):
                aifpl.evaluate(f"({op} #t 1)")

        # float ops reject integers, strings, and booleans
        for op in ("float<?", "float>?", "float<=?", "float>=?"):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*integer"):
                aifpl.evaluate(f"({op} 1.0 2)")

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires float arguments.*string"):
                aifpl.evaluate(f'({op} "hello" 1.0)')

        # string ops reject integers and booleans
        for op in ("string<?", "string>?", "string<=?", "string>=?"):
            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires string arguments.*integer"):
                aifpl.evaluate(f'({op} "a" 1)')

            with pytest.raises(AIFPLEvalError, match=f"{op}.*requires string arguments.*boolean"):
                aifpl.evaluate(f'({op} #t "a")')

    def test_comparison_operators_minimum_arguments(self, aifpl):
        """Test that typed ordered comparison operators require at least 2 arguments."""
        all_typed_ops = [
            "integer<?", "integer>?", "integer<=?", "integer>=?",
            "float<?",   "float>?",   "float<=?",   "float>=?",
            "string<?",  "string>?",  "string<=?",  "string>=?",
        ]

        for op in all_typed_ops:
            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f"({op})")

            with pytest.raises(AIFPLEvalError, match=f"{op}.*has wrong number of arguments"):
                aifpl.evaluate(f"({op} 1)" if op.startswith("integer") else
                               f"({op} 1.0)" if op.startswith("float") else
                               f'({op} "a")')

    def test_comparison_chains_early_termination(self, aifpl):
        """Test that variadic comparison chains terminate early when a condition fails."""
        # integer<? chain: first pair fails immediately
        result = aifpl.evaluate("(integer<? 5 3 10)")
        assert result is False

        # integer<? chain: second pair fails
        result = aifpl.evaluate("(integer<? 1 2 1)")
        assert result is False

        # integer>? chain: first pair fails immediately
        result = aifpl.evaluate("(integer>? 3 5 1)")
        assert result is False

        # integer>? chain: second pair fails
        result = aifpl.evaluate("(integer>? 5 4 6)")
        assert result is False

        # integer<=? chain: first pair fails immediately
        result = aifpl.evaluate("(integer<=? 5 3 10)")
        assert result is False

        # integer>=? chain: first pair fails immediately
        result = aifpl.evaluate("(integer>=? 3 5 1)")
        assert result is False

        # float<? chain
        result = aifpl.evaluate("(float<? 5.0 3.0 10.0)")
        assert result is False

        # string<? chain
        result = aifpl.evaluate('(string<? "b" "a" "c")')
        assert result is False

    def test_successful_comparison_chains(self, aifpl):
        """Test variadic comparison chains that succeed (return True at the end)."""
        result = aifpl.evaluate("(integer<? 1 2 3 4)")
        assert result is True

        result = aifpl.evaluate("(integer>? 10 8 5 2)")
        assert result is True

        result = aifpl.evaluate("(integer<=? 1 2 2 3)")
        assert result is True

        result = aifpl.evaluate("(integer>=? 10 8 8 5)")
        assert result is True

        result = aifpl.evaluate("(float<? 1.0 2.0 3.0)")
        assert result is True

        result = aifpl.evaluate("(float>=? 3.0 2.0 2.0 1.0)")
        assert result is True

        result = aifpl.evaluate('(string<? "apple" "banana" "cherry")')
        assert result is True

        result = aifpl.evaluate("(integer=? 5 5 5 5)")
        assert result is True
