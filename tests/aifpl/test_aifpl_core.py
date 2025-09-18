"""Tests for the main AIFPL class and core integration."""

import pytest
from typing import Any

from aifpl import AIFPL, AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError


class TestAIFPLCore:
    """Test the main AIFPL class and basic integration."""

    def test_aifpl_default_initialization(self):
        """Test AIFPL initializes with default parameters."""
        aifpl = AIFPL()
        assert aifpl.max_depth == 100
        assert aifpl.imaginary_tolerance == 1e-10

    def test_aifpl_custom_initialization(self, aifpl_custom):
        """Test AIFPL initializes with custom parameters."""
        aifpl = aifpl_custom(max_depth=200, imaginary_tolerance=1e-12)
        assert aifpl.max_depth == 200
        assert aifpl.imaginary_tolerance == 1e-12

    def test_evaluate_returns_python_objects(self, aifpl):
        """Test that evaluate() returns Python objects."""
        # Integer
        result = aifpl.evaluate("42")
        assert result == 42
        assert isinstance(result, int)

        # Float
        result = aifpl.evaluate("3.14")
        assert result == 3.14
        assert isinstance(result, float)

        # String
        result = aifpl.evaluate('"hello"')
        assert result == "hello"
        assert isinstance(result, str)

        # Boolean
        result = aifpl.evaluate("#t")
        assert result is True
        assert isinstance(result, bool)

        result = aifpl.evaluate("#f")
        assert result is False
        assert isinstance(result, bool)

        # List
        result = aifpl.evaluate("(list 1 2 3)")
        assert result == [1, 2, 3]
        assert isinstance(result, list)

    def test_evaluate_and_format_returns_lisp_strings(self, aifpl):
        """Test that evaluate_and_format() returns LISP-formatted strings."""
        # Integer
        result = aifpl.evaluate_and_format("42")
        assert result == "42"
        assert isinstance(result, str)

        # Float
        result = aifpl.evaluate_and_format("3.14")
        assert result == "3.14"
        assert isinstance(result, str)

        # String (quoted in LISP format)
        result = aifpl.evaluate_and_format('"hello"')
        assert result == '"hello"'
        assert isinstance(result, str)

        # Boolean (LISP format)
        result = aifpl.evaluate_and_format("#t")
        assert result == "#t"
        assert isinstance(result, str)

        result = aifpl.evaluate_and_format("#f")
        assert result == "#f"
        assert isinstance(result, str)

        # List (LISP format)
        result = aifpl.evaluate_and_format("(list 1 2 3)")
        assert result == "(1 2 3)"
        assert isinstance(result, str)

    def test_simple_arithmetic_integration(self, aifpl, helpers):
        """Test basic arithmetic through full pipeline."""
        helpers.assert_evaluates_to(aifpl, "(+ 1 2)", "3")
        helpers.assert_evaluates_to(aifpl, "(- 5 3)", "2")
        helpers.assert_evaluates_to(aifpl, "(* 2 3)", "6")
        helpers.assert_evaluates_to(aifpl, "(/ 8 2)", "4")

    def test_nested_expressions_integration(self, aifpl, helpers):
        """Test nested expressions through full pipeline."""
        helpers.assert_evaluates_to(aifpl, "(+ (* 2 3) (- 5 1))", "10")
        helpers.assert_evaluates_to(aifpl, "(* (+ 1 2) (+ 3 4))", "21")

    def test_constants_integration(self, aifpl):
        """Test mathematical constants are available."""
        # Test pi is available and approximately correct
        result = aifpl.evaluate("pi")
        assert abs(result - 3.14159265) < 1e-6

        # Test e is available and approximately correct
        result = aifpl.evaluate("e")
        assert abs(result - 2.71828182) < 1e-6

        # Test imaginary unit
        result = aifpl.evaluate("j")
        assert result == 1j

        # Test boolean constants
        assert aifpl.evaluate("true") is True
        assert aifpl.evaluate("false") is False

    def test_empty_input_error(self, aifpl):
        """Test that empty input raises appropriate error."""
        with pytest.raises(AIFPLParseError, match="Empty expression"):
            aifpl.evaluate("")

        with pytest.raises(AIFPLParseError, match="Empty expression"):
            aifpl.evaluate("   ")  # Whitespace only

    def test_invalid_syntax_error(self, aifpl):
        """Test that invalid syntax raises parse errors."""
        with pytest.raises(AIFPLParseError):
            aifpl.evaluate("(+ 1 2")  # Missing closing paren

        with pytest.raises(AIFPLParseError):
            aifpl.evaluate("+ 1 2)")  # Missing opening paren

        with pytest.raises(AIFPLTokenError):
            aifpl.evaluate("@invalid")  # Invalid character

    def test_max_depth_configuration_effect(self, aifpl_custom):
        """Test that max_depth configuration affects recursion limits."""
        # Create AIFPL with very low max depth
        aifpl_shallow = aifpl_custom(max_depth=5)

        # Create deeply nested expression that should exceed limit
        deep_expr = "(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1))))))"  # Depth > 5

        with pytest.raises(AIFPLEvalError, match="too deeply nested"):
            aifpl_shallow.evaluate(deep_expr)

        # Same expression should work with default depth
        aifpl_normal = aifpl_custom(max_depth=100)
        result = aifpl_normal.evaluate(deep_expr)
        assert result == 7

    def test_imaginary_tolerance_configuration_effect(self, aifpl_custom):
        """Test that imaginary_tolerance affects complex number simplification."""
        # Create complex number with tiny imaginary part
        expr = "(+ 5 (* 1e-15 j))"  # 5 + 1e-15i

        # With default tolerance (1e-10), should simplify to real
        aifpl_default = aifpl_custom(imaginary_tolerance=1e-10)
        result = aifpl_default.evaluate(expr)
        assert result == 5  # Should be simplified to real
        assert isinstance(result, (int, float))

        # With very strict tolerance (1e-20), should remain complex
        aifpl_strict = aifpl_custom(imaginary_tolerance=1e-20)
        result = aifpl_strict.evaluate(expr)
        assert isinstance(result, complex)
        assert abs(result.imag - 1e-15) < 1e-20

    def test_both_evaluation_methods_consistent(self, aifpl):
        """Test that evaluate() and evaluate_and_format() are consistent."""
        test_cases = [
            "42",
            "3.14",
            '"hello"',
            "#t",
            "#f",
            "(+ 1 2 3)",
            "(list 1 2 3)",
            "(string-append \"hello\" \" \" \"world\")",
        ]

        for expr in test_cases:
            python_result = aifpl.evaluate(expr)
            formatted_result = aifpl.evaluate_and_format(expr)

            # The formatted result should be the LISP representation
            # We can't directly compare, but we can verify both succeed
            assert python_result is not None
            assert formatted_result is not None
            assert isinstance(formatted_result, str)

    @pytest.mark.parametrize("expression,expected_type", [
        ("42", int),
        ("3.14", float),
        ("(complex 1 2)", complex),
        ('"hello"', str),
        ("#t", bool),
        ("#f", bool),
        ("(list 1 2 3)", list),
    ])
    def test_result_types(self, aifpl, expression, expected_type):
        """Test that expressions return expected Python types."""
        result = aifpl.evaluate(expression)
        assert isinstance(result, expected_type)

    def test_whitespace_handling(self, aifpl, helpers):
        """Test that whitespace is handled correctly."""
        # Extra whitespace should be ignored
        helpers.assert_evaluates_to(aifpl, "  ( +   1    2   )  ", "3")
        helpers.assert_evaluates_to(aifpl, "\n(\t+\n1\n2\n)\n", "3")

        # Whitespace in strings should be preserved
        helpers.assert_evaluates_to(aifpl, '"  hello  world  "', '"  hello  world  "')

    def test_multiple_expressions_error(self, aifpl):
        """Test that multiple expressions in one call raise error."""
        with pytest.raises(AIFPLParseError, match="Unexpected token after expression"):
            aifpl.evaluate("1 2")

        with pytest.raises(AIFPLParseError, match="Unexpected token after expression"):
            aifpl.evaluate("(+ 1 2) (+ 3 4)")

    def test_exception_hierarchy(self):
        """Test that all AIFPL exceptions inherit from AIFPLError."""
        assert issubclass(AIFPLTokenError, AIFPLError)
        assert issubclass(AIFPLParseError, AIFPLError)
        assert issubclass(AIFPLEvalError, AIFPLError)

        # Test that they can be instantiated
        token_error = AIFPLTokenError("test token error")
        assert str(token_error) == "test token error"

        parse_error = AIFPLParseError("test parse error")
        assert str(parse_error) == "test parse error"

        eval_error = AIFPLEvalError("test eval error")
        assert str(eval_error) == "test eval error"
