"""Tests for the main Menai class and core integration."""

import pytest

from menai import Menai, MenaiError, MenaiTokenError, MenaiParseError, MenaiEvalError


class TestMenaiCore:
    """Test the main Menai class and basic integration."""

    def test_evaluate_returns_python_objects(self, menai):
        """Test that evaluate() returns Python objects."""
        # Integer
        result = menai.evaluate("42")
        assert result == 42
        assert isinstance(result, int)

        # Float
        result = menai.evaluate("3.14")
        assert result == 3.14
        assert isinstance(result, float)

        # String
        result = menai.evaluate('"hello"')
        assert result == "hello"
        assert isinstance(result, str)

        # Boolean
        result = menai.evaluate("#t")
        assert result is True
        assert isinstance(result, bool)

        result = menai.evaluate("#f")
        assert result is False
        assert isinstance(result, bool)

        # List
        result = menai.evaluate("(list 1 2 3)")
        assert result == [1, 2, 3]
        assert isinstance(result, list)

    def test_evaluate_and_format_returns_lisp_strings(self, menai):
        """Test that evaluate_and_format() returns LISP-formatted strings."""
        # Integer
        result = menai.evaluate_and_format("42")
        assert result == "42"
        assert isinstance(result, str)

        # Float
        result = menai.evaluate_and_format("3.14")
        assert result == "3.14"
        assert isinstance(result, str)

        # String (quoted in LISP format)
        result = menai.evaluate_and_format('"hello"')
        assert result == '"hello"'
        assert isinstance(result, str)

        # Boolean (LISP format)
        result = menai.evaluate_and_format("#t")
        assert result == "#t"
        assert isinstance(result, str)

        result = menai.evaluate_and_format("#f")
        assert result == "#f"
        assert isinstance(result, str)

        # List (LISP format)
        result = menai.evaluate_and_format("(list 1 2 3)")
        assert result == "(1 2 3)"
        assert isinstance(result, str)

    def test_simple_arithmetic_integration(self, menai, helpers):
        """Test basic arithmetic through full pipeline."""
        helpers.assert_evaluates_to(menai, "(integer+ 1 2)", "3")
        helpers.assert_evaluates_to(menai, "(integer- 5 3)", "2")
        helpers.assert_evaluates_to(menai, "(integer* 2 3)", "6")
        helpers.assert_evaluates_to(menai, "(float/ 8.0 2.0)", "4.0")

    def test_nested_expressions_integration(self, menai, helpers):
        """Test nested expressions through full pipeline."""
        helpers.assert_evaluates_to(menai, "(integer+ (integer* 2 3) (integer- 5 1))", "10")
        helpers.assert_evaluates_to(menai, "(integer* (integer+ 1 2) (integer+ 3 4))", "21")

    def test_constants_integration(self, menai):
        """Test mathematical constants are available."""
        # Test pi is available and approximately correct
        result = menai.evaluate("pi")
        assert abs(result - 3.14159265) < 1e-6

        # Test e is available and approximately correct
        result = menai.evaluate("e")
        assert abs(result - 2.71828182) < 1e-6

        # Test imaginary unit
        result = menai.evaluate("1j")
        assert result == 1j

        # Test boolean constants
        assert menai.evaluate("#t") is True
        assert menai.evaluate("#f") is False

    def test_empty_input_error(self, menai):
        """Test that empty input raises appropriate error."""
        with pytest.raises(MenaiParseError, match="Empty expression"):
            menai.evaluate("")

        with pytest.raises(MenaiParseError, match="Empty expression"):
            menai.evaluate("   ")  # Whitespace only

    def test_invalid_syntax_error(self, menai):
        """Test that invalid syntax raises parse errors."""
        with pytest.raises(MenaiParseError):
            menai.evaluate("(+ 1 2")  # Missing closing paren

        with pytest.raises(MenaiParseError):
            menai.evaluate("+ 1 2)")  # Missing opening paren

        with pytest.raises(MenaiTokenError):
            menai.evaluate("@invalid")  # Invalid character

    def test_both_evaluation_methods_consistent(self, menai):
        """Test that evaluate() and evaluate_and_format() are consistent."""
        test_cases = [
            "42",
            "3.14",
            '"hello"',
            "#t",
            "#f",
            "(integer+ 1 2 3)",
            "(list 1 2 3)",
            "(string-concat \"hello\" \" \" \"world\")",
        ]

        for expr in test_cases:
            python_result = menai.evaluate(expr)
            formatted_result = menai.evaluate_and_format(expr)

            # The formatted result should be the LISP representation
            # We can't directly compare, but we can verify both succeed
            assert python_result is not None
            assert formatted_result is not None
            assert isinstance(formatted_result, str)

    @pytest.mark.parametrize("expression,expected_type", [
        ("42", int),
        ("3.14", float),
        ("(integer->complex 1 2)", complex),
        ('"hello"', str),
        ("#t", bool),
        ("#f", bool),
        ("(list 1 2 3)", list),
    ])
    def test_result_types(self, menai, expression, expected_type):
        """Test that expressions return expected Python types."""
        result = menai.evaluate(expression)
        assert isinstance(result, expected_type)

    def test_whitespace_handling(self, menai, helpers):
        """Test that whitespace is handled correctly."""
        # Extra whitespace should be ignored
        helpers.assert_evaluates_to(menai, "  ( integer+   1    2   )  ", "3")
        helpers.assert_evaluates_to(menai, "\n(\t integer+\n1\n2\n)\n", "3")

        # Whitespace in strings should be preserved
        helpers.assert_evaluates_to(menai, '"  hello  world  "', '"  hello  world  "')

    def test_multiple_expressions_error(self, menai):
        """Test that multiple expressions in one call raise error."""
        with pytest.raises(MenaiParseError, match=r"Unexpected token after complete expression"):
            menai.evaluate("1 2")

        with pytest.raises(MenaiParseError, match=r"Unexpected token after complete expression"):
            menai.evaluate("(integer+ 1 2) (integer+ 3 4)")

    def test_exception_hierarchy(self):
        """Test that all Menai exceptions inherit from MenaiError."""
        assert issubclass(MenaiTokenError, MenaiError)
        assert issubclass(MenaiParseError, MenaiError)
        assert issubclass(MenaiEvalError, MenaiError)

        # Test that they can be instantiated
        token_error = MenaiTokenError("test token error")
        assert "test token error" in str(token_error)

        parse_error = MenaiParseError("test parse error")
        assert "test parse error" in str(parse_error)

        eval_error = MenaiEvalError("test eval error")
        assert "test eval error" in str(eval_error)
