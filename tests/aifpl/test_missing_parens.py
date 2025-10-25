"""Tests for missing parenthesis error detection and reporting."""

import pytest
from aifpl import AIFPL, AIFPLParseError


class TestMissingParens:
    """Test enhanced error messages for missing parentheses."""

    def test_simple_missing_closing_paren(self, aifpl):
        """Test error message for simple missing closing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(+ 1 2")

        error = str(exc_info.value)
        assert "missing 1 closing parenthesis" in error.lower()
        assert "position" in error.lower()

    def test_nested_missing_closing_parens(self, aifpl):
        """Test error message for nested missing closing parens."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(+ (* 2 3) (- 5 1")

        error = str(exc_info.value)
        assert "missing 2 closing" in error.lower()
        assert "unclosed expressions" in error.lower()

    def test_let_with_missing_paren_simple(self, aifpl):
        """Test error for simple let with missing closing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(let ((x 5)) (+ x 2")

        error = str(exc_info.value)
        assert "missing" in error.lower()
        assert "closing" in error.lower()

    def test_let_binding_missing_closing_paren(self, aifpl):
        """Test error when a binding value is missing closing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("""(let (
  (add (lambda (x y) (+ x y)))
  (mul (lambda (x y) (* x y))
  (sub (lambda (x y) (- x y)))
)
  (add 5 3)
)""")

        error = str(exc_info.value)
        # Should mention it's in a let binding context
        assert "let" in error.lower() or "binding" in error.lower()
        # Should give some indication about which binding
        assert "mul" in error.lower() or "binding" in error.lower()

    def test_let_binding_missing_paren_shows_position(self, aifpl):
        """Test that error shows helpful position information."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(let ((a 1) (b (+ 2 3) (c 4)) (+ a b c))")

        error = str(exc_info.value)
        # Should have position information
        assert "position" in error.lower()

    def test_deeply_nested_let_bindings(self, aifpl):
        """Test error reporting for deeply nested structures."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("""(let (
  (x 5)
  (y (let ((a 1) (b 2)) (+ a b))
  (z 10)
)
  (+ x y z)
)""")

        error = str(exc_info.value)
        assert "missing" in error.lower()
        # Should show depth information
        assert "unclosed" in error.lower() or "depth" in error.lower()

    def test_if_expression_missing_paren(self, aifpl):
        """Test error for if expression missing closing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(if (> x 5) \"big\" \"small\"")

        error = str(exc_info.value)
        assert "missing" in error.lower()
        assert "if" in error.lower()

    def test_lambda_missing_closing_paren(self, aifpl):
        """Test error for lambda missing closing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(lambda (x y) (+ x y")

        error = str(exc_info.value)
        assert "missing" in error.lower()
        assert "lambda" in error.lower()

    def test_multiple_missing_parens_shows_count(self, aifpl):
        """Test that error shows correct count of missing parens."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(let ((x 5)) (if (> x 0) (+ x 1")

        error = str(exc_info.value)
        assert "missing 3 closing" in error.lower()

    def test_error_shows_unclosed_expression_list(self, aifpl):
        """Test that error lists all unclosed expressions."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(let ((x 5)) (+ x 2")

        error = str(exc_info.value)
        # Should show both the let and the function call as unclosed
        assert "unclosed expressions" in error.lower()

    def test_error_suggests_closing_parens(self, aifpl):
        """Test that error suggests the closing parens to add."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(+ 1 (+ 2 3")

        error = str(exc_info.value)
        # Should suggest adding ) )
        assert ")" in error

    def test_binding_with_complex_value_missing_paren(self, aifpl):
        """Test error when binding has complex value expression missing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("""(let (
  (x 5)
  (y (+ (* 2 3) (- 10 5))
  (z 7)
)
  (+ x y z)
)""")

        error = str(exc_info.value)
        # Should identify it's in a let binding
        assert "let" in error.lower() or "binding" in error.lower()

    def test_last_complete_position_tracking(self, aifpl):
        """Test that parser tracks where last complete element was."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(let ((x 5) (y 10)) (+ x y")

        error = str(exc_info.value)
        # Should have information about where things are
        assert "position" in error.lower()

    def test_empty_binding_list_missing_paren(self, aifpl):
        """Test error for let with empty bindings missing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(let ( (+ 1 2)")

        error = str(exc_info.value)
        assert "missing" in error.lower()

    def test_single_binding_missing_paren(self, aifpl):
        """Test error for let with single binding missing paren."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("(let ((x 5) (+ x 1))")

        error = str(exc_info.value)
        assert "missing" in error.lower()

    def test_error_message_shows_expression_type(self, aifpl):
        """Test that error identifies the type of expression (let, lambda, if, etc)."""
        test_cases = [
            ("(let ((x 5)) x", "let"),
            ("(lambda (x) x", "lambda"),
            ("(if (> 1 0) 1 0", "if"),
        ]

        for expr, expected_type in test_cases:
            with pytest.raises(AIFPLParseError) as exc_info:
                aifpl.evaluate(expr)

            error = str(exc_info.value)
            assert expected_type in error.lower(), f"Expected '{expected_type}' in error for: {expr}"

    def test_multiple_levels_of_nesting(self, aifpl):
        """Test error reporting with multiple levels of nesting."""
        with pytest.raises(AIFPLParseError) as exc_info:
            aifpl.evaluate("""(let (
  (outer 1)
  (middle (let ((inner 2)) inner)
)
  outer
)""")

        error = str(exc_info.value)
        # Should show nested structure
        assert "unclosed" in error.lower()
        assert "let" in error.lower()


@pytest.fixture
def aifpl():
    """Create a fresh AIFPL instance for each test."""
    return AIFPL()
