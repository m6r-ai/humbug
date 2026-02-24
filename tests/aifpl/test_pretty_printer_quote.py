"""Tests for quote preservation bug fix in compact lists.

This test file verifies that the bug where quotes were lost in compact
(single-line) lists has been fixed. The bug was in _try_compact_list()
which called _format_atom() instead of _format_expression(), causing
QUOTE tokens to be silently dropped.
"""

import pytest

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


class TestQuotePreservationInCompactLists:
    """Test that quotes are preserved in compact (single-line) lists."""

    def test_quoted_symbols_in_list(self):
        """Test that quoted symbols in lists are preserved."""
        printer = AIFPLPrettyPrinter()
        code = "(list 'a 'b 'c)"
        result = printer.format(code)

        # All quotes should be preserved
        assert "'a" in result
        assert "'b" in result
        assert "'c" in result
        assert result.strip() == "(list 'a 'b 'c)"

    def test_quoted_symbols_in_match_clauses(self):
        """Test that quoted symbols in match clauses are preserved."""
        printer = AIFPLPrettyPrinter()
        code = "(match x (1 'one) (2 'two) (_ 'other))"
        result = printer.format(code)

        # All quotes should be preserved
        assert "'one" in result
        assert "'two" in result
        assert "'other" in result

    def test_quoted_lists_in_compact_context(self):
        """Test that quoted lists are preserved."""
        printer = AIFPLPrettyPrinter()
        code = "(list '(a b) '(c d))"
        result = printer.format(code)

        assert "'(a b)" in result
        assert "'(c d)" in result

    def test_mixed_quoted_and_unquoted(self):
        """Test mixing quoted and unquoted expressions."""
        printer = AIFPLPrettyPrinter()
        code = "(list 1 'a 2 'b 3)"
        result = printer.format(code)

        assert "'a" in result
        assert "'b" in result
        assert "1" in result
        assert "2" in result
        assert "3" in result

    def test_nested_quotes_in_compact_list(self):
        """Test nested quoted expressions in compact lists."""
        printer = AIFPLPrettyPrinter()
        code = "(list 'x '(a b c) 'y)"
        result = printer.format(code)

        assert "'x" in result
        assert "'(a b c)" in result
        assert "'y" in result

    def test_quotes_in_function_arguments(self):
        """Test quotes as function arguments."""
        printer = AIFPLPrettyPrinter()
        code = "(list-prepend 'first 'rest)"
        result = printer.format(code)

        assert "'first" in result
        assert "'rest" in result

    def test_quotes_in_if_branches_compact(self):
        """Test quotes in if branches (when they would be compact)."""
        printer = AIFPLPrettyPrinter()
        # Note: if always uses multiline, but branches might be compact
        code = "(if #t 'yes 'no)"
        result = printer.format(code)

        assert "'yes" in result
        assert "'no" in result

    def test_quote_with_empty_list(self):
        """Test quoting empty list."""
        printer = AIFPLPrettyPrinter()
        code = "(list '() 'x)"
        result = printer.format(code)

        assert "'()" in result
        assert "'x" in result

    def test_multiple_levels_of_nesting_with_quotes(self):
        """Test quotes at multiple nesting levels."""
        printer = AIFPLPrettyPrinter()
        code = "(list (list 'a 'b) (list 'c 'd))"
        result = printer.format(code)

        assert "'a" in result
        assert "'b" in result
        assert "'c" in result
        assert "'d" in result

    def test_quote_vs_quote_form(self):
        """Test that both ' and (quote ...) work."""
        printer = AIFPLPrettyPrinter()

        # Using ' shorthand
        code1 = "(list 'a 'b)"
        result1 = printer.format(code1)
        assert "'a" in result1
        assert "'b" in result1

        # Using (quote ...) form
        code2 = "(list (quote a) (quote b))"
        result2 = printer.format(code2)
        assert "quote" in result2

    def test_idempotence_with_quotes(self):
        """Test that formatting with quotes is idempotent."""
        printer = AIFPLPrettyPrinter()
        code = "(list 'a 'b 'c)"

        result1 = printer.format(code)
        result2 = printer.format(result1)

        # Should be identical
        assert result1 == result2
        # And should preserve quotes
        assert "'a" in result2
        assert "'b" in result2
        assert "'c" in result2


class TestQuotePreservationEdgeCases:
    """Test edge cases for quote preservation."""

    def test_quote_at_start_of_list(self):
        """Test quote as first element in list."""
        printer = AIFPLPrettyPrinter()
        code = "('a b c)"
        result = printer.format(code)
        assert "'a" in result

    def test_quote_at_end_of_list(self):
        """Test quote as last element in list."""
        printer = AIFPLPrettyPrinter()
        code = "(list a b 'c)"
        result = printer.format(code)
        assert "'c" in result

    def test_only_quoted_elements(self):
        """Test list with only quoted elements."""
        printer = AIFPLPrettyPrinter()
        code = "(list 'a 'b 'c 'd 'e)"
        result = printer.format(code)

        for letter in ['a', 'b', 'c', 'd', 'e']:
            assert f"'{letter}" in result

    def test_quoted_boolean_values(self):
        """Test quoting boolean values."""
        printer = AIFPLPrettyPrinter()
        code = "(list '#t '#f)"
        result = printer.format(code)

        assert "'#t" in result
        assert "'#f" in result

    def test_quoted_numbers(self):
        """Test quoting numbers (unusual but valid)."""
        printer = AIFPLPrettyPrinter()
        code = "(list '42 '3.14)"
        result = printer.format(code)

        assert "'42" in result
        assert "'3.14" in result

    def test_deeply_nested_quoted_lists(self):
        """Test deeply nested quoted list structures."""
        printer = AIFPLPrettyPrinter()
        code = "(list '(a (b (c d))))"
        result = printer.format(code)

        assert "'(a (b (c d)))" in result

    def test_quote_in_cond_style_match(self):
        """Test quotes in match expressions that look like cond."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  ((> x 0) 'positive)
  ((< x 0) 'negative)
  (_ 'zero))"""
        result = printer.format(code)

        assert "'positive" in result
        assert "'negative" in result
        assert "'zero" in result
