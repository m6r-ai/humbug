"""Tests for equality comparisons that verify metadata doesn't affect equality.

This module tests that AIFPLValue equality comparisons (using = operator and
Python's __eq__) compare only the value content and ignore source location
metadata (line, column).

This is critical for pattern matching where literal patterns created by the
desugarer may have different metadata than runtime values.
"""

import pytest

from aifpl import AIFPL
from aifpl.aifpl_value import AIFPLString, AIFPLBoolean, AIFPLInteger


class TestEquality:
    """Test that equality ignores metadata fields."""

    def test_equals_operator_with_strings(self, aifpl):
        """Test that = operator works with strings."""
        # The = operator uses Python's __eq__ internally
        result = aifpl.evaluate('(string=? "hello" "hello")')
        assert result is True

        result = aifpl.evaluate('(string=? "hello" "world")')
        assert result is False

        # Multiple strings
        result = aifpl.evaluate('(string=? "test" "test" "test")')
        assert result is True

        result = aifpl.evaluate('(string=? "a" "a" "b")')
        assert result is False

    def test_equals_operator_with_booleans(self, aifpl):
        """Test that = operator works with booleans."""
        result = aifpl.evaluate('(boolean=? #t #t)')
        assert result is True

        result = aifpl.evaluate('(boolean=? #f #f)')
        assert result is True

        result = aifpl.evaluate('(boolean=? #t #f)')
        assert result is False

        # Multiple booleans
        result = aifpl.evaluate('(boolean=? #t #t #t)')
        assert result is True

        result = aifpl.evaluate('(boolean=? #f #f #f)')
        assert result is True

    def test_not_equals_operator_with_strings(self, aifpl):
        """Test that != operator works with strings."""
        result = aifpl.evaluate('(string!=? "hello" "world")')
        assert result is True

        result = aifpl.evaluate('(string!=? "hello" "hello")')
        assert result is False

        result = aifpl.evaluate('(string!=? "a" "b" "c")')
        assert result is True

    def test_not_equals_operator_with_booleans(self, aifpl):
        """Test that != operator works with booleans."""
        result = aifpl.evaluate('(boolean!=? #t #f)')
        assert result is True

        result = aifpl.evaluate('(boolean!=? #t #t)')
        assert result is False

    def test_string_in_list_operations(self, aifpl):
        """Test that strings work correctly in list operations (uses __eq__)."""
        # member? uses __eq__ internally
        result = aifpl.evaluate('(list-member? "hello" (list "hello" "world"))')
        assert result is True

        result = aifpl.evaluate('(list-member? "test" (list "hello" "world"))')
        assert result is False

        # position uses __eq__ internally
        result = aifpl.evaluate('(list-position "world" (list "hello" "world" "test"))')
        assert result == 1

        result = aifpl.evaluate('(list-position "missing" (list "hello" "world"))')
        assert result is False

    def test_boolean_in_list_operations(self, aifpl):
        """Test that booleans work correctly in list operations."""
        result = aifpl.evaluate('(list-member? #t (list #t #f))')
        assert result is True

        result = aifpl.evaluate('(list-member? #t (list #f #f))')
        assert result is False

        result = aifpl.evaluate('(list-position #f (list #t #f #t))')
        assert result == 1
