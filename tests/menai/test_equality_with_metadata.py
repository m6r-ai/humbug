"""Tests for equality comparisons that verify metadata doesn't affect equality.

This module tests that MenaiValue equality comparisons (using = operator and
Python's __eq__) compare only the value content and ignore source location
metadata (line, column).

This is critical for pattern matching where literal patterns created by the
desugarer may have different metadata than runtime values.
"""

import pytest

from menai import Menai
from menai.menai_value import MenaiString, MenaiBoolean, MenaiInteger


class TestEquality:
    """Test that equality ignores metadata fields."""

    def test_equals_operator_with_strings(self, menai):
        """Test that = operator works with strings."""
        # The = operator uses Python's __eq__ internally
        result = menai.evaluate('(string=? "hello" "hello")')
        assert result is True

        result = menai.evaluate('(string=? "hello" "world")')
        assert result is False

        # Multiple strings
        result = menai.evaluate('(string=? "test" "test" "test")')
        assert result is True

        result = menai.evaluate('(string=? "a" "a" "b")')
        assert result is False

    def test_equals_operator_with_booleans(self, menai):
        """Test that = operator works with booleans."""
        result = menai.evaluate('(boolean=? #t #t)')
        assert result is True

        result = menai.evaluate('(boolean=? #f #f)')
        assert result is True

        result = menai.evaluate('(boolean=? #t #f)')
        assert result is False

        # Multiple booleans
        result = menai.evaluate('(boolean=? #t #t #t)')
        assert result is True

        result = menai.evaluate('(boolean=? #f #f #f)')
        assert result is True

    def test_not_equals_operator_with_strings(self, menai):
        """Test that != operator works with strings."""
        result = menai.evaluate('(string!=? "hello" "world")')
        assert result is True

        result = menai.evaluate('(string!=? "hello" "hello")')
        assert result is False

        result = menai.evaluate('(string!=? "a" "b" "c")')
        assert result is True

    def test_not_equals_operator_with_booleans(self, menai):
        """Test that != operator works with booleans."""
        result = menai.evaluate('(boolean!=? #t #f)')
        assert result is True

        result = menai.evaluate('(boolean!=? #t #t)')
        assert result is False

    def test_string_in_list_operations(self, menai):
        """Test that strings work correctly in list operations (uses __eq__)."""
        # member? uses __eq__ internally
        result = menai.evaluate('(list-member? (list "hello" "world") "hello")')
        assert result is True

        result = menai.evaluate('(list-member? (list "hello" "world") "test")')
        assert result is False

        # position uses __eq__ internally
        result = menai.evaluate('(list-index (list "hello" "world" "test") "world")')
        assert result == 1

        result = menai.evaluate('(list-index (list "hello" "world") "missing")')
        assert result is None

    def test_boolean_in_list_operations(self, menai):
        """Test that booleans work correctly in list operations."""
        result = menai.evaluate('(list-member? (list #t #f) #t)')
        assert result is True

        result = menai.evaluate('(list-member? (list #f #f) #t)')
        assert result is False

        result = menai.evaluate('(list-index (list #t #f #t) #f)')
        assert result == 1
