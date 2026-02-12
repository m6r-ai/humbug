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


class TestEqualityWithMetadata:
    """Test that equality ignores metadata fields."""

    def test_string_equality_ignores_metadata(self):
        """Test that string equality ignores line/column metadata."""
        # Create two strings with same value but different metadata
        str1 = AIFPLString("hello", line=1, column=5)
        str2 = AIFPLString("hello", line=10, column=20)
        str3 = AIFPLString("hello")  # No metadata
        str4 = AIFPLString("world", line=1, column=5)

        # Same value should be equal regardless of metadata
        assert str1 == str2
        assert str1 == str3
        assert str2 == str3

        # Different values should not be equal
        assert str1 != str4
        assert str2 != str4

    def test_boolean_equality_ignores_metadata(self):
        """Test that boolean equality ignores line/column metadata."""
        # Create booleans with same value but different metadata
        bool1 = AIFPLBoolean(True, line=1, column=5)
        bool2 = AIFPLBoolean(True, line=10, column=20)
        bool3 = AIFPLBoolean(True)  # No metadata
        bool4 = AIFPLBoolean(False, line=1, column=5)

        # Same value should be equal regardless of metadata
        assert bool1 == bool2
        assert bool1 == bool3
        assert bool2 == bool3

        # Different values should not be equal
        assert bool1 != bool4
        assert bool2 != bool4

    def test_string_hash_ignores_metadata(self):
        """Test that string hashing ignores metadata."""
        str1 = AIFPLString("test", line=1, column=5)
        str2 = AIFPLString("test", line=10, column=20)
        str3 = AIFPLString("test")

        # Same value should have same hash
        assert hash(str1) == hash(str2)
        assert hash(str1) == hash(str3)

        # Can be used as dict keys
        d = {str1: "value1"}
        assert d[str2] == "value1"  # str2 should work as same key
        assert d[str3] == "value1"  # str3 should work as same key

    def test_boolean_hash_ignores_metadata(self):
        """Test that boolean hashing ignores metadata."""
        bool1 = AIFPLBoolean(True, line=1, column=5)
        bool2 = AIFPLBoolean(True, line=10, column=20)
        bool3 = AIFPLBoolean(True)

        # Same value should have same hash
        assert hash(bool1) == hash(bool2)
        assert hash(bool1) == hash(bool3)

        # Can be used as dict keys
        d = {bool1: "value1"}
        assert d[bool2] == "value1"
        assert d[bool3] == "value1"

    def test_match_string_literal_with_metadata(self, aifpl):
        """Test that match works with string literals (the original bug)."""
        # This was failing before the fix because the desugarer creates
        # string literals with metadata that differs from runtime values
        result = aifpl.evaluate_and_format(
            '(match "hello" ("hello" "matched") (_ "no match"))'
        )
        assert result == '"matched"'

        result = aifpl.evaluate_and_format(
            '(match "world" ("hello" "wrong") ("world" "correct") (_ "no match"))'
        )
        assert result == '"correct"'

    def test_match_boolean_literal_with_metadata(self, aifpl):
        """Test that match works with boolean literals."""
        result = aifpl.evaluate_and_format(
            '(match #t (#t "true") (#f "false"))'
        )
        assert result == '"true"'

        result = aifpl.evaluate_and_format(
            '(match #f (#t "true") (#f "false"))'
        )
        assert result == '"false"'

    def test_equals_operator_with_strings(self, aifpl):
        """Test that = operator works with strings."""
        # The = operator uses Python's __eq__ internally
        result = aifpl.evaluate('(= "hello" "hello")')
        assert result is True

        result = aifpl.evaluate('(= "hello" "world")')
        assert result is False

        # Multiple strings
        result = aifpl.evaluate('(= "test" "test" "test")')
        assert result is True

        result = aifpl.evaluate('(= "a" "a" "b")')
        assert result is False

    def test_equals_operator_with_booleans(self, aifpl):
        """Test that = operator works with booleans."""
        result = aifpl.evaluate('(= #t #t)')
        assert result is True

        result = aifpl.evaluate('(= #f #f)')
        assert result is True

        result = aifpl.evaluate('(= #t #f)')
        assert result is False

        # Multiple booleans
        result = aifpl.evaluate('(= #t #t #t)')
        assert result is True

        result = aifpl.evaluate('(= #f #f #f)')
        assert result is True

    def test_not_equals_operator_with_strings(self, aifpl):
        """Test that != operator works with strings."""
        result = aifpl.evaluate('(!= "hello" "world")')
        assert result is True

        result = aifpl.evaluate('(!= "hello" "hello")')
        assert result is False

        result = aifpl.evaluate('(!= "a" "b" "c")')
        assert result is True

    def test_not_equals_operator_with_booleans(self, aifpl):
        """Test that != operator works with booleans."""
        result = aifpl.evaluate('(!= #t #f)')
        assert result is True

        result = aifpl.evaluate('(!= #t #t)')
        assert result is False

    def test_string_in_list_operations(self, aifpl):
        """Test that strings work correctly in list operations (uses __eq__)."""
        # member? uses __eq__ internally
        result = aifpl.evaluate('(member? "hello" (list "hello" "world"))')
        assert result is True

        result = aifpl.evaluate('(member? "test" (list "hello" "world"))')
        assert result is False

        # position uses __eq__ internally
        result = aifpl.evaluate('(position "world" (list "hello" "world" "test"))')
        assert result == 1

        result = aifpl.evaluate('(position "missing" (list "hello" "world"))')
        assert result is False

    def test_boolean_in_list_operations(self, aifpl):
        """Test that booleans work correctly in list operations."""
        result = aifpl.evaluate('(member? #t (list #t #f))')
        assert result is True

        result = aifpl.evaluate('(member? #t (list #f #f))')
        assert result is False

        result = aifpl.evaluate('(position #f (list #t #f #t))')
        assert result == 1
