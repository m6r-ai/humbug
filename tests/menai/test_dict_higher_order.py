"""Tests for Menai dict higher-order functions: map-dict and filter-dict."""

import pytest
from menai import Menai, MenaiEvalError


@pytest.fixture
def tool():
    """Create Menai instance for testing."""
    return Menai()


class TestDictMap:
    """Tests for map-dict."""

    def test_map_transform_values(self, tool):
        """Test basic value transformation."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) (integer* v 2))'
            ' (dict "a" 1 "b" 2 "c" 3))'
        )
        assert result == {"a": 2, "b": 4, "c": 6}

    def test_map_uses_key_in_result(self, tool):
        """Test that the function receives the key and can use it."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) (string-concat k "=" (integer->string v)))'
            ' (dict "a" 1 "b" 2))'
        )
        assert result == {"a": "a=1", "b": "b=2"}

    def test_map_keys_unchanged(self, tool):
        """Test that keys are not modified by map-dict."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) (integer-neg v))'
            ' (dict "x" 10 "y" 20))'
        )
        assert list(result.keys()) == ["x", "y"]

    def test_map_preserves_insertion_order(self, tool):
        """Test that map-dict preserves key insertion order."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) v)'
            ' (dict "z" 1 "a" 2 "m" 3))'
        )
        assert list(result.keys()) == ["z", "a", "m"]

    def test_map_empty_dict(self, tool):
        """Test map-dict on an empty dict returns empty dict."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) v) (dict))'
        )
        assert result == {}

    def test_map_single_entry(self, tool):
        """Test map-dict on a single-entry dict."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) (integer+ v 100))'
            ' (dict "only" 1))'
        )
        assert result == {"only": 101}

    def test_map_returns_dict(self, tool):
        """Test that map-dict returns a dict, not a list."""
        result = tool.evaluate(
            '(dict? (map-dict (lambda (k v) v)'
            ' (dict "a" 1)))'
        )
        assert result is True

    def test_map_identity(self, tool):
        """Test that identity function leaves dict values unchanged."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) v)'
            ' (dict "a" 1 "b" 2 "c" 3))'
        )
        assert result == {"a": 1, "b": 2, "c": 3}

    def test_map_change_value_type(self, tool):
        """Test that map-dict can change the type of values."""
        result = tool.evaluate(
            '(map-dict (lambda (k v) (integer->string v))'
            ' (dict "a" 1 "b" 2))'
        )
        assert result == {"a": "1", "b": "2"}

    def test_map_first_class(self, tool):
        """Test that map-dict can be passed as a first-class function."""
        result = tool.evaluate(
            '(let ((double-vals (lambda (al) (map-dict (lambda (k v) (integer* v 2)) al))))'
            ' (double-vals (dict "a" 3 "b" 4)))'
        )
        assert result == {"a": 6, "b": 8}


class TestDictFilter:
    """Tests for filter-dict."""

    def test_filter_by_value(self, tool):
        """Test filtering entries by value."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) (integer=? (integer% v 2) 0))'
            ' (dict "a" 1 "b" 2 "c" 3 "d" 4))'
        )
        assert result == {"b": 2, "d": 4}

    def test_filter_by_key(self, tool):
        """Test filtering entries by key."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) (string>=? k "b"))'
            ' (dict "a" 1 "b" 2 "c" 3))'
        )
        assert result == {"b": 2, "c": 3}

    def test_filter_by_key_and_value(self, tool):
        """Test filtering using both key and value."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) (string=? k (integer->string v)))'
            ' (dict "1" 1 "2" 99 "3" 3))'
        )
        assert result == {"1": 1, "3": 3}

    def test_filter_keep_all(self, tool):
        """Test filter with always-true predicate keeps all entries."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) #t)'
            ' (dict "a" 1 "b" 2))'
        )
        assert result == {"a": 1, "b": 2}

    def test_filter_keep_none(self, tool):
        """Test filter with always-false predicate returns empty dict."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) #f)'
            ' (dict "a" 1 "b" 2))'
        )
        assert result == {}

    def test_filter_empty_dict(self, tool):
        """Test filter-dict on an empty dict returns empty dict."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) #t) (dict))'
        )
        assert result == {}

    def test_filter_single_entry_kept(self, tool):
        """Test filter-dict on a single-entry dict where entry is kept."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) #t) (dict "only" 42))'
        )
        assert result == {"only": 42}

    def test_filter_single_entry_removed(self, tool):
        """Test filter-dict on a single-entry dict where entry is removed."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) #f) (dict "only" 42))'
        )
        assert result == {}

    def test_filter_preserves_insertion_order(self, tool):
        """Test that filter-dict preserves key insertion order of kept entries."""
        result = tool.evaluate(
            '(filter-dict (lambda (k v) (integer>? v 1))'
            ' (dict "z" 3 "a" 1 "m" 2))'
        )
        assert list(result.keys()) == ["z", "m"]

    def test_filter_returns_dict(self, tool):
        """Test that filter-dict returns a dict, not a list."""
        result = tool.evaluate(
            '(dict? (filter-dict (lambda (k v) #t)'
            ' (dict "a" 1)))'
        )
        assert result is True

    def test_filter_first_class(self, tool):
        """Test that filter-dict can be passed as a first-class function."""
        result = tool.evaluate(
            '(let ((keep-positives (lambda (al)'
            '         (filter-dict (lambda (k v) (integer>? v 0)) al))))'
            ' (keep-positives (dict "a" 1 "b" -1 "c" 2)))'
        )
        assert result == {"a": 1, "c": 2}


class TestDictMapFilterComposition:
    """Tests for composing map-dict and filter-dict."""

    def test_filter_then_map(self, tool):
        """Test filtering then mapping."""
        result = tool.evaluate(
            '(let ((data (dict "a" 1 "b" 2 "c" 3 "d" 4)))'
            ' (map-dict (lambda (k v) (integer* v 10))'
            '   (filter-dict (lambda (k v) (integer=? (integer% v 2) 0)) data)))'
        )
        assert result == {"b": 20, "d": 40}

    def test_map_then_filter(self, tool):
        """Test mapping then filtering."""
        result = tool.evaluate(
            '(let ((data (dict "a" 1 "b" 2 "c" 3)))'
            ' (filter-dict (lambda (k v) (integer>? v 4))'
            '   (map-dict (lambda (k v) (integer* v 2)) data)))'
        )
        assert result == {"c": 6}
