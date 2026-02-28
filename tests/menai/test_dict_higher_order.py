"""Tests for Menai dict higher-order functions: dict-map and dict-filter."""

import pytest
from menai import Menai, MenaiEvalError


@pytest.fixture
def tool():
    """Create Menai instance for testing."""
    return Menai()


class TestDictMap:
    """Tests for dict-map."""

    def test_map_transform_values(self, tool):
        """Test basic value transformation."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) (integer* v 2))'
            ' (dict (list "a" 1) (list "b" 2) (list "c" 3)))'
        )
        assert result == {"a": 2, "b": 4, "c": 6}

    def test_map_uses_key_in_result(self, tool):
        """Test that the function receives the key and can use it."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) (string-concat k "=" (integer->string v)))'
            ' (dict (list "a" 1) (list "b" 2)))'
        )
        assert result == {"a": "a=1", "b": "b=2"}

    def test_map_keys_unchanged(self, tool):
        """Test that keys are not modified by dict-map."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) (integer-neg v))'
            ' (dict (list "x" 10) (list "y" 20)))'
        )
        assert list(result.keys()) == ["x", "y"]

    def test_map_preserves_insertion_order(self, tool):
        """Test that dict-map preserves key insertion order."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) v)'
            ' (dict (list "z" 1) (list "a" 2) (list "m" 3)))'
        )
        assert list(result.keys()) == ["z", "a", "m"]

    def test_map_empty_dict(self, tool):
        """Test dict-map on an empty dict returns empty dict."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) v) (dict))'
        )
        assert result == {}

    def test_map_single_entry(self, tool):
        """Test dict-map on a single-entry dict."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) (integer+ v 100))'
            ' (dict (list "only" 1)))'
        )
        assert result == {"only": 101}

    def test_map_returns_dict(self, tool):
        """Test that dict-map returns an dict, not a list."""
        result = tool.evaluate(
            '(dict? (dict-map (lambda (k v) v)'
            ' (dict (list "a" 1))))'
        )
        assert result is True

    def test_map_identity(self, tool):
        """Test that identity function leaves dict values unchanged."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) v)'
            ' (dict (list "a" 1) (list "b" 2) (list "c" 3)))'
        )
        assert result == {"a": 1, "b": 2, "c": 3}

    def test_map_change_value_type(self, tool):
        """Test that dict-map can change the type of values."""
        result = tool.evaluate(
            '(dict-map (lambda (k v) (integer->string v))'
            ' (dict (list "a" 1) (list "b" 2)))'
        )
        assert result == {"a": "1", "b": "2"}

    def test_map_first_class(self, tool):
        """Test that dict-map can be passed as a first-class function."""
        result = tool.evaluate(
            '(let ((double-vals (lambda (al) (dict-map (lambda (k v) (integer* v 2)) al))))'
            ' (double-vals (dict (list "a" 3) (list "b" 4))))'
        )
        assert result == {"a": 6, "b": 8}


class TestDictFilter:
    """Tests for dict-filter."""

    def test_filter_by_value(self, tool):
        """Test filtering entries by value."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) (integer=? (integer% v 2) 0))'
            ' (dict (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4)))'
        )
        assert result == {"b": 2, "d": 4}

    def test_filter_by_key(self, tool):
        """Test filtering entries by key."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) (string>=? k "b"))'
            ' (dict (list "a" 1) (list "b" 2) (list "c" 3)))'
        )
        assert result == {"b": 2, "c": 3}

    def test_filter_by_key_and_value(self, tool):
        """Test filtering using both key and value."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) (string=? k (integer->string v)))'
            ' (dict (list "1" 1) (list "2" 99) (list "3" 3)))'
        )
        assert result == {"1": 1, "3": 3}

    def test_filter_keep_all(self, tool):
        """Test filter with always-true predicate keeps all entries."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) #t)'
            ' (dict (list "a" 1) (list "b" 2)))'
        )
        assert result == {"a": 1, "b": 2}

    def test_filter_keep_none(self, tool):
        """Test filter with always-false predicate returns empty dict."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) #f)'
            ' (dict (list "a" 1) (list "b" 2)))'
        )
        assert result == {}

    def test_filter_empty_dict(self, tool):
        """Test dict-filter on an empty dict returns empty dict."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) #t) (dict))'
        )
        assert result == {}

    def test_filter_single_entry_kept(self, tool):
        """Test dict-filter on a single-entry dict where entry is kept."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) #t) (dict (list "only" 42)))'
        )
        assert result == {"only": 42}

    def test_filter_single_entry_removed(self, tool):
        """Test dict-filter on a single-entry dict where entry is removed."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) #f) (dict (list "only" 42)))'
        )
        assert result == {}

    def test_filter_preserves_insertion_order(self, tool):
        """Test that dict-filter preserves key insertion order of kept entries."""
        result = tool.evaluate(
            '(dict-filter (lambda (k v) (integer>? v 1))'
            ' (dict (list "z" 3) (list "a" 1) (list "m" 2)))'
        )
        assert list(result.keys()) == ["z", "m"]

    def test_filter_returns_dict(self, tool):
        """Test that dict-filter returns an dict, not a list."""
        result = tool.evaluate(
            '(dict? (dict-filter (lambda (k v) #t)'
            ' (dict (list "a" 1))))'
        )
        assert result is True

    def test_filter_first_class(self, tool):
        """Test that dict-filter can be passed as a first-class function."""
        result = tool.evaluate(
            '(let ((keep-positives (lambda (al)'
            '         (dict-filter (lambda (k v) (integer>? v 0)) al))))'
            ' (keep-positives (dict (list "a" 1) (list "b" -1) (list "c" 2))))'
        )
        assert result == {"a": 1, "c": 2}


class TestDictMapFilterComposition:
    """Tests for composing dict-map and dict-filter."""

    def test_filter_then_map(self, tool):
        """Test filtering then mapping."""
        result = tool.evaluate(
            '(let ((data (dict (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4))))'
            ' (dict-map (lambda (k v) (integer* v 10))'
            '   (dict-filter (lambda (k v) (integer=? (integer% v 2) 0)) data)))'
        )
        assert result == {"b": 20, "d": 40}

    def test_map_then_filter(self, tool):
        """Test mapping then filtering."""
        result = tool.evaluate(
            '(let ((data (dict (list "a" 1) (list "b" 2) (list "c" 3))))'
            ' (dict-filter (lambda (k v) (integer>? v 4))'
            '   (dict-map (lambda (k v) (integer* v 2)) data)))'
        )
        assert result == {"c": 6}
