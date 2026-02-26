"""Tests for AIFPL alist higher-order functions: alist-map and alist-filter."""

import pytest
from aifpl import AIFPL, AIFPLEvalError


@pytest.fixture
def tool():
    """Create AIFPL instance for testing."""
    return AIFPL()


class TestAListMap:
    """Tests for alist-map."""

    def test_map_transform_values(self, tool):
        """Test basic value transformation."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) (integer* v 2))'
            ' (alist (list "a" 1) (list "b" 2) (list "c" 3)))'
        )
        assert result == {"a": 2, "b": 4, "c": 6}

    def test_map_uses_key_in_result(self, tool):
        """Test that the function receives the key and can use it."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) (string-concat k "=" (integer->string v)))'
            ' (alist (list "a" 1) (list "b" 2)))'
        )
        assert result == {"a": "a=1", "b": "b=2"}

    def test_map_keys_unchanged(self, tool):
        """Test that keys are not modified by alist-map."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) (integer-neg v))'
            ' (alist (list "x" 10) (list "y" 20)))'
        )
        assert list(result.keys()) == ["x", "y"]

    def test_map_preserves_insertion_order(self, tool):
        """Test that alist-map preserves key insertion order."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) v)'
            ' (alist (list "z" 1) (list "a" 2) (list "m" 3)))'
        )
        assert list(result.keys()) == ["z", "a", "m"]

    def test_map_empty_alist(self, tool):
        """Test alist-map on an empty alist returns empty alist."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) v) (alist))'
        )
        assert result == {}

    def test_map_single_entry(self, tool):
        """Test alist-map on a single-entry alist."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) (integer+ v 100))'
            ' (alist (list "only" 1)))'
        )
        assert result == {"only": 101}

    def test_map_returns_alist(self, tool):
        """Test that alist-map returns an alist, not a list."""
        result = tool.evaluate(
            '(alist? (alist-map (lambda (k v) v)'
            ' (alist (list "a" 1))))'
        )
        assert result is True

    def test_map_identity(self, tool):
        """Test that identity function leaves alist values unchanged."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) v)'
            ' (alist (list "a" 1) (list "b" 2) (list "c" 3)))'
        )
        assert result == {"a": 1, "b": 2, "c": 3}

    def test_map_change_value_type(self, tool):
        """Test that alist-map can change the type of values."""
        result = tool.evaluate(
            '(alist-map (lambda (k v) (integer->string v))'
            ' (alist (list "a" 1) (list "b" 2)))'
        )
        assert result == {"a": "1", "b": "2"}

    def test_map_first_class(self, tool):
        """Test that alist-map can be passed as a first-class function."""
        result = tool.evaluate(
            '(let ((double-vals (lambda (al) (alist-map (lambda (k v) (integer* v 2)) al))))'
            ' (double-vals (alist (list "a" 3) (list "b" 4))))'
        )
        assert result == {"a": 6, "b": 8}


class TestAListFilter:
    """Tests for alist-filter."""

    def test_filter_by_value(self, tool):
        """Test filtering entries by value."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) (integer=? (integer% v 2) 0))'
            ' (alist (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4)))'
        )
        assert result == {"b": 2, "d": 4}

    def test_filter_by_key(self, tool):
        """Test filtering entries by key."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) (string>=? k "b"))'
            ' (alist (list "a" 1) (list "b" 2) (list "c" 3)))'
        )
        assert result == {"b": 2, "c": 3}

    def test_filter_by_key_and_value(self, tool):
        """Test filtering using both key and value."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) (string=? k (integer->string v)))'
            ' (alist (list "1" 1) (list "2" 99) (list "3" 3)))'
        )
        assert result == {"1": 1, "3": 3}

    def test_filter_keep_all(self, tool):
        """Test filter with always-true predicate keeps all entries."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) #t)'
            ' (alist (list "a" 1) (list "b" 2)))'
        )
        assert result == {"a": 1, "b": 2}

    def test_filter_keep_none(self, tool):
        """Test filter with always-false predicate returns empty alist."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) #f)'
            ' (alist (list "a" 1) (list "b" 2)))'
        )
        assert result == {}

    def test_filter_empty_alist(self, tool):
        """Test alist-filter on an empty alist returns empty alist."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) #t) (alist))'
        )
        assert result == {}

    def test_filter_single_entry_kept(self, tool):
        """Test alist-filter on a single-entry alist where entry is kept."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) #t) (alist (list "only" 42)))'
        )
        assert result == {"only": 42}

    def test_filter_single_entry_removed(self, tool):
        """Test alist-filter on a single-entry alist where entry is removed."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) #f) (alist (list "only" 42)))'
        )
        assert result == {}

    def test_filter_preserves_insertion_order(self, tool):
        """Test that alist-filter preserves key insertion order of kept entries."""
        result = tool.evaluate(
            '(alist-filter (lambda (k v) (integer>? v 1))'
            ' (alist (list "z" 3) (list "a" 1) (list "m" 2)))'
        )
        assert list(result.keys()) == ["z", "m"]

    def test_filter_returns_alist(self, tool):
        """Test that alist-filter returns an alist, not a list."""
        result = tool.evaluate(
            '(alist? (alist-filter (lambda (k v) #t)'
            ' (alist (list "a" 1))))'
        )
        assert result is True

    def test_filter_first_class(self, tool):
        """Test that alist-filter can be passed as a first-class function."""
        result = tool.evaluate(
            '(let ((keep-positives (lambda (al)'
            '         (alist-filter (lambda (k v) (integer>? v 0)) al))))'
            ' (keep-positives (alist (list "a" 1) (list "b" -1) (list "c" 2))))'
        )
        assert result == {"a": 1, "c": 2}


class TestAListMapFilterComposition:
    """Tests for composing alist-map and alist-filter."""

    def test_filter_then_map(self, tool):
        """Test filtering then mapping."""
        result = tool.evaluate(
            '(let ((data (alist (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4))))'
            ' (alist-map (lambda (k v) (integer* v 10))'
            '   (alist-filter (lambda (k v) (integer=? (integer% v 2) 0)) data)))'
        )
        assert result == {"b": 20, "d": 40}

    def test_map_then_filter(self, tool):
        """Test mapping then filtering."""
        result = tool.evaluate(
            '(let ((data (alist (list "a" 1) (list "b" 2) (list "c" 3))))'
            ' (alist-filter (lambda (k v) (integer>? v 4))'
            '   (alist-map (lambda (k v) (integer* v 2)) data)))'
        )
        assert result == {"c": 6}
