"""Tests for Menai sort-list higher-order function."""

import pytest
from menai import Menai, MenaiEvalError


@pytest.fixture
def tool():
    """Create Menai instance for testing."""
    return Menai()


class TestListSortBasic:
    """Basic sort-list tests."""

    def test_sort_integers_ascending(self, tool):
        """Test sorting integers in ascending order."""
        result = tool.evaluate("(sort-list integer<? (list 3 1 4 1 5 9 2 6))")
        assert result == [1, 1, 2, 3, 4, 5, 6, 9]

    def test_sort_integers_descending(self, tool):
        """Test sorting integers in descending order."""
        result = tool.evaluate("(sort-list integer>? (list 3 1 4 1 5 9 2 6))")
        assert result == [9, 6, 5, 4, 3, 2, 1, 1]

    def test_sort_strings_ascending(self, tool):
        """Test sorting strings in ascending order."""
        result = tool.evaluate('(sort-list string<? (list "banana" "apple" "cherry" "date"))')
        assert result == ["apple", "banana", "cherry", "date"]

    def test_sort_strings_descending(self, tool):
        """Test sorting strings in descending order."""
        result = tool.evaluate('(sort-list string>? (list "banana" "apple" "cherry" "date"))')
        assert result == ["date", "cherry", "banana", "apple"]

    def test_sort_already_sorted(self, tool):
        """Test sorting an already sorted list."""
        result = tool.evaluate("(sort-list integer<? (list 1 2 3 4 5))")
        assert result == [1, 2, 3, 4, 5]

    def test_sort_reverse_sorted(self, tool):
        """Test sorting a reverse-sorted list."""
        result = tool.evaluate("(sort-list integer<? (list 5 4 3 2 1))")
        assert result == [1, 2, 3, 4, 5]


class TestListSortEdgeCases:
    """Edge case tests for sort-list."""

    def test_sort_empty_list(self, tool):
        """Test sorting an empty list returns empty list."""
        result = tool.evaluate("(sort-list integer<? (list))")
        assert result == []

    def test_sort_single_element(self, tool):
        """Test sorting a single-element list returns that list."""
        result = tool.evaluate("(sort-list integer<? (list 42))")
        assert result == [42]

    def test_sort_two_elements_ordered(self, tool):
        """Test sorting two already-ordered elements."""
        result = tool.evaluate("(sort-list integer<? (list 1 2))")
        assert result == [1, 2]

    def test_sort_two_elements_unordered(self, tool):
        """Test sorting two unordered elements."""
        result = tool.evaluate("(sort-list integer<? (list 2 1))")
        assert result == [1, 2]

    def test_sort_all_equal(self, tool):
        """Test sorting a list of all equal elements."""
        result = tool.evaluate("(sort-list integer<? (list 3 3 3 3))")
        assert result == [3, 3, 3, 3]

    def test_sort_duplicates(self, tool):
        """Test sorting a list with duplicate values."""
        result = tool.evaluate("(sort-list integer<? (list 2 1 2 1 3))")
        assert result == [1, 1, 2, 2, 3]


class TestListSortStability:
    """Tests for sort stability — equal elements preserve original order."""

    def test_sort_stable_by_key(self, tool):
        """Test that sort is stable: equal keys preserve original order."""
        # Sort dicts by a numeric field; equal values should stay in original order
        result = tool.evaluate("""
            (sort-list
              (lambda (a b) (integer<? (dict-get a "key") (dict-get b "key")))
              (list
                (dict (list "key" 2) (list "id" "first"))
                (dict (list "key" 1) (list "id" "second"))
                (dict (list "key" 2) (list "id" "third"))
                (dict (list "key" 1) (list "id" "fourth"))))
        """)
        ids = [entry["id"] for entry in result]
        assert ids == ["second", "fourth", "first", "third"]


class TestListSortCustomComparator:
    """Tests for sort-list with custom comparator functions."""

    def test_sort_by_absolute_value(self, tool):
        """Test sorting by absolute value using a custom comparator."""
        result = tool.evaluate("""
            (sort-list
              (lambda (a b) (integer<? (integer-abs a) (integer-abs b)))
              (list -3 1 -4 1 5 -9 2 -6))
        """)
        assert result == [1, 1, 2, -3, -4, 5, -6, -9]

    def test_sort_by_string_length(self, tool):
        """Test sorting strings by length."""
        result = tool.evaluate("""
            (sort-list
              (lambda (a b) (integer<? (string-length a) (string-length b)))
              (list "banana" "fig" "apple" "kiwi" "date"))
        """)
        assert result == ["fig", "kiwi", "date", "apple", "banana"]

    def test_sort_dicts_by_field(self, tool):
        """Test sorting dicts by a specific field."""
        result = tool.evaluate("""
            (sort-list
              (lambda (a b) (integer<? (dict-get a "age") (dict-get b "age")))
              (list
                (dict (list "name" "Charlie") (list "age" 30))
                (dict (list "name" "Alice") (list "age" 25))
                (dict (list "name" "Bob") (list "age" 35))))
        """)
        names = [entry["name"] for entry in result]
        assert names == ["Alice", "Charlie", "Bob"]

    def test_sort_first_class_comparator(self, tool):
        """Test passing a comparator as a first-class value."""
        result = tool.evaluate("""
            (let ((cmp integer<?))
              (sort-list cmp (list 5 3 1 4 2)))
        """)
        assert result == [1, 2, 3, 4, 5]

    def test_sort_returns_new_list(self, tool):
        """Test that sort-list returns a new list, not modifying the original."""
        result = tool.evaluate("""
            (let ((original (list 3 1 2)))
              (let ((sorted (sort-list integer<? original)))
                (list original sorted)))
        """)
        assert result == [[3, 1, 2], [1, 2, 3]]
