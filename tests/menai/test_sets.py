"""Tests for Menai set operations."""

import pytest
from menai import Menai, MenaiEvalError


@pytest.fixture
def tool():
    """Create Menai instance for testing."""
    return Menai()


class TestSetConstruction:
    """Test set construction."""

    def test_empty_set(self, tool):
        """Test creating an empty set."""
        result = tool.evaluate_and_format("(set)")
        assert result == "#{}"

    def test_set_with_integers(self, tool):
        """Test creating a set of integers."""
        result = tool.evaluate_and_format("(set 1 2 3)")
        assert result == "#{1 2 3}"

    def test_set_with_strings(self, tool):
        """Test creating a set of strings."""
        result = tool.evaluate_and_format('(set "a" "b" "c")')
        assert result == '#{\"a\" \"b\" \"c\"}'

    def test_set_deduplicates(self, tool):
        """Test that duplicate elements are silently dropped."""
        result = tool.evaluate_and_format("(set 1 2 3 2 1)")
        assert result == "#{1 2 3}"

    def test_set_preserves_insertion_order(self, tool):
        """Test that first-seen insertion order is preserved after deduplication."""
        result = tool.evaluate_and_format("(set 3 1 2 1 3)")
        assert result == "#{3 1 2}"

    def test_set_with_booleans(self, tool):
        """Test creating a set of booleans."""
        result = tool.evaluate_and_format("(set #t #f)")
        assert result == "#{#t #f}"

    def test_set_with_mixed_hashable_types(self, tool):
        """Test set with mixed hashable types."""
        result = tool.evaluate_and_format('(set 1 "hello" #t)')
        assert result == '#{1 \"hello\" #t}'

    def test_set_singleton(self, tool):
        """Test set with a single element."""
        result = tool.evaluate_and_format("(set 42)")
        assert result == "#{42}"

    def test_set_all_duplicates(self, tool):
        """Test set where all elements are the same."""
        result = tool.evaluate_and_format("(set 5 5 5 5)")
        assert result == "#{5}"


class TestSetConstructionErrors:
    """Test set construction error cases."""

    def test_set_rejects_list_element(self, tool):
        """Test that list elements are rejected as unhashable."""
        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set (list 1 2))")

    def test_set_add_rejects_list_element(self, tool):
        """Test that set-add rejects unhashable elements."""
        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set-add (set 1 2) (list 3 4))")

    def test_list_to_set_rejects_list_element(self, tool):
        """Test that list->set rejects lists containing unhashable elements."""
        with pytest.raises(MenaiEvalError):
            tool.evaluate("(list->set (list (list 1 2)))")


class TestSetTypePredicate:
    """Test set? predicate."""

    def test_set_is_set(self, tool):
        """Test that a set is recognised as a set."""
        result = tool.evaluate("(set? (set 1 2 3))")
        assert result is True

    def test_empty_set_is_set(self, tool):
        """Test that an empty set is recognised as a set."""
        result = tool.evaluate("(set? (set))")
        assert result is True

    def test_list_is_not_set(self, tool):
        """Test that a list is not a set."""
        result = tool.evaluate("(set? (list 1 2 3))")
        assert result is False

    def test_integer_is_not_set(self, tool):
        """Test that an integer is not a set."""
        result = tool.evaluate("(set? 42)")
        assert result is False

    def test_dict_is_not_set(self, tool):
        """Test that a dict is not a set."""
        result = tool.evaluate('(set? (dict (list "a" 1)))')
        assert result is False


class TestSetEquality:
    """Test set=? and set!=? predicates."""

    def test_equal_sets(self, tool):
        """Test that sets with same elements are equal."""
        result = tool.evaluate("(set=? (set 1 2 3) (set 1 2 3))")
        assert result is True

    def test_equal_sets_different_order(self, tool):
        """Test that set equality is order-insensitive."""
        result = tool.evaluate("(set=? (set 1 2 3) (set 3 1 2))")
        assert result is True

    def test_unequal_sets(self, tool):
        """Test that sets with different elements are not equal."""
        result = tool.evaluate("(set=? (set 1 2 3) (set 1 2 4))")
        assert result is False

    def test_equal_empty_sets(self, tool):
        """Test that two empty sets are equal."""
        result = tool.evaluate("(set=? (set) (set))")
        assert result is True

    def test_empty_and_nonempty_not_equal(self, tool):
        """Test that empty and non-empty sets are not equal."""
        result = tool.evaluate("(set=? (set) (set 1))")
        assert result is False

    def test_neq_different_sets(self, tool):
        """Test set!=? with different sets."""
        result = tool.evaluate("(set!=? (set 1 2) (set 1 3))")
        assert result is True

    def test_neq_equal_sets(self, tool):
        """Test set!=? with equal sets."""
        result = tool.evaluate("(set!=? (set 1 2) (set 2 1))")
        assert result is False

    def test_set_eq_variadic_all_equal(self, tool):
        """Test variadic set=? with all equal sets."""
        result = tool.evaluate("(set=? (set 1 2) (set 2 1) (set 1 2))")
        assert result is True

    def test_set_eq_variadic_one_different(self, tool):
        """Test variadic set=? with one different set."""
        result = tool.evaluate("(set=? (set 1 2) (set 2 1) (set 1 3))")
        assert result is False

    def test_set_eq_requires_two_args(self, tool):
        """Test that set=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set=? (set 1 2))")

    def test_set_neq_requires_two_args(self, tool):
        """Test that set!=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set!=? (set 1 2))")

    def test_set_eq_non_set_arg(self, tool):
        """Test that set=? rejects non-set arguments."""
        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set=? (set 1 2) (list 1 2))")

        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set=? (list 1 2) (set 1 2))")

    def test_set_neq_non_set_arg(self, tool):
        """Test that set=? rejects non-set arguments."""
        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set!=? (set 1 2) (list 1 2))")

        with pytest.raises(MenaiEvalError):
            tool.evaluate("(set!=? (list 1 2) (set 1 2))")


class TestSetMembership:
    """Test set-member? predicate."""

    def test_member_present(self, tool):
        """Test membership of an element that is present."""
        result = tool.evaluate("(set-member? (set 1 2 3) 2)")
        assert result is True

    def test_member_absent(self, tool):
        """Test membership of an element that is absent."""
        result = tool.evaluate("(set-member? (set 1 2 3) 99)")
        assert result is False

    def test_member_empty_set(self, tool):
        """Test membership on an empty set."""
        result = tool.evaluate("(set-member? (set) 1)")
        assert result is False

    def test_member_string(self, tool):
        """Test string membership."""
        result = tool.evaluate('(set-member? (set "a" "b" "c") "b")')
        assert result is True

    def test_member_wrong_type(self, tool):
        """Test that set-member? rejects non-set first argument."""
        with pytest.raises(MenaiEvalError, match="requires a set argument"):
            tool.evaluate("(set-member? (list 1 2 3) 1)")


class TestSetLength:
    """Test set-length operation."""

    def test_length_empty(self, tool):
        """Test length of empty set."""
        result = tool.evaluate("(set-length (set))")
        assert result == 0

    def test_length_nonempty(self, tool):
        """Test length of non-empty set."""
        result = tool.evaluate("(set-length (set 1 2 3))")
        assert result == 3

    def test_length_after_deduplication(self, tool):
        """Test that length reflects deduplicated count."""
        result = tool.evaluate("(set-length (set 1 2 2 3 3 3))")
        assert result == 3

    def test_length_wrong_type(self, tool):
        """Test that set-length rejects non-set argument."""
        with pytest.raises(MenaiEvalError, match="requires a set argument"):
            tool.evaluate("(set-length (list 1 2 3))")


class TestSetAdd:
    """Test set-add operation."""

    def test_add_new_element(self, tool):
        """Test adding a new element to a set."""
        result = tool.evaluate_and_format("(set-add (set 1 2) 3)")
        assert result == "#{1 2 3}"

    def test_add_existing_element(self, tool):
        """Test adding an existing element is a no-op."""
        result = tool.evaluate_and_format("(set-add (set 1 2 3) 2)")
        assert result == "#{1 2 3}"

    def test_add_to_empty_set(self, tool):
        """Test adding to an empty set."""
        result = tool.evaluate_and_format("(set-add (set) 42)")
        assert result == "#{42}"

    def test_add_preserves_immutability(self, tool):
        """Test that set-add returns a new set, leaving original unchanged."""
        result = tool.evaluate_and_format(
            "(let* ((s (set 1 2)) (_ (set-add s 3))) s)"
        )
        assert result == "#{1 2}"

    def test_add_wrong_type(self, tool):
        """Test that set-add rejects non-set first argument."""
        with pytest.raises(MenaiEvalError, match="requires a set argument"):
            tool.evaluate("(set-add (list 1 2 3) 4)")


class TestSetRemove:
    """Test set-remove operation."""

    def test_remove_existing_element(self, tool):
        """Test removing an element that is present."""
        result = tool.evaluate_and_format("(set-remove (set 1 2 3) 2)")
        assert result == "#{1 3}"

    def test_remove_absent_element(self, tool):
        """Test removing an element that is absent is a no-op."""
        result = tool.evaluate_and_format("(set-remove (set 1 2 3) 99)")
        assert result == "#{1 2 3}"

    def test_remove_from_empty_set(self, tool):
        """Test removing from an empty set is a no-op."""
        result = tool.evaluate_and_format("(set-remove (set) 1)")
        assert result == "#{}"

    def test_remove_last_element(self, tool):
        """Test removing the only element produces empty set."""
        result = tool.evaluate_and_format("(set-remove (set 42) 42)")
        assert result == "#{}"

    def test_remove_wrong_type(self, tool):
        """Test that set-remove rejects non-set first argument."""
        with pytest.raises(MenaiEvalError, match="requires a set argument"):
            tool.evaluate("(set-remove (list 1 2 3) 1)")


class TestSetAlgebra:
    """Test set-union, set-intersection, set-difference, set-subset?."""

    def test_union_disjoint(self, tool):
        """Test union of disjoint sets."""
        result = tool.evaluate_and_format("(set->list (set-union (set 1 2) (set 3 4)))")
        assert result == "(1 2 3 4)"

    def test_union_overlapping(self, tool):
        """Test union of overlapping sets."""
        result = tool.evaluate_and_format("(set->list (set-union (set 1 2 3) (set 3 4 5)))")
        assert result == "(1 2 3 4 5)"

    def test_union_with_empty(self, tool):
        """Test union with empty set is identity."""
        result = tool.evaluate("(set=? (set-union (set 1 2 3) (set)) (set 1 2 3))")
        assert result is True

    def test_union_both_empty(self, tool):
        """Test union of two empty sets."""
        result = tool.evaluate_and_format("(set-union (set) (set))")
        assert result == "#{}"

    def test_intersection_overlapping(self, tool):
        """Test intersection of overlapping sets."""
        result = tool.evaluate_and_format("(set->list (set-intersection (set 1 2 3) (set 2 3 4)))")
        assert result == "(2 3)"

    def test_intersection_disjoint(self, tool):
        """Test intersection of disjoint sets is empty."""
        result = tool.evaluate_and_format("(set-intersection (set 1 2) (set 3 4))")
        assert result == "#{}"

    def test_intersection_with_empty(self, tool):
        """Test intersection with empty set is empty."""
        result = tool.evaluate_and_format("(set-intersection (set 1 2 3) (set))")
        assert result == "#{}"

    def test_intersection_preserves_order_of_first(self, tool):
        """Test that intersection preserves insertion order of first set."""
        result = tool.evaluate_and_format("(set->list (set-intersection (set 3 1 2) (set 1 2 3)))")
        assert result == "(3 1 2)"

    def test_difference_overlapping(self, tool):
        """Test difference of overlapping sets (a minus b)."""
        result = tool.evaluate_and_format("(set->list (set-difference (set 1 2 3 4) (set 2 4)))")
        assert result == "(1 3)"

    def test_difference_disjoint(self, tool):
        """Test difference of disjoint sets returns first set unchanged."""
        result = tool.evaluate("(set=? (set-difference (set 1 2 3) (set 4 5)) (set 1 2 3))")
        assert result is True

    def test_difference_with_empty(self, tool):
        """Test difference with empty set returns original."""
        result = tool.evaluate("(set=? (set-difference (set 1 2 3) (set)) (set 1 2 3))")
        assert result is True

    def test_difference_all_removed(self, tool):
        """Test difference where all elements are removed."""
        result = tool.evaluate_and_format("(set-difference (set 1 2) (set 1 2 3))")
        assert result == "#{}"

    def test_subset_true(self, tool):
        """Test that a proper subset is detected."""
        result = tool.evaluate("(set-subset? (set 1 2) (set 1 2 3))")
        assert result is True

    def test_subset_equal_sets(self, tool):
        """Test that a set is a subset of itself."""
        result = tool.evaluate("(set-subset? (set 1 2 3) (set 1 2 3))")
        assert result is True

    def test_subset_false(self, tool):
        """Test that a non-subset is not detected as subset."""
        result = tool.evaluate("(set-subset? (set 1 2 4) (set 1 2 3))")
        assert result is False

    def test_subset_empty_is_subset_of_all(self, tool):
        """Test that empty set is a subset of any set."""
        result = tool.evaluate("(set-subset? (set) (set 1 2 3))")
        assert result is True

    def test_subset_empty_of_empty(self, tool):
        """Test that empty set is a subset of empty set."""
        result = tool.evaluate("(set-subset? (set) (set))")
        assert result is True

    def test_union_wrong_type(self, tool):
        """Test that set-union rejects non-set arguments."""
        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-union (set 1 2) (list 3))")

        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-union (list 1 2) (set 3))")

    def test_intersection_wrong_type(self, tool):
        """Test that set-intersection rejects non-set arguments."""
        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-intersection (set 1 2) (list 3))")

        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-intersection (list 1 2) (set 3))")

    def test_difference_wrong_type(self, tool):
        """Test that set-difference rejects non-set arguments."""
        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-difference (set 1 2) 42)")

        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-difference 42 (set 1 2))")

    def test_subset_wrong_type(self, tool):
        """Test that set-subset? rejects non-set arguments."""
        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-subset? (set 1 2) (list 1 2))")

        with pytest.raises(MenaiEvalError, match="requires set arguments"):
            tool.evaluate("(set-subset? (list 1) (set 1 2))")


class TestSetConversion:
    """Test set->list and list->set conversions."""

    def test_set_to_list(self, tool):
        """Test converting a set to a list."""
        result = tool.evaluate_and_format("(set->list (set 1 2 3))")
        assert result == "(1 2 3)"

    def test_empty_set_to_list(self, tool):
        """Test converting an empty set to a list."""
        result = tool.evaluate_and_format("(set->list (set))")
        assert result == "()"

    def test_set_to_list_preserves_order(self, tool):
        """Test that set->list preserves insertion order."""
        result = tool.evaluate_and_format("(set->list (set 3 1 2))")
        assert result == "(3 1 2)"

    def test_list_to_set(self, tool):
        """Test converting a list to a set."""
        result = tool.evaluate("(set=? (list->set (list 1 2 3)) (set 1 2 3))")
        assert result is True

    def test_list_to_set_deduplicates(self, tool):
        """Test that list->set deduplicates."""
        result = tool.evaluate("(set-length (list->set (list 1 2 1 3 2)))")
        assert result == 3

    def test_list_to_set_empty(self, tool):
        """Test converting an empty list to a set."""
        result = tool.evaluate_and_format("(list->set (list))")
        assert result == "#{}"

    def test_roundtrip_set_to_list_to_set(self, tool):
        """Test that set->list->set roundtrip preserves set equality."""
        result = tool.evaluate("(set=? (list->set (set->list (set 1 2 3))) (set 1 2 3))")
        assert result is True

    def test_set_to_list_wrong_type(self, tool):
        """Test that set->list rejects non-set argument."""
        with pytest.raises(MenaiEvalError, match="requires a set argument"):
            tool.evaluate("(set->list (list 1 2 3))")

    def test_list_to_set_wrong_type(self, tool):
        """Test that list->set rejects non-list argument."""
        with pytest.raises(MenaiEvalError, match="requires a list argument"):
            tool.evaluate("(list->set (set 1 2 3))")


class TestSetHigherOrder:
    """Test map-set, filter-set, fold-set."""

    def test_map_set(self, tool):
        """Test mapping a function over a set."""
        result = tool.evaluate(
            "(set=? (map-set (lambda (x) (integer* x 2)) (set 1 2 3)) (set 2 4 6))"
        )
        assert result is True

    def test_map_set_empty(self, tool):
        """Test mapping over an empty set returns empty set."""
        result = tool.evaluate_and_format(
            "(map-set (lambda (x) (integer* x 2)) (set))"
        )
        assert result == "#{}"

    def test_map_set_deduplicates(self, tool):
        """Test that map-set deduplicates when function maps two elements to same value."""
        result = tool.evaluate(
            "(set-length (map-set (lambda (x) (integer% x 2)) (set 1 2 3 4)))"
        )
        assert result == 2

    def test_filter_set(self, tool):
        """Test filtering a set by a predicate."""
        result = tool.evaluate(
            "(set=? (filter-set (lambda (x) (integer>? x 2)) (set 1 2 3 4)) (set 3 4))"
        )
        assert result is True

    def test_filter_set_empty_result(self, tool):
        """Test filtering where no elements match."""
        result = tool.evaluate_and_format(
            "(filter-set (lambda (x) (integer>? x 10)) (set 1 2 3))"
        )
        assert result == "#{}"

    def test_filter_set_all_match(self, tool):
        """Test filtering where all elements match."""
        result = tool.evaluate(
            "(set=? (filter-set (lambda (x) (integer>? x 0)) (set 1 2 3)) (set 1 2 3))"
        )
        assert result is True

    def test_fold_set_sum(self, tool):
        """Test folding a set with addition."""
        result = tool.evaluate("(fold-set integer+ 0 (set 1 2 3 4 5))")
        assert result == 15

    def test_fold_set_empty(self, tool):
        """Test folding an empty set returns init value."""
        result = tool.evaluate("(fold-set integer+ 0 (set))")
        assert result == 0

    def test_fold_set_product(self, tool):
        """Test folding a set with multiplication."""
        result = tool.evaluate("(fold-set integer* 1 (set 1 2 3 4))")
        assert result == 24


class TestSetPatternMatching:
    """Test sets in pattern matching contexts."""

    def test_match_set_with_predicate(self, tool):
        """Test matching a set using predicate pattern."""
        result = tool.evaluate(
            "(match (set 1 2 3) ((? set? s) (set-length s)) (_ 0))"
        )
        assert result == 3

    def test_match_non_set_falls_through(self, tool):
        """Test that non-set falls through to wildcard."""
        result = tool.evaluate(
            "(match (list 1 2 3) ((? set? s) (set-length s)) (_ 0))"
        )
        assert result == 0


class TestSetFirstClassValues:
    """Test that sets are proper first-class values."""

    def test_set_in_list(self, tool):
        """Test storing a set inside a list."""
        result = tool.evaluate(
            "(set-length (list-first (list (set 1 2 3))))"
        )
        assert result == 3

    def test_set_in_let_binding(self, tool):
        """Test binding a set in a let expression."""
        result = tool.evaluate(
            "(let ((s (set 10 20 30))) (set-member? s 20))"
        )
        assert result is True

    def test_set_passed_to_function(self, tool):
        """Test passing a set to a user-defined function."""
        result = tool.evaluate(
            "((lambda (s) (set-length s)) (set 1 2 3 4 5))"
        )
        assert result == 5

    def test_set_returned_from_function(self, tool):
        """Test returning a set from a user-defined function."""
        result = tool.evaluate(
            "(set=? ((lambda (a b) (set-union a b)) (set 1 2) (set 3 4)) (set 1 2 3 4))"
        )
        assert result is True


class TestAnySet:
    """Test any-set? predicate."""

    def test_any_set_true(self, tool):
        """Test any-set? returns #t when at least one element satisfies predicate."""
        result = tool.evaluate("(any-set? (lambda (x) (integer>? x 2)) (set 1 2 3))")
        assert result is True

    def test_any_set_false(self, tool):
        """Test any-set? returns #f when no element satisfies predicate."""
        result = tool.evaluate("(any-set? (lambda (x) (integer>? x 10)) (set 1 2 3))")
        assert result is False

    def test_any_set_empty(self, tool):
        """Test any-set? returns #f on empty set."""
        result = tool.evaluate("(any-set? (lambda (x) #t) (set))")
        assert result is False

    def test_any_set_single_element_match(self, tool):
        """Test any-set? with a single matching element."""
        result = tool.evaluate("(any-set? (lambda (x) (integer=? x 42)) (set 42))")
        assert result is True

    def test_any_set_single_element_no_match(self, tool):
        """Test any-set? with a single non-matching element."""
        result = tool.evaluate("(any-set? (lambda (x) (integer=? x 99)) (set 42))")
        assert result is False

    def test_any_set_all_match(self, tool):
        """Test any-set? returns #t when all elements satisfy predicate."""
        result = tool.evaluate("(any-set? (lambda (x) (integer>? x 0)) (set 1 2 3))")
        assert result is True

    def test_any_set_string_elements(self, tool):
        """Test any-set? with string elements."""
        result = tool.evaluate('(any-set? (lambda (x) (string=? x "b")) (set "a" "b" "c"))')
        assert result is True


class TestAllSet:
    """Test all-set? predicate."""

    def test_all_set_true(self, tool):
        """Test all-set? returns #t when all elements satisfy predicate."""
        result = tool.evaluate("(all-set? (lambda (x) (integer>? x 0)) (set 1 2 3))")
        assert result is True

    def test_all_set_false(self, tool):
        """Test all-set? returns #f when at least one element does not satisfy predicate."""
        result = tool.evaluate("(all-set? (lambda (x) (integer>? x 1)) (set 1 2 3))")
        assert result is False

    def test_all_set_empty(self, tool):
        """Test all-set? returns #t vacuously on empty set."""
        result = tool.evaluate("(all-set? (lambda (x) #f) (set))")
        assert result is True

    def test_all_set_single_element_match(self, tool):
        """Test all-set? with a single matching element."""
        result = tool.evaluate("(all-set? (lambda (x) (integer=? x 42)) (set 42))")
        assert result is True

    def test_all_set_single_element_no_match(self, tool):
        """Test all-set? with a single non-matching element."""
        result = tool.evaluate("(all-set? (lambda (x) (integer=? x 99)) (set 42))")
        assert result is False

    def test_all_set_string_elements(self, tool):
        """Test all-set? with string elements."""
        result = tool.evaluate('(all-set? (lambda (x) (integer>? (string-length x) 0)) (set "a" "bb" "ccc"))')
        assert result is True

    def test_all_set_one_fails(self, tool):
        """Test all-set? returns #f when exactly one element fails."""
        result = tool.evaluate("(all-set? (lambda (x) (integer>? x 0)) (set 1 2 0 3))")
        assert result is False
