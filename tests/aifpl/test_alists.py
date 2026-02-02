"""Tests for AIFPL alist (association list) operations."""

import pytest
from aifpl import AIFPL, AIFPLEvalError


@pytest.fixture
def tool():
    """Create AIFPL instance for testing."""
    return AIFPL()


class TestAListConstruction:
    """Test alist construction."""

    def test_empty_alist(self, tool):
        """Test creating an empty alist."""
        result = tool.evaluate("(alist)")
        assert result == {}

    def test_simple_alist(self, tool):
        """Test creating a simple alist with string keys."""
        result = tool.evaluate('(alist (list "name" "Alice") (list "age" 30))')
        assert result == {"name": "Alice", "age": 30}

    def test_alist_with_number_keys(self, tool):
        """Test alist with numeric keys."""
        result = tool.evaluate("(alist (list 1 \"one\") (list 2 \"two\") (list 3 \"three\"))")
        assert result == {"1": "one", "2": "two", "3": "three"}

    def test_alist_with_boolean_keys(self, tool):
        """Test alist with boolean keys."""
        result = tool.evaluate('(alist (list #t "true value") (list #f "false value"))')
        assert result == {"True": "true value", "False": "false value"}

    def test_alist_with_mixed_value_types(self, tool):
        """Test alist with different value types."""
        result = tool.evaluate('(alist (list "name" "Bob") (list "age" 25) (list "active" #t))')
        assert result == {"name": "Bob", "age": 25, "active": True}

    def test_alist_with_nested_values(self, tool):
        """Test alist with nested lists as values."""
        result = tool.evaluate('(alist (list "numbers" (list 1 2 3)) (list "letters" (list "a" "b")))')
        assert result == {"numbers": [1, 2, 3], "letters": ["a", "b"]}

    def test_alist_preserves_insertion_order(self, tool):
        """Test that alist preserves insertion order."""
        result = tool.evaluate('(alist (list "z" 1) (list "a" 2) (list "m" 3))')
        keys = list(result.keys())
        assert keys == ["z", "a", "m"]


class TestAListConstructionErrors:
    """Test alist construction error cases."""

    def test_alist_pair_not_list(self, tool):
        """Test error when pair is not a list."""
        with pytest.raises(AIFPLEvalError, match="AList pair 1 must be a list"):
            tool.evaluate('(alist "not-a-pair")')

    def test_alist_pair_wrong_length(self, tool):
        """Test error when pair doesn't have exactly 2 elements."""
        with pytest.raises(AIFPLEvalError, match="AList pair 1 must have exactly 2 elements"):
            tool.evaluate('(alist (list "key"))')

    def test_alist_pair_too_many_elements(self, tool):
        """Test error when pair has more than 2 elements."""
        with pytest.raises(AIFPLEvalError, match="AList pair 1 must have exactly 2 elements"):
            tool.evaluate('(alist (list "key" "val1" "val2"))')

    def test_alist_invalid_key_type(self, tool):
        """Test error with invalid key type (list)."""
        with pytest.raises(AIFPLEvalError, match="AList keys must be strings, numbers, booleans, or symbols"):
            tool.evaluate('(alist (list (list 1 2) "value"))')


class TestAListGet:
    """Test alist-get operation."""

    def test_alist_get_existing_key(self, tool):
        """Test getting an existing key."""
        result = tool.evaluate('(alist-get (alist (list "name" "Alice") (list "age" 30)) "name")')
        assert result == "Alice"

    def test_alist_get_missing_key_default_false(self, tool):
        """Test getting a missing key returns #f by default."""
        result = tool.evaluate('(alist-get (alist (list "name" "Alice")) "age")')
        assert result is False

    def test_alist_get_with_default(self, tool):
        """Test getting a missing key with custom default."""
        result = tool.evaluate('(alist-get (alist (list "name" "Alice")) "age" 0)')
        assert result == 0

    def test_alist_get_number_key(self, tool):
        """Test getting with numeric key."""
        result = tool.evaluate('(alist-get (alist (list 1 "one") (list 2 "two")) 2)')
        assert result == "two"

    def test_alist_get_from_nested_alist(self, tool):
        """Test getting from nested alists."""
        result = tool.evaluate('''
            (let ((data (alist (list "user" (alist (list "name" "Bob") (list "id" 123))))))
              (alist-get (alist-get data "user") "name"))
        ''')
        assert result == "Bob"


class TestAListGetErrors:
    """Test alist-get error cases."""

    def test_alist_get_wrong_arg_count_too_few(self, tool):
        """Test error with too few arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-get requires 2 or 3 arguments"):
            tool.evaluate('(alist-get (alist (list "a" 1)))')

    def test_alist_get_wrong_arg_count_too_many(self, tool):
        """Test error with too many arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-get requires 2 or 3 arguments"):
            tool.evaluate('(alist-get (alist (list "a" 1)) "a" 0 "extra")')

    def test_alist_get_not_alist(self, tool):
        """Test error when first argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="First argument must be an alist"):
            tool.evaluate('(alist-get (list 1 2 3) "key")')


class TestAListSet:
    """Test alist-set operation."""

    def test_alist_set_new_key(self, tool):
        """Test setting a new key."""
        result = tool.evaluate('(alist-set (alist (list "name" "Alice")) "age" 30)')
        assert result == {"name": "Alice", "age": 30}

    def test_alist_set_existing_key(self, tool):
        """Test updating an existing key."""
        result = tool.evaluate('(alist-set (alist (list "name" "Alice") (list "age" 30)) "age" 31)')
        assert result == {"name": "Alice", "age": 31}

    def test_alist_set_immutable(self, tool):
        """Test that alist-set doesn't modify original."""
        result = tool.evaluate('''
            (let ((original (alist (list "name" "Alice") (list "age" 30)))
                  (updated (alist-set original "age" 31)))
              (list (alist-get original "age") (alist-get updated "age")))
        ''')
        assert result == [30, 31]

    def test_alist_set_preserves_order(self, tool):
        """Test that alist-set preserves insertion order when updating."""
        result = tool.evaluate('(alist-set (alist (list "a" 1) (list "b" 2) (list "c" 3)) "b" 20)')
        keys = list(result.keys())
        assert keys == ["a", "b", "c"]


class TestAListSetErrors:
    """Test alist-set error cases."""

    def test_alist_set_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-set requires exactly 3 arguments"):
            tool.evaluate('(alist-set (alist (list "a" 1)) "b")')

    def test_alist_set_not_alist(self, tool):
        """Test error when first argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="First argument must be an alist"):
            tool.evaluate('(alist-set "not-alist" "key" "value")')


class TestAListHas:
    """Test alist-has? operation."""

    def test_alist_has_existing_key(self, tool):
        """Test checking for existing key."""
        result = tool.evaluate('(alist-has? (alist (list "name" "Alice") (list "age" 30)) "name")')
        assert result is True

    def test_alist_has_missing_key(self, tool):
        """Test checking for missing key."""
        result = tool.evaluate('(alist-has? (alist (list "name" "Alice")) "age")')
        assert result is False

    def test_alist_has_empty_alist(self, tool):
        """Test checking in empty alist."""
        result = tool.evaluate('(alist-has? (alist) "any-key")')
        assert result is False


class TestAListHasErrors:
    """Test alist-has? error cases."""

    def test_alist_has_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-has\\? requires exactly 2 arguments"):
            tool.evaluate('(alist-has? (alist (list "a" 1)))')

    def test_alist_has_not_alist(self, tool):
        """Test error when first argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="First argument must be an alist"):
            tool.evaluate('(alist-has? 42 "key")')


class TestAListKeys:
    """Test alist-keys operation."""

    def test_alist_keys_simple(self, tool):
        """Test getting keys from alist."""
        result = tool.evaluate('(alist-keys (alist (list "name" "Alice") (list "age" 30) (list "city" "NYC")))')
        assert result == ["name", "age", "city"]

    def test_alist_keys_empty(self, tool):
        """Test getting keys from empty alist."""
        result = tool.evaluate('(alist-keys (alist))')
        assert result == []

    def test_alist_keys_preserves_order(self, tool):
        """Test that keys are returned in insertion order."""
        result = tool.evaluate('(alist-keys (alist (list "z" 1) (list "a" 2) (list "m" 3)))')
        assert result == ["z", "a", "m"]


class TestAListKeysErrors:
    """Test alist-keys error cases."""

    def test_alist_keys_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-keys requires exactly 1 argument"):
            tool.evaluate('(alist-keys)')

    def test_alist_keys_not_alist(self, tool):
        """Test error when argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="Argument must be an alist"):
            tool.evaluate('(alist-keys (list 1 2 3))')


class TestAListValues:
    """Test alist-values operation."""

    def test_alist_values_simple(self, tool):
        """Test getting values from alist."""
        result = tool.evaluate('(alist-values (alist (list "name" "Alice") (list "age" 30) (list "city" "NYC")))')
        assert result == ["Alice", 30, "NYC"]

    def test_alist_values_empty(self, tool):
        """Test getting values from empty alist."""
        result = tool.evaluate('(alist-values (alist))')
        assert result == []

    def test_alist_values_preserves_order(self, tool):
        """Test that values are returned in insertion order."""
        result = tool.evaluate('(alist-values (alist (list "z" 1) (list "a" 2) (list "m" 3)))')
        assert result == [1, 2, 3]


class TestAListValuesErrors:
    """Test alist-values error cases."""

    def test_alist_values_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-values requires exactly 1 argument"):
            tool.evaluate('(alist-values (alist (list "a" 1)) "extra")')

    def test_alist_values_not_alist(self, tool):
        """Test error when argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="Argument must be an alist"):
            tool.evaluate('(alist-values #t)')


class TestAListRemove:
    """Test alist-remove operation."""

    def test_alist_remove_existing_key(self, tool):
        """Test removing an existing key."""
        result = tool.evaluate('(alist-remove (alist (list "name" "Alice") (list "age" 30) (list "city" "NYC")) "age")')
        assert result == {"name": "Alice", "city": "NYC"}

    def test_alist_remove_missing_key(self, tool):
        """Test removing a non-existent key (no-op)."""
        result = tool.evaluate('(alist-remove (alist (list "name" "Alice")) "age")')
        assert result == {"name": "Alice"}

    def test_alist_remove_immutable(self, tool):
        """Test that alist-remove doesn't modify original."""
        result = tool.evaluate('''
            (let ((original (alist (list "name" "Alice") (list "age" 30)))
                  (removed (alist-remove original "age")))
              (list (alist-has? original "age") (alist-has? removed "age")))
        ''')
        assert result == [True, False]


class TestAListRemoveErrors:
    """Test alist-remove error cases."""

    def test_alist_remove_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-remove requires exactly 2 arguments"):
            tool.evaluate('(alist-remove (alist (list "a" 1)))')

    def test_alist_remove_not_alist(self, tool):
        """Test error when first argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="First argument must be an alist"):
            tool.evaluate('(alist-remove (list 1 2) "key")')


class TestAListMerge:
    """Test alist-merge operation."""

    def test_alist_merge_no_conflicts(self, tool):
        """Test merging alists with no overlapping keys."""
        result = tool.evaluate('(alist-merge (alist (list "a" 1) (list "b" 2)) (alist (list "c" 3) (list "d" 4)))')
        assert result == {"a": 1, "b": 2, "c": 3, "d": 4}

    def test_alist_merge_with_conflicts(self, tool):
        """Test merging alists with overlapping keys (second wins)."""
        result = tool.evaluate('(alist-merge (alist (list "a" 1) (list "b" 2)) (alist (list "b" 20) (list "c" 3)))')
        assert result == {"a": 1, "b": 20, "c": 3}

    def test_alist_merge_empty_first(self, tool):
        """Test merging empty alist with non-empty."""
        result = tool.evaluate('(alist-merge (alist) (alist (list "a" 1) (list "b" 2)))')
        assert result == {"a": 1, "b": 2}

    def test_alist_merge_empty_second(self, tool):
        """Test merging non-empty with empty alist."""
        result = tool.evaluate('(alist-merge (alist (list "a" 1) (list "b" 2)) (alist))')
        assert result == {"a": 1, "b": 2}

    def test_alist_merge_preserves_order(self, tool):
        """Test that merge preserves first alist's order, then adds new keys."""
        result = tool.evaluate('(alist-merge (alist (list "z" 1) (list "a" 2)) (alist (list "m" 3) (list "b" 4)))')
        keys = list(result.keys())
        assert keys == ["z", "a", "m", "b"]


class TestAListMergeErrors:
    """Test alist-merge error cases."""

    def test_alist_merge_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="alist-merge requires exactly 2 arguments"):
            tool.evaluate('(alist-merge (alist (list "a" 1)))')

    def test_alist_merge_first_not_alist(self, tool):
        """Test error when first argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="First argument must be an alist"):
            tool.evaluate('(alist-merge (list 1 2) (alist (list "a" 1)))')

    def test_alist_merge_second_not_alist(self, tool):
        """Test error when second argument is not an alist."""
        with pytest.raises(AIFPLEvalError, match="Second argument must be an alist"):
            tool.evaluate('(alist-merge (alist (list "a" 1)) "not-alist")')


class TestAListPredicate:
    """Test alist? type predicate."""

    def test_alist_predicate_true(self, tool):
        """Test alist? returns true for alist."""
        result = tool.evaluate('(alist? (alist (list "name" "Alice")))')
        assert result is True

    def test_alist_predicate_false_list(self, tool):
        """Test alist? returns false for list."""
        result = tool.evaluate('(alist? (list 1 2 3))')
        assert result is False

    def test_alist_predicate_false_number(self, tool):
        """Test alist? returns false for number."""
        result = tool.evaluate('(alist? 42)')
        assert result is False

    def test_alist_predicate_false_string(self, tool):
        """Test alist? returns false for string."""
        result = tool.evaluate('(alist? "hello")')
        assert result is False


class TestAListPredicateErrors:
    """Test alist? error cases."""

    def test_alist_predicate_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="alist\\? requires exactly 1 argument"):
            tool.evaluate('(alist?)')


class TestAListFormatting:
    """Test alist formatting."""

    def test_alist_format_simple(self, tool):
        """Test formatting a simple alist."""
        result = tool.evaluate_and_format('(alist (list "name" "Alice") (list "age" 30))')
        assert result == '{("name" "Alice") ("age" 30)}'

    def test_alist_format_empty(self, tool):
        """Test formatting an empty alist."""
        result = tool.evaluate_and_format('(alist)')
        assert result == '{}'

    def test_alist_format_nested(self, tool):
        """Test formatting alist with nested values."""
        result = tool.evaluate_and_format('(alist (list "items" (list 1 2 3)))')
        assert result == '{("items" (1 2 3))}'


class TestAListWithFunctionalOperations:
    """Test alists with higher-order functions."""

    def test_map_over_alist_keys(self, tool):
        """Test mapping over alist keys."""
        result = tool.evaluate('''
            (let ((data (alist (list "name" "Alice") (list "age" 30))))
              (map string-upcase (alist-keys data)))
        ''')
        assert result == ["NAME", "AGE"]

    def test_filter_alist_values(self, tool):
        """Test filtering alist values."""
        result = tool.evaluate('''
            (let ((data (alist (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4))))
              (filter (lambda (v) (> v 2)) (alist-values data)))
        ''')
        assert result == [3, 4]

    def test_fold_over_alist_values(self, tool):
        """Test folding over alist values."""
        result = tool.evaluate('''
            (let ((data (alist (list "a" 1) (list "b" 2) (list "c" 3))))
              (fold + 0 (alist-values data)))
        ''')
        assert result == 6

    def test_process_list_of_alists(self, tool):
        """Test processing a list of alists."""
        result = tool.evaluate('''
            (let ((people (list 
                           (alist (list "name" "Alice") (list "age" 30))
                           (alist (list "name" "Bob") (list "age" 25))
                           (alist (list "name" "Carol") (list "age" 35)))))
              (map (lambda (p) (alist-get p "name")) people))
        ''')
        assert result == ["Alice", "Bob", "Carol"]


class TestAListPatternMatching:
    """Test alists with pattern matching."""

    def test_match_alist_type(self, tool):
        """Test matching alist type."""
        result = tool.evaluate('''
            (match (alist (list "name" "Alice"))
              ((alist? a) "is-alist")
              (_ "not-alist"))
        ''')
        assert result == "is-alist"

    def test_match_alist_vs_list(self, tool):
        """Test distinguishing alist from list in pattern matching."""
        result = tool.evaluate('''
            (let ((process (lambda (data)
                             (match data
                               ((alist? a) "alist")
                               ((list? l) "list")
                               (_ "other")))))
              (list (process (alist (list "a" 1)))
                    (process (list 1 2 3))))
        ''')
        assert result == ["alist", "list"]

    def test_match_with_alist_operations(self, tool):
        """Test pattern matching combined with alist operations."""
        result = tool.evaluate('''
            (match (alist (list "type" "user") (list "name" "Alice"))
              ((alist? data)
               (if (= (alist-get data "type") "user")
                   (alist-get data "name")
                   "unknown"))
              (_ "not-alist"))
        ''')
        assert result == "Alice"


class TestAListComplexScenarios:
    """Test complex scenarios with alists."""

    def test_nested_alists(self, tool):
        """Test nested alist structures."""
        result = tool.evaluate('''
            (let ((user (alist 
                         (list "name" "Alice")
                         (list "address" (alist 
                                     (list "city" "NYC")
                                     (list "zip" "10001"))))))
              (alist-get (alist-get user "address") "city"))
        ''')
        assert result == "NYC"

    def test_alist_transformation_pipeline(self, tool):
        """Test transforming alist through multiple operations."""
        result = tool.evaluate('''
            (let ((data (alist (list "a" 1) (list "b" 2) (list "c" 3))))
              (let ((with-d (alist-set data "d" 4))
                    (without-b (alist-remove with-d "b"))
                    (updated-c (alist-set without-b "c" 30)))
                updated-c))
        ''')
        assert result == {"a": 1, "c": 30, "d": 4}

    def test_merge_multiple_alists(self, tool):
        """Test merging multiple alists."""
        result = tool.evaluate('''
            (let ((defaults (alist (list "port" 8080) (list "host" "localhost")))
                  (config (alist (list "port" 3000)))
                  (overrides (alist (list "debug" #t))))
              (alist-merge (alist-merge defaults config) overrides))
        ''')
        assert result == {"port": 3000, "host": "localhost", "debug": True}

    def test_convert_list_to_alist(self, tool):
        """Test building alist from list data."""
        result = tool.evaluate('''
            (let ((pairs (list (list "name" "Alice") (list "age" 30) (list "city" "NYC"))))
              (fold (lambda (acc pair)
                      (alist-set acc (first pair) (first (rest pair))))
                    (alist)
                    pairs))
        ''')
        assert result == {"name": "Alice", "age": 30, "city": "NYC"}


class TestAListEquality:
    """Test alist equality comparisons."""

    def test_alist_equality_same_content(self, tool):
        """Test that alists with same content are equal."""
        result = tool.evaluate('(= (alist (list "a" 1) (list "b" 2)) (alist (list "a" 1) (list "b" 2)))')
        assert result is True

    def test_alist_equality_different_content(self, tool):
        """Test that alists with different content are not equal."""
        result = tool.evaluate('(= (alist (list "a" 1) (list "b" 2)) (alist (list "a" 1) (list "b" 3)))')
        assert result is False

    def test_alist_equality_different_keys(self, tool):
        """Test that alists with different keys are not equal."""
        result = tool.evaluate('(= (alist (list "a" 1)) (alist (list "b" 1)))')
        assert result is False

    def test_alist_inequality(self, tool):
        """Test alist inequality operator."""
        result = tool.evaluate('(!= (alist (list "a" 1)) (alist (list "a" 2)))')
        assert result is True

    def test_alist_not_equal_to_list(self, tool):
        """Test that alist is not equal to list."""
        result = tool.evaluate('(= (alist (list "a" 1)) (list 1 2))')
        assert result is False



class TestAListLength:
    """Test alist length operations."""

    def test_length_with_alist(self, tool):
        """Test that length works with alists."""
        result = tool.evaluate('(length (alist (list "name" "Alice") (list "age" 30) (list "city" "NYC")))')
        assert result == 3

    def test_length_empty_alist(self, tool):
        """Test length of empty alist."""
        result = tool.evaluate('(length (alist))')
        assert result == 0

    def test_length_single_entry_alist(self, tool):
        """Test length of alist with single entry."""
        result = tool.evaluate('(length (alist (list "only" "one")))')
        assert result == 1

    def test_length_still_works_with_lists(self, tool):
        """Test that length still works with regular lists."""
        result = tool.evaluate('(length (list 1 2 3 4 5))')
        assert result == 5

    def test_length_with_nested_alist(self, tool):
        """Test length of alist containing nested alists."""
        result = tool.evaluate('''
            (length (alist (list "user" (alist (list "name" "Bob") (list "id" 123)))))
        ''')
        assert result == 1

    def test_length_after_alist_set(self, tool):
        """Test length after adding entries with alist-set."""
        result = tool.evaluate('''
            (let ((a1 (alist (list "a" 1))))
              (let ((a2 (alist-set a1 "b" 2)))
                (let ((a3 (alist-set a2 "c" 3)))
                  (length a3))))
        ''')
        assert result == 3

    def test_length_after_alist_remove(self, tool):
        """Test length after removing entries with alist-remove."""
        result = tool.evaluate('''
            (let ((a1 (alist (list "a" 1) (list "b" 2) (list "c" 3))))
              (length (alist-remove a1 "b")))
        ''')
        assert result == 2

    def test_length_equals_keys_length(self, tool):
        """Test that length of alist equals length of its keys."""
        result = tool.evaluate('''
            (let ((my-alist (alist (list "a" 1) (list "b" 2) (list "c" 3))))
              (= (length my-alist) (length (alist-keys my-alist))))
        ''')
        assert result is True


class TestAListLengthFunction:
    """Test alist-length specific function."""

    def test_alist_length_basic(self, tool):
        """Test alist-length function."""
        result = tool.evaluate('(alist-length (alist (list "x" 1) (list "y" 2)))')
        assert result == 2

    def test_alist_length_empty(self, tool):
        """Test alist-length on empty alist."""
        result = tool.evaluate('(alist-length (alist))')
        assert result == 0

    def test_alist_length_matches_length(self, tool):
        """Test that alist-length and length return same result."""
        result = tool.evaluate('''
            (let ((a (alist (list "a" 1) (list "b" 2) (list "c" 3))))
              (= (length a) (alist-length a)))
        ''')
        assert result is True

    def test_alist_length_in_expression(self, tool):
        """Test using alist-length in arithmetic expression."""
        result = tool.evaluate('''
            (let ((data (alist (list "a" 1) (list "b" 2))))
              (* (alist-length data) 10))
        ''')
        assert result == 20


class TestAListLengthErrors:
    """Test error handling for alist length operations."""

    def test_length_with_invalid_type(self, tool):
        """Test that length with invalid type raises error."""
        with pytest.raises(AIFPLEvalError, match="length requires list or alist argument"):
            tool.evaluate('(length 42)')

    def test_alist_length_with_non_alist(self, tool):
        """Test that alist-length with non-alist raises error."""
        with pytest.raises(AIFPLEvalError, match="alist-length requires alist argument"):
            tool.evaluate('(alist-length (list 1 2 3))')
