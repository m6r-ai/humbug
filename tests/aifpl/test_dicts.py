"""Tests for AIFPL dict (association list) operations."""

import pytest
from aifpl import AIFPL, AIFPLEvalError


@pytest.fixture
def tool():
    """Create AIFPL instance for testing."""
    return AIFPL()


class TestDictConstruction:
    """Test dict construction."""

    def test_empty_dict(self, tool):
        """Test creating an empty dict."""
        result = tool.evaluate("(dict)")
        assert result == {}

    def test_simple_dict(self, tool):
        """Test creating a simple dict with string keys."""
        result = tool.evaluate('(dict (list "name" "Alice") (list "age" 30))')
        assert result == {"name": "Alice", "age": 30}

    def test_dict_with_number_keys(self, tool):
        """Test dict with numeric keys."""
        result = tool.evaluate("(dict (list 1 \"one\") (list 2 \"two\") (list 3 \"three\"))")
        assert result == {"1": "one", "2": "two", "3": "three"}

    def test_dict_with_boolean_keys(self, tool):
        """Test dict with boolean keys."""
        result = tool.evaluate('(dict (list #t "true value") (list #f "false value"))')
        assert result == {"True": "true value", "False": "false value"}

    def test_dict_with_mixed_value_types(self, tool):
        """Test dict with different value types."""
        result = tool.evaluate('(dict (list "name" "Bob") (list "age" 25) (list "active" #t))')
        assert result == {"name": "Bob", "age": 25, "active": True}

    def test_dict_with_nested_values(self, tool):
        """Test dict with nested lists as values."""
        result = tool.evaluate('(dict (list "numbers" (list 1 2 3)) (list "letters" (list "a" "b")))')
        assert result == {"numbers": [1, 2, 3], "letters": ["a", "b"]}

    def test_dict_preserves_insertion_order(self, tool):
        """Test that dict preserves insertion order."""
        result = tool.evaluate('(dict (list "z" 1) (list "a" 2) (list "m" 3))')
        keys = list(result.keys())
        assert keys == ["z", "a", "m"]


class TestDictConstructionErrors:
    """Test dict construction error cases."""

    def test_dict_pair_not_list(self, tool):
        """Test error when pair is not a list."""
        with pytest.raises(AIFPLEvalError, match="Dict pair 1 must be a list"):
            tool.evaluate('(dict "not-a-pair")')

    def test_dict_pair_wrong_length(self, tool):
        """Test error when pair doesn't have exactly 2 elements."""
        with pytest.raises(AIFPLEvalError, match="Dict pair 1 must have exactly 2 elements"):
            tool.evaluate('(dict (list "key"))')

    def test_dict_pair_too_many_elements(self, tool):
        """Test error when pair has more than 2 elements."""
        with pytest.raises(AIFPLEvalError, match="Dict pair 1 must have exactly 2 elements"):
            tool.evaluate('(dict (list "key" "val1" "val2"))')

    def test_dict_invalid_key_type(self, tool):
        """Test error with invalid key type (list)."""
        with pytest.raises(AIFPLEvalError, match="Dict keys must be strings, numbers, booleans, or symbols"):
            tool.evaluate('(dict (list (list 1 2) "value"))')


class TestDictGet:
    """Test dict-get operation."""

    def test_dict_get_existing_key(self, tool):
        """Test getting an existing key."""
        result = tool.evaluate('(dict-get (dict (list "name" "Alice") (list "age" 30)) "name")')
        assert result == "Alice"

    def test_dict_get_missing_key_default_false(self, tool):
        """Test getting a missing key returns #f by default."""
        result = tool.evaluate('(dict-get (dict (list "name" "Alice")) "age")')
        assert result is None

    def test_dict_get_with_default(self, tool):
        """Test getting a missing key with custom default."""
        result = tool.evaluate('(dict-get (dict (list "name" "Alice")) "age" 0)')
        assert result == 0

    def test_dict_get_number_key(self, tool):
        """Test getting with numeric key."""
        result = tool.evaluate('(dict-get (dict (list 1 "one") (list 2 "two")) 2)')
        assert result == "two"

    def test_dict_get_from_nested_dict(self, tool):
        """Test getting from nested dicts."""
        result = tool.evaluate('''
            (let ((data (dict (list "user" (dict (list "name" "Bob") (list "id" 123))))))
              (dict-get (dict-get data "user") "name"))
        ''')
        assert result == "Bob"


class TestDictGetErrors:
    """Test dict-get error cases."""

    def test_dict_get_wrong_arg_count_too_few(self, tool):
        """Test error with too few arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-get (dict (list "a" 1)))')

    def test_dict_get_wrong_arg_count_too_many(self, tool):
        """Test error with too many arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-get (dict (list "a" 1)) "a" 0 "extra")')

    def test_dict_get_not_dict(self, tool):
        """Test error when first argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-get (list 1 2 3) "key")')


class TestDictSet:
    """Test dict-set operation."""

    def test_dict_set_new_key(self, tool):
        """Test setting a new key."""
        result = tool.evaluate('(dict-set (dict (list "name" "Alice")) "age" 30)')
        assert result == {"name": "Alice", "age": 30}

    def test_dict_set_existing_key(self, tool):
        """Test updating an existing key."""
        result = tool.evaluate('(dict-set (dict (list "name" "Alice") (list "age" 30)) "age" 31)')
        assert result == {"name": "Alice", "age": 31}

    def test_dict_set_immutable(self, tool):
        """Test that dict-set doesn't modify original."""
        result = tool.evaluate('''
            (let* ((original (dict (list "name" "Alice") (list "age" 30)))
                  (updated (dict-set original "age" 31)))
              (list (dict-get original "age") (dict-get updated "age")))
        ''')
        assert result == [30, 31]

    def test_dict_set_preserves_order(self, tool):
        """Test that dict-set preserves insertion order when updating."""
        result = tool.evaluate('(dict-set (dict (list "a" 1) (list "b" 2) (list "c" 3)) "b" 20)')
        keys = list(result.keys())
        assert keys == ["a", "b", "c"]


class TestDictSetErrors:
    """Test dict-set error cases."""

    def test_dict_set_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-set (dict (list "a" 1)) "b")')

    def test_dict_set_not_dict(self, tool):
        """Test error when first argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-set "not-dict" "key" "value")')


class TestDictHas:
    """Test dict-has? operation."""

    def test_dict_has_existing_key(self, tool):
        """Test checking for existing key."""
        result = tool.evaluate('(dict-has? (dict (list "name" "Alice") (list "age" 30)) "name")')
        assert result is True

    def test_dict_has_missing_key(self, tool):
        """Test checking for missing key."""
        result = tool.evaluate('(dict-has? (dict (list "name" "Alice")) "age")')
        assert result is False

    def test_dict_has_empty_dict(self, tool):
        """Test checking in empty dict."""
        result = tool.evaluate('(dict-has? (dict) "any-key")')
        assert result is False


class TestDictHasErrors:
    """Test dict-has? error cases."""

    def test_dict_has_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-has? (dict (list "a" 1)))')

    def test_dict_has_not_dict(self, tool):
        """Test error when first argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-has? 42 "key")')


class TestDictKeys:
    """Test dict-keys operation."""

    def test_dict_keys_simple(self, tool):
        """Test getting keys from dict."""
        result = tool.evaluate('(dict-keys (dict (list "name" "Alice") (list "age" 30) (list "city" "NYC")))')
        assert result == ["name", "age", "city"]

    def test_dict_keys_empty(self, tool):
        """Test getting keys from empty dict."""
        result = tool.evaluate('(dict-keys (dict))')
        assert result == []

    def test_dict_keys_preserves_order(self, tool):
        """Test that keys are returned in insertion order."""
        result = tool.evaluate('(dict-keys (dict (list "z" 1) (list "a" 2) (list "m" 3)))')
        assert result == ["z", "a", "m"]


class TestDictKeysErrors:
    """Test dict-keys error cases."""

    def test_dict_keys_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-keys)')

    def test_dict_keys_not_dict(self, tool):
        """Test error when argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-keys (list 1 2 3))')


class TestDictValues:
    """Test dict-values operation."""

    def test_dict_values_simple(self, tool):
        """Test getting values from dict."""
        result = tool.evaluate('(dict-values (dict (list "name" "Alice") (list "age" 30) (list "city" "NYC")))')
        assert result == ["Alice", 30, "NYC"]

    def test_dict_values_empty(self, tool):
        """Test getting values from empty dict."""
        result = tool.evaluate('(dict-values (dict))')
        assert result == []

    def test_dict_values_preserves_order(self, tool):
        """Test that values are returned in insertion order."""
        result = tool.evaluate('(dict-values (dict (list "z" 1) (list "a" 2) (list "m" 3)))')
        assert result == [1, 2, 3]


class TestDictValuesErrors:
    """Test dict-values error cases."""

    def test_dict_values_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-values (dict (list "a" 1)) "extra")')

    def test_dict_values_not_dict(self, tool):
        """Test error when argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-values #t)')


class TestDictRemove:
    """Test dict-remove operation."""

    def test_dict_remove_existing_key(self, tool):
        """Test removing an existing key."""
        result = tool.evaluate('(dict-remove (dict (list "name" "Alice") (list "age" 30) (list "city" "NYC")) "age")')
        assert result == {"name": "Alice", "city": "NYC"}

    def test_dict_remove_missing_key(self, tool):
        """Test removing a non-existent key (no-op)."""
        result = tool.evaluate('(dict-remove (dict (list "name" "Alice")) "age")')
        assert result == {"name": "Alice"}

    def test_dict_remove_immutable(self, tool):
        """Test that dict-remove doesn't modify original."""
        result = tool.evaluate('''
            (let* ((original (dict (list "name" "Alice") (list "age" 30)))
                   (removed (dict-remove original "age")))
              (list (dict-has? original "age") (dict-has? removed "age")))
        ''')
        assert result == [True, False]


class TestDictRemoveErrors:
    """Test dict-remove error cases."""

    def test_dict_remove_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-remove (dict (list "a" 1)))')

    def test_dict_remove_not_dict(self, tool):
        """Test error when first argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-remove (list 1 2) "key")')


class TestDictMerge:
    """Test dict-merge operation."""

    def test_dict_merge_no_conflicts(self, tool):
        """Test merging dicts with no overlapping keys."""
        result = tool.evaluate('(dict-merge (dict (list "a" 1) (list "b" 2)) (dict (list "c" 3) (list "d" 4)))')
        assert result == {"a": 1, "b": 2, "c": 3, "d": 4}

    def test_dict_merge_with_conflicts(self, tool):
        """Test merging dicts with overlapping keys (second wins)."""
        result = tool.evaluate('(dict-merge (dict (list "a" 1) (list "b" 2)) (dict (list "b" 20) (list "c" 3)))')
        assert result == {"a": 1, "b": 20, "c": 3}

    def test_dict_merge_empty_first(self, tool):
        """Test merging empty dict with non-empty."""
        result = tool.evaluate('(dict-merge (dict) (dict (list "a" 1) (list "b" 2)))')
        assert result == {"a": 1, "b": 2}

    def test_dict_merge_empty_second(self, tool):
        """Test merging non-empty with empty dict."""
        result = tool.evaluate('(dict-merge (dict (list "a" 1) (list "b" 2)) (dict))')
        assert result == {"a": 1, "b": 2}

    def test_dict_merge_preserves_order(self, tool):
        """Test that merge preserves first dict's order, then adds new keys."""
        result = tool.evaluate('(dict-merge (dict (list "z" 1) (list "a" 2)) (dict (list "m" 3) (list "b" 4)))')
        keys = list(result.keys())
        assert keys == ["z", "a", "m", "b"]


class TestDictMergeErrors:
    """Test dict-merge error cases."""

    def test_dict_merge_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict-merge (dict (list "a" 1)))')

    def test_dict_merge_first_not_dict(self, tool):
        """Test error when first argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-merge (list 1 2) (dict (list "a" 1)))')

    def test_dict_merge_second_not_dict(self, tool):
        """Test error when second argument is not an dict."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-merge (dict (list "a" 1)) "not-dict")')


class TestDictPredicate:
    """Test dict? type predicate."""

    def test_dict_predicate_true(self, tool):
        """Test dict? returns true for dict."""
        result = tool.evaluate('(dict? (dict (list "name" "Alice")))')
        assert result is True

    def test_dict_predicate_false_list(self, tool):
        """Test dict? returns false for list."""
        result = tool.evaluate('(dict? (list 1 2 3))')
        assert result is False

    def test_dict_predicate_false_number(self, tool):
        """Test dict? returns false for number."""
        result = tool.evaluate('(dict? 42)')
        assert result is False

    def test_dict_predicate_false_string(self, tool):
        """Test dict? returns false for string."""
        result = tool.evaluate('(dict? "hello")')
        assert result is False


class TestDictPredicateErrors:
    """Test dict? error cases."""

    def test_dict_predicate_wrong_arg_count(self, tool):
        """Test error with wrong number of arguments."""
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            tool.evaluate('(dict?)')


class TestDictFormatting:
    """Test dict formatting."""

    def test_dict_format_simple(self, tool):
        """Test formatting a simple dict."""
        result = tool.evaluate_and_format('(dict (list "name" "Alice") (list "age" 30))')
        assert result == '{("name" "Alice") ("age" 30)}'

    def test_dict_format_empty(self, tool):
        """Test formatting an empty dict."""
        result = tool.evaluate_and_format('(dict)')
        assert result == '{}'

    def test_dict_format_nested(self, tool):
        """Test formatting dict with nested values."""
        result = tool.evaluate_and_format('(dict (list "items" (list 1 2 3)))')
        assert result == '{("items" (1 2 3))}'


class TestDictWithFunctionalOperations:
    """Test dicts with higher-order functions."""

    def test_map_over_dict_keys(self, tool):
        """Test mapping over dict keys."""
        result = tool.evaluate('''
            (let ((data (dict (list "name" "Alice") (list "age" 30))))
              (list-map string-upcase (dict-keys data)))
        ''')
        assert result == ["NAME", "AGE"]

    def test_filter_dict_values(self, tool):
        """Test filtering dict values."""
        result = tool.evaluate('''
            (let* ((data (dict (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4))))
              (list-filter (lambda (v) (integer>? v 2)) (dict-values data)))
        ''')
        assert result == [3, 4]

    def test_fold_over_dict_values(self, tool):
        """Test folding over dict values."""
        result = tool.evaluate('''
            (let ((data (dict (list "a" 1) (list "b" 2) (list "c" 3))))
              (list-fold integer+ 0 (dict-values data)))
        ''')
        assert result == 6

    def test_process_list_of_dicts(self, tool):
        """Test processing a list of dicts."""
        result = tool.evaluate('''
            (let* ((people (list 
                           (dict (list "name" "Alice") (list "age" 30))
                           (dict (list "name" "Bob") (list "age" 25))
                           (dict (list "name" "Carol") (list "age" 35)))))
              (list-map (lambda (p) (dict-get p "name")) people))
        ''')
        assert result == ["Alice", "Bob", "Carol"]


class TestDictPatternMatching:
    """Test dicts with pattern matching."""

    def test_match_dict_type(self, tool):
        """Test matching dict type."""
        result = tool.evaluate('''
            (match (dict (list "name" "Alice"))
              ((? dict? a) "is-dict")
              (_ "not-dict"))
        ''')
        assert result == "is-dict"

    def test_match_dict_vs_list(self, tool):
        """Test distinguishing dict from list in pattern matching."""
        result = tool.evaluate('''
            (let ((process (lambda (data)
                             (match data
                               ((? dict? a) "dict")
                               ((? list? l) "list")
                               (_ "other")))))
              (list (process (dict (list "a" 1)))
                    (process (list 1 2 3))))
        ''')
        assert result == ["dict", "list"]

    def test_match_with_dict_operations(self, tool):
        """Test pattern matching combined with dict operations."""
        result = tool.evaluate('''
            (match (dict (list "type" "user") (list "name" "Alice"))
              ((? dict? data)
               (if (string=? (dict-get data "type") "user")
                   (dict-get data "name")
                   "unknown"))
              (_ "not-dict"))
        ''')
        assert result == "Alice"


class TestDictComplexScenarios:
    """Test complex scenarios with dicts."""

    def test_nested_dicts(self, tool):
        """Test nested dict structures."""
        result = tool.evaluate('''
            (let ((user (dict 
                         (list "name" "Alice")
                         (list "address" (dict 
                                     (list "city" "NYC")
                                     (list "zip" "10001"))))))
              (dict-get (dict-get user "address") "city"))
        ''')
        assert result == "NYC"

    def test_dict_transformation_pipeline(self, tool):
        """Test transforming dict through multiple operations."""
        result = tool.evaluate('''
            (let ((data (dict (list "a" 1) (list "b" 2) (list "c" 3))))
              (let* ((with-d (dict-set data "d" 4))
                     (without-b (dict-remove with-d "b"))
                     (updated-c (dict-set without-b "c" 30)))
                updated-c))
        ''')
        assert result == {"a": 1, "c": 30, "d": 4}

    def test_merge_multiple_dicts(self, tool):
        """Test merging multiple dicts."""
        result = tool.evaluate('''
            (let* ((defaults (dict (list "port" 8080) (list "host" "localhost")))
                  (config (dict (list "port" 3000)))
                  (overrides (dict (list "debug" #t))))
              (dict-merge (dict-merge defaults config) overrides))
        ''')
        assert result == {"port": 3000, "host": "localhost", "debug": True}

    def test_convert_list_to_dict(self, tool):
        """Test building dict from list data."""
        result = tool.evaluate('''
            (let* ((pairs (list (list "name" "Alice") (list "age" 30) (list "city" "NYC"))))
              (list-fold (lambda (acc pair)
                      (dict-set acc (list-first pair) (list-first (list-rest pair))))
                    (dict)
                    pairs))
        ''')
        assert result == {"name": "Alice", "age": 30, "city": "NYC"}


class TestDictEquality:
    """Test dict equality comparisons."""

    def test_dict_equality_same_content(self, tool):
        """Test that dicts with same content are equal."""
        result = tool.evaluate('(dict=? (dict (list "a" 1) (list "b" 2)) (dict (list "a" 1) (list "b" 2)))')
        assert result is True

    def test_dict_equality_different_content(self, tool):
        """Test that dicts with different content are not equal."""
        result = tool.evaluate('(dict=? (dict (list "a" 1) (list "b" 2)) (dict (list "a" 1) (list "b" 3)))')
        assert result is False

    def test_dict_equality_different_keys(self, tool):
        """Test that dicts with different keys are not equal."""
        result = tool.evaluate('(dict=? (dict (list "a" 1)) (dict (list "b" 1)))')
        assert result is False

    def test_dict_inequality(self, tool):
        """Test dict inequality operator."""
        result = tool.evaluate('(dict!=? (dict (list "a" 1)) (dict (list "a" 2)))')
        assert result is True


class TestDictLength:
    """Test dict length operations."""

    def test_length_with_dict(self, tool):
        """Test that length works with dicts."""
        result = tool.evaluate('(dict-length (dict (list "name" "Alice") (list "age" 30) (list "city" "NYC")))')
        assert result == 3

    def test_length_empty_dict(self, tool):
        """Test length of empty dict."""
        result = tool.evaluate('(dict-length (dict))')
        assert result == 0

    def test_length_single_entry_dict(self, tool):
        """Test length of dict with single entry."""
        result = tool.evaluate('(dict-length (dict (list "only" "one")))')
        assert result == 1

    def test_length_with_nested_dict(self, tool):
        """Test length of dict containing nested dicts."""
        result = tool.evaluate('''
            (dict-length (dict (list "user" (dict (list "name" "Bob") (list "id" 123)))))
        ''')
        assert result == 1

    def test_length_after_dict_set(self, tool):
        """Test length after adding entries with dict-set."""
        result = tool.evaluate('''
            (let ((a1 (dict (list "a" 1))))
              (let ((a2 (dict-set a1 "b" 2)))
                (let ((a3 (dict-set a2 "c" 3)))
                  (dict-length a3))))
        ''')
        assert result == 3

    def test_length_after_dict_remove(self, tool):
        """Test length after removing entries with dict-remove."""
        result = tool.evaluate('''
            (let ((a1 (dict (list "a" 1) (list "b" 2) (list "c" 3))))
              (dict-length (dict-remove a1 "b")))
        ''')
        assert result == 2

    def test_length_equals_keys_length(self, tool):
        """Test that length of dict equals length of its keys."""
        result = tool.evaluate('''
            (let ((my-dict (dict (list "a" 1) (list "b" 2) (list "c" 3))))
              (integer=? (dict-length my-dict) (list-length (dict-keys my-dict))))
        ''')
        assert result is True


class TestDictLengthFunction:
    """Test dict-length specific function."""

    def test_dict_length_basic(self, tool):
        """Test dict-length function."""
        result = tool.evaluate('(dict-length (dict (list "x" 1) (list "y" 2)))')
        assert result == 2

    def test_dict_length_empty(self, tool):
        """Test dict-length on empty dict."""
        result = tool.evaluate('(dict-length (dict))')
        assert result == 0

    def test_dict_length_in_expression(self, tool):
        """Test using dict-length in arithmetic expression."""
        result = tool.evaluate('''
            (let ((data (dict (list "a" 1) (list "b" 2))))
              (integer* (dict-length data) 10))
        ''')
        assert result == 20


class TestDictLengthErrors:
    """Test error handling for dict length operations."""

    def test_length_with_invalid_type(self, tool):
        """Test that length with invalid type raises error."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-length 42)')

    def test_dict_length_with_non_dict(self, tool):
        """Test that dict-length with non-dict raises error."""
        with pytest.raises(AIFPLEvalError, match="requires dict argument"):
            tool.evaluate('(dict-length (list 1 2 3))')
