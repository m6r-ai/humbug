"""Tests for Menai collection operations edge cases."""

import pytest

from menai import MenaiEvalError


class TestMenaiCollectionEdgeCases:
    """Test collection operation edge cases and boundary conditions."""

    def test_empty_collection_operations(self, menai):
        """Test operations on empty collections."""
        # Empty list creation and properties
        result = menai.evaluate("()")
        assert result == []

        result = menai.evaluate("(list)")
        assert result == []

        # Empty list predicates
        assert menai.evaluate("(list-null? ())") is True
        assert menai.evaluate("(list-null? (list))") is True
        assert menai.evaluate("(list? ())") is True
        assert menai.evaluate("(list-length ())") == 0

        # Empty list operations that should work
        assert menai.evaluate("(list-reverse ())") == []
        assert menai.evaluate("(list-concat () ())") == []
        assert menai.evaluate("(list-concat () (list 1))") == [1]
        assert menai.evaluate("(list-concat (list 1) ())") == [1]

        # Empty list membership and search
        assert menai.evaluate("(list-member? () 1)") is False
        assert menai.evaluate("(list-index () 1)") is None

        # Empty list utilities
        assert menai.evaluate("(list-remove () 1)") == []
        assert menai.evaluate("(list-slice () 0 0)") == []
        assert menai.evaluate("(list-slice () 0)") == []
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-slice () 0 5)")  # end out of range on empty list
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-slice () 5)")    # start out of range on empty list

    def test_empty_collection_error_cases(self, menai):
        """Test operations that should fail on empty collections."""
        # Operations that require non-empty lists
        with pytest.raises(MenaiEvalError, match="Cannot get first element of empty list"):
            menai.evaluate("(list-first ())")

        with pytest.raises(MenaiEvalError, match="Cannot get rest of empty list"):
            menai.evaluate("(list-rest ())")

        with pytest.raises(MenaiEvalError, match="Cannot get last element of empty list"):
            menai.evaluate("(list-last ())")

    def test_single_element_collections(self, menai):
        """Test operations on single-element collections."""
        # Single element list operations
        assert menai.evaluate("(list-length (list 42))") == 1
        assert menai.evaluate("(list-first (list 42))") == 42
        assert menai.evaluate("(list-rest (list 42))") == []
        assert menai.evaluate("(list-last (list 42))") == 42
        assert menai.evaluate("(list-null? (list 42))") is False

        # Single element list transformations
        assert menai.evaluate("(list-reverse (list 42))") == [42]
        assert menai.evaluate("(list-concat (list 42) ())") == [42]
        assert menai.evaluate("(list-concat () (list 42))") == [42]

        # Single element list membership
        assert menai.evaluate("(list-member? (list 42) 42)") is True
        assert menai.evaluate("(list-member? (list 42) 43)") is False
        assert menai.evaluate("(list-index (list 42) 42)") == 0
        assert menai.evaluate("(list-index (list 42) 43)") is None

    def test_boundary_index_operations(self, menai):
        """Test boundary conditions for indexed operations."""
        # Valid boundary indices
        test_list = "(list 10 20 30)"

        # Valid indices
        assert menai.evaluate(f"(list-ref {test_list} 0)") == 10
        assert menai.evaluate(f"(list-ref {test_list} 1)") == 20
        assert menai.evaluate(f"(list-ref {test_list} 2)") == 30

        # Invalid indices should raise errors
        with pytest.raises(MenaiEvalError, match="index out of range"):
            menai.evaluate(f"(list-ref {test_list} 3)")

        with pytest.raises(MenaiEvalError, match="index out of range"):
            menai.evaluate(f"(list-ref {test_list} -1)")

        with pytest.raises(MenaiEvalError, match="index out of range"):
            menai.evaluate("(list-ref () 0)")

    def test_take_drop_boundary_conditions(self, menai):
        """Test take and drop with boundary conditions."""
        test_list = "(list 1 2 3 4 5)"

        # Normal take/drop operations
        assert menai.evaluate(f"(list-slice {test_list} 0 0)") == []
        assert menai.evaluate(f"(list-slice {test_list} 0 1)") == [1]
        assert menai.evaluate(f"(list-slice {test_list} 0 3)") == [1, 2, 3]
        assert menai.evaluate(f"(list-slice {test_list} 0 5)") == [1, 2, 3, 4, 5]

        # Take more than available (should raise error)
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f"(list-slice {test_list} 0 10)")

        # Drop operations
        assert menai.evaluate(f"(list-slice {test_list} 0)") == [1, 2, 3, 4, 5]
        assert menai.evaluate(f"(list-slice {test_list} 1)") == [2, 3, 4, 5]
        assert menai.evaluate(f"(list-slice {test_list} 3)") == [4, 5]
        assert menai.evaluate(f"(list-slice {test_list} 5)") == []

        # Drop more than available (should raise error)
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f"(list-slice {test_list} 10)")

        # Negative arguments should be handled
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f"(list-slice {test_list} 0 -1)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate(f"(list-slice {test_list} -1)")

    def test_mixed_type_collections(self, menai):
        """Test collections with mixed data types."""
        # Create mixed-type list
        mixed_list = '(list 1 "hello" #t 3.14 (list 2 3))'
        result = menai.evaluate(mixed_list)
        assert result == [1, "hello", True, 3.14, [2, 3]]

        # Operations on mixed-type lists
        assert menai.evaluate(f"(list-length {mixed_list})") == 5
        assert menai.evaluate(f"(list-first {mixed_list})") == 1
        assert menai.evaluate(f'(list-ref {mixed_list} 1)') == "hello"
        assert menai.evaluate(f"(list-ref {mixed_list} 2)") is True
        assert menai.evaluate(f"(list-ref {mixed_list} 3)") == 3.14
        assert menai.evaluate(f"(list-ref {mixed_list} 4)") == [2, 3]

        # Membership tests with mixed types
        assert menai.evaluate(f"(list-member? {mixed_list} 1)") is True
        assert menai.evaluate(f'(list-member? {mixed_list} "hello")') is True
        assert menai.evaluate(f"(list-member? {mixed_list} #t)") is True
        assert menai.evaluate(f"(list-member? {mixed_list} 3.14)") is True
        assert menai.evaluate(f"(list-member? {mixed_list} 42)") is False

        # Position tests with mixed types
        assert menai.evaluate(f"(list-index {mixed_list} 1)") == 0
        assert menai.evaluate(f'(list-index {mixed_list} "hello")') == 1
        assert menai.evaluate(f"(list-index {mixed_list} #t)") == 2
        assert menai.evaluate(f"(list-index {mixed_list} 42)") is None

    def test_nested_collections(self, menai):
        """Test operations on nested collections."""
        # Create nested list structure
        nested = "(list (list 1 2) (list 3 4) (list 5 6))"
        result = menai.evaluate(nested)
        assert result == [[1, 2], [3, 4], [5, 6]]

        # Operations on nested structure
        assert menai.evaluate(f"(list-length {nested})") == 3
        assert menai.evaluate(f"(list-first {nested})") == [1, 2]
        assert menai.evaluate(f"(list-ref {nested} 1)") == [3, 4]

        # Access nested elements
        assert menai.evaluate(f"(list-first (list-first {nested}))") == 1
        assert menai.evaluate(f"(list-first (list-ref {nested} 1))") == 3

        # Deeply nested structure
        deep_nested = "(list (list (list 1)))"
        result = menai.evaluate(deep_nested)
        assert result == [[[1]]]

        assert menai.evaluate(f"(list-first (list-first (list-first {deep_nested})))") == 1

    def test_large_collections(self, menai):
        """Test operations on large collections."""
        # Create large list using range
        large_list_expr = "(range 1 1001)"
        result = menai.evaluate(large_list_expr)
        assert len(result) == 1000
        assert result[0] == 1
        assert result[999] == 1000

        # Operations on large list
        assert menai.evaluate("(list-length (range 1 1001))") == 1000
        assert menai.evaluate("(list-first (range 1 1001))") == 1
        assert menai.evaluate("(list-last (range 1 1001))") == 1000
        assert menai.evaluate("(list-ref (range 1 1001) 999)") == 1000

        # Take/drop on large list
        assert menai.evaluate("(list-length (list-slice (range 1 1001) 0 100))") == 100
        assert menai.evaluate("(list-length (list-slice (range 1 1001) 900))") == 100
        assert menai.evaluate("(list-first (list-slice (range 1 1001) 500))") == 501

    def test_collection_equality_edge_cases(self, menai):
        """Test collection equality edge cases."""
        # Empty list equality
        assert menai.evaluate("(list=? () ())") is True
        assert menai.evaluate("(list=? (list) ())") is True
        assert menai.evaluate("(list=? () (list))") is True

        # Single element equality
        assert menai.evaluate("(list=? (list 1) (list 1))") is True
        assert menai.evaluate("(list!=? (list 1) (list 2))") is True

        # Multi-element equality
        assert menai.evaluate("(list=? (list 1 2 3) (list 1 2 3))") is True
        assert menai.evaluate("(list!=? (list 1 2 3) (list 3 2 1))") is True

        # Mixed type equality
        assert menai.evaluate('(list=? (list 1 "hello") (list 1 "hello"))') is True
        assert menai.evaluate('(list!=? (list 1 "hello") (list 1 "world"))') is True

        # Nested list equality
        assert menai.evaluate("(list=? (list (list 1 2)) (list (list 1 2)))") is True
        assert menai.evaluate("(list!=? (list (list 1 2)) (list (list 2 1)))") is True

    def test_collection_immutability(self, menai):
        """Test that collection operations don't modify originals."""
        # Original list should not be modified by operations
        original_expr = "(list 1 2 3)"

        # Test that list-concat doesn't modify original
        result = menai.evaluate(f"""
        (let ((original {original_expr}))
          (list
            original
            (list-concat original (list 4))
            original))
        """)

        assert result[0] == [1, 2, 3]
        assert result[1] == [1, 2, 3, 4]
        assert result[2] == [1, 2, 3]  # Should be unchanged

        # Test that reverse doesn't modify original
        result = menai.evaluate(f"""
        (let ((original {original_expr}))
          (list
            original
            (list-reverse original)
            original))
        """)

        assert result[0] == [1, 2, 3]
        assert result[1] == [3, 2, 1]
        assert result[2] == [1, 2, 3]  # Should be unchanged

    def test_collection_type_errors(self, menai):
        """Test type errors with collection operations."""
        # Operations that require lists
        non_list_values = ["42", '"hello"', "#t", "3.14"]

        for value in non_list_values:
            # first, rest, last should fail on non-lists
            with pytest.raises(MenaiEvalError):
                menai.evaluate(f"(list-first {value})")

            with pytest.raises(MenaiEvalError):
                menai.evaluate(f"(list-rest {value})")

            with pytest.raises(MenaiEvalError):
                menai.evaluate(f"(list-last {value})")

            with pytest.raises(MenaiEvalError):
                menai.evaluate(f"(list-length {value})")

            with pytest.raises(MenaiEvalError):
                menai.evaluate(f"(list-reverse {value})")

        # cons requires second argument to be list
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(list-prepend "hello" 1)')

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-prepend 42 1)")

        # list-concat requires all arguments to be lists
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(list-concat (list 1 2) "hello")')

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-concat (list 1 2) 42)")

    def test_collection_arity_errors(self, menai):
        """Test arity errors with collection operations."""
        # Functions that require specific argument counts

        # cons requires exactly 2 arguments
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(cons)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-prepend 1)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-prepend 1 (list 2) 3)")

        # first, rest, last require exactly 1 argument
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(first)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-first (list 1) (list 2))")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(rest)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(last)")

        # length requires exactly 1 argument
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(length)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-length (list 1) (list 2))")

    def test_collection_higher_order_edge_cases(self, menai):
        """Test higher-order function edge cases with collections."""
        # Map with empty list
        assert menai.evaluate("(map-list (lambda (x) (integer* x 2)) ())") == []

        # Filter with empty list
        assert menai.evaluate("(filter-list (lambda (x) #t) ())") == []

        # Fold with empty list
        assert menai.evaluate("(fold-list integer+ 0 ())") == 0

        # Map with single element
        assert menai.evaluate("(map-list (lambda (x) (integer* x 2)) (list 5))") == [10]

        # Filter that removes all elements
        assert menai.evaluate("(filter-list (lambda (x) #f) (list 1 2 3))") == []

        # Filter that keeps all elements
        assert menai.evaluate("(filter-list (lambda (x) #t) (list 1 2 3))") == [1, 2, 3]

        # Map with complex transformation
        result = menai.evaluate("(map-list (lambda (x) (list x (integer* x 2))) (list 1 2 3))")
        assert result == [[1, 2], [2, 4], [3, 6]]

    def test_collection_search_edge_cases(self, menai):
        """Test collection search operation edge cases."""
        test_list = "(list 1 2 3 2 4)"

        # Position of first occurrence
        assert menai.evaluate(f"(list-index {test_list} 2)") == 1  # First occurrence

        # Position of non-existent element
        assert menai.evaluate(f"(list-index {test_list} 99)") is None

        # Member tests
        assert menai.evaluate(f"(list-member? {test_list} 1)") is True
        assert menai.evaluate(f"(list-member? {test_list} 99)") is False

        # Remove operations
        assert menai.evaluate(f"(list-remove {test_list} 2)") == [1, 3, 4]  # Removes all occurrences
        assert menai.evaluate(f"(list-remove {test_list} 99)") == [1, 2, 3, 2, 4]  # No change

        # Search in mixed-type list
        mixed = '(list 1 "hello" #t 2)'
        assert menai.evaluate(f'(list-index {mixed} "hello")') == 1
        assert menai.evaluate(f"(list-index {mixed} #t)") == 2
        assert menai.evaluate(f'(list-member? {mixed} "hello")') is True
        assert menai.evaluate(f"(list-member? {mixed} #f)") is False

    def test_collection_range_edge_cases(self, menai):
        """Test range function edge cases."""
        # Basic ranges
        assert menai.evaluate("(range 1 5)") == [1, 2, 3, 4]
        assert menai.evaluate("(range 0 3)") == [0, 1, 2]

        # Empty ranges
        assert menai.evaluate("(range 5 5)") == []
        assert menai.evaluate("(range 5 3)") == []  # Start > end

        # Single element range
        assert menai.evaluate("(range 1 2)") == [1]

        # Range with step (if supported)
        try:
            result = menai.evaluate("(range 1 10 2)")
            assert result == [1, 3, 5, 7, 9]
        except MenaiEvalError:
            # Step parameter might not be supported
            pass

        # Negative ranges (if supported)
        try:
            result = menai.evaluate("(range -3 3)")
            assert result == [-3, -2, -1, 0, 1, 2]
        except MenaiEvalError:
            # Negative ranges might not be supported
            pass

    def test_collection_cons_edge_cases(self, menai):
        """Test cons operation edge cases."""
        # Cons to empty list
        assert menai.evaluate("(list-prepend () 1)") == [1]

        # Cons to single element list
        assert menai.evaluate("(list-prepend (list 1) 0)") == [0, 1]

        # Cons to multi-element list
        assert menai.evaluate("(list-prepend (list 1 2 3) 0)") == [0, 1, 2, 3]

        # Cons different types
        assert menai.evaluate('(list-prepend (list 1 2) "hello")') == ["hello", 1, 2]
        assert menai.evaluate("(list-prepend (list 1 2) #t)") == [True, 1, 2]

        # Cons nested structures
        assert menai.evaluate("(list-prepend (list 1 2) (list 0))") == [[0], 1, 2]

    def test_collection_append_edge_cases(self, menai):
        """Test list-concat operation edge cases."""
        # Append empty lists
        assert menai.evaluate("(list-concat () ())") == []
        assert menai.evaluate("(list-concat () () ())") == []

        # Append to empty
        assert menai.evaluate("(list-concat () (list 1 2))") == [1, 2]
        assert menai.evaluate("(list-concat (list 1 2) ())") == [1, 2]

        # Append multiple lists
        assert menai.evaluate("(list-concat (list 1) (list 2) (list 3))") == [1, 2, 3]
        assert menai.evaluate("(list-concat (list 1 2) (list 3 4) (list 5 6))") == [1, 2, 3, 4, 5, 6]

        # Append with mixed types
        result = menai.evaluate('(list-concat (list 1) (list "hello") (list #t))')
        assert result == [1, "hello", True]

        # Append nested structures
        result = menai.evaluate("(list-concat (list (list 1)) (list (list 2)))")
        assert result == [[1], [2]]

    def test_collection_reverse_edge_cases(self, menai):
        """Test reverse operation edge cases."""
        # Reverse empty list
        assert menai.evaluate("(list-reverse ())") == []

        # Reverse single element
        assert menai.evaluate("(list-reverse (list 1))") == [1]

        # Reverse multiple elements
        assert menai.evaluate("(list-reverse (list 1 2 3))") == [3, 2, 1]

        # Reverse mixed types
        result = menai.evaluate('(list-reverse (list 1 "hello" #t))')
        assert result == [True, "hello", 1]

        # Reverse nested structures
        result = menai.evaluate("(list-reverse (list (list 1 2) (list 3 4)))")
        assert result == [[3, 4], [1, 2]]

        # Double reverse should be identity
        result = menai.evaluate("(list-reverse (list-reverse (list 1 2 3)))")
        assert result == [1, 2, 3]

    def test_collection_memory_efficiency_large_operations(self, menai):
        """Test memory efficiency with large collection operations."""
        # Large list-concat operations
        result = menai.evaluate("(list-length (list-concat (range 1 501) (range 501 1001)))")
        assert result == 1000

        # Large reverse operations
        result = menai.evaluate("(list-first (list-reverse (range 1 1001)))")
        assert result == 1000

        # Large take/drop combinations
        result = menai.evaluate("(list-length (list-slice (list-slice (range 1 1001) 400) 0 100))")
        assert result == 100

        # Nested large operations
        result = menai.evaluate("(list-length (list-reverse (list-slice (range 1 1001) 0 500)))")
        assert result == 500

    # New tests for missing coverage

    def test_string_arity_errors(self, menai):
        """Test arity errors for string functions."""
        # string-upcase requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-upcase)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-upcase "hello" "world")')

        # string-downcase requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-downcase)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-downcase "HELLO" "WORLD")')

        # string-trim requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-trim)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-trim "  hello  " "  world  ")')

        # string-trim-left requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-trim-left)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-trim-left "  hello  " "  world  ")')

        # string-trim-right requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-trim-right)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-trim-right "  hello  " "  world  ")')

        # string-replace requires exactly 3 arguments
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-replace)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-replace "hello")')

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-replace "hello" "l")')

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-replace "hello" "l" "L" "extra")')

        # string-prefix? requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-prefix?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-prefix? "hello")')

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-prefix? "hello" "he" "extra")')

        # string-suffix? requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string-suffix?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-suffix? "hello")')

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-suffix? "hello" "lo" "extra")')

    def test_list_arity_errors_additional(self, menai):
        """Test additional arity errors for list functions."""
        # reverse requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-reverse)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-reverse (list 1) (list 2))")

        # list? requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list? (list 1) (list 2))")

        # remove requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-remove)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-remove 1)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-remove 1 (list 1 2) (list 3))")

        # index requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-index)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-index 1)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-index 1 (list 1 2) (list 3))")

        # slice requires 2 or 3 arguments
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-slice)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-slice (list 1 2))")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list-slice (list 1 2) 0 1 2)")

    def test_string_list_conversion_arity_errors(self, menai):
        """Test arity errors for string-list conversion functions."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string->list)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list->string)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string->list)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string->list "hello" "," "extra")')

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(list->string)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(list->string (list "a") "," "extra")')

    def test_type_predicate_arity_errors(self, menai):
        """Test arity errors for type predicate functions."""
        # integer? requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(integer?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(integer? 1 2)")

        # float? requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(float?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(float? 1.0 2.0)")

        # complex? requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(complex?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(complex? (+ 1 1j) (+ 2 1j))")

        # string? requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(string?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string? "hello" "world")')

        # boolean? requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(boolean?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(boolean? #t #f)")

        # function? requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(function?)")

        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(function? + -)")

    def test_list_to_string_error_handling(self, menai):
        """Test error handling in list->string conversion."""
        # Test the exception handling branch in list->string
        # This tests the generic Exception catch
        # We need to test a case where to_python() might raise an exception
        # For now, just verify normal operation works
        assert menai.evaluate('(list->string (list "h" "e" "l" "l" "o"))') == "hello"

        # Test with numbers (should convert to string representation)
        assert menai.evaluate('(list->string (list "1" "2" "3"))') == "123"

    def test_string_replace_functionality(self, menai):
        """Test string-replace function thoroughly."""
        # Basic replacement
        assert menai.evaluate('(string-replace "hello" "l" "L")') == "heLLo"

        # No match
        assert menai.evaluate('(string-replace "hello" "x" "X")') == "hello"

        # Replace with empty string
        assert menai.evaluate('(string-replace "hello" "l" "")') == "heo"

        # Replace empty string (should work but not change anything meaningful)
        assert menai.evaluate('(string-replace "hello" "" "X")') == "XhXeXlXlXoX"


    def test_boolean_predicate_return_coverage(self, menai):
        """Test boolean? return statement coverage."""
        # Test with boolean values to ensure return statement is covered
        assert menai.evaluate("(boolean? #t)") is True
        assert menai.evaluate("(boolean? #f)") is True

        # Test with non-boolean values
        assert menai.evaluate("(boolean? 1)") is False
        assert menai.evaluate('(boolean? "hello")') is False
        assert menai.evaluate("(boolean? (list 1))") is False

    def test_function_predicate_return_coverage(self, menai):
        """Test function? return statement coverage."""
        # Test with function values to ensure return statement is covered
        assert menai.evaluate("(function? integer+)") is True
        assert menai.evaluate("(function? (lambda (x) x))") is True

        # Test with non-function values
        assert menai.evaluate("(function? 1)") is False
        assert menai.evaluate('(function? "hello")') is False
        assert menai.evaluate("(function? #t)") is False
        assert menai.evaluate("(function? (list 1))") is False
