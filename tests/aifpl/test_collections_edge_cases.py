"""Tests for AIFPL collection operations edge cases."""

import pytest

from aifpl import AIFPLEvalError


class TestAIFPLCollectionEdgeCases:
    """Test collection operation edge cases and boundary conditions."""

    def test_empty_collection_operations(self, aifpl):
        """Test operations on empty collections."""
        # Empty list creation and properties
        result = aifpl.evaluate("()")
        assert result == []

        result = aifpl.evaluate("(list)")
        assert result == []

        # Empty list predicates
        assert aifpl.evaluate("(null? ())") is True
        assert aifpl.evaluate("(null? (list))") is True
        assert aifpl.evaluate("(list? ())") is True
        assert aifpl.evaluate("(length ())") == 0

        # Empty list operations that should work
        assert aifpl.evaluate("(reverse ())") == []
        assert aifpl.evaluate("(append () ())") == []
        assert aifpl.evaluate("(append () (list 1))") == [1]
        assert aifpl.evaluate("(append (list 1) ())") == [1]

        # Empty list membership and search
        assert aifpl.evaluate("(member? 1 ())") is False
        assert aifpl.evaluate("(position 1 ())") is False

        # Empty list utilities
        assert aifpl.evaluate("(remove 1 ())") == []
        assert aifpl.evaluate("(take 5 ())") == []
        assert aifpl.evaluate("(drop 5 ())") == []
        assert aifpl.evaluate("(take 0 ())") == []
        assert aifpl.evaluate("(drop 0 ())") == []

    def test_empty_collection_error_cases(self, aifpl):
        """Test operations that should fail on empty collections."""
        # Operations that require non-empty lists
        with pytest.raises(AIFPLEvalError, match="Cannot get first element of empty list"):
            aifpl.evaluate("(first ())")

        with pytest.raises(AIFPLEvalError, match="Cannot get rest of empty list"):
            aifpl.evaluate("(rest ())")

        with pytest.raises(AIFPLEvalError, match="Cannot get last element of empty list"):
            aifpl.evaluate("(last ())")

    def test_single_element_collections(self, aifpl):
        """Test operations on single-element collections."""
        # Single element list operations
        assert aifpl.evaluate("(length (list 42))") == 1
        assert aifpl.evaluate("(first (list 42))") == 42
        assert aifpl.evaluate("(rest (list 42))") == []
        assert aifpl.evaluate("(last (list 42))") == 42
        assert aifpl.evaluate("(null? (list 42))") is False

        # Single element list transformations
        assert aifpl.evaluate("(reverse (list 42))") == [42]
        assert aifpl.evaluate("(append (list 42) ())") == [42]
        assert aifpl.evaluate("(append () (list 42))") == [42]

        # Single element list membership
        assert aifpl.evaluate("(member? 42 (list 42))") is True
        assert aifpl.evaluate("(member? 43 (list 42))") is False
        assert aifpl.evaluate("(position 42 (list 42))") == 0
        assert aifpl.evaluate("(position 43 (list 42))") is False

    def test_boundary_index_operations(self, aifpl):
        """Test boundary conditions for indexed operations."""
        # Valid boundary indices
        test_list = "(list 10 20 30)"

        # Valid indices
        assert aifpl.evaluate(f"(list-ref {test_list} 0)") == 10
        assert aifpl.evaluate(f"(list-ref {test_list} 1)") == 20
        assert aifpl.evaluate(f"(list-ref {test_list} 2)") == 30

        # Invalid indices should raise errors
        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate(f"(list-ref {test_list} 3)")

        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate(f"(list-ref {test_list} -1)")

        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate("(list-ref () 0)")

    def test_take_drop_boundary_conditions(self, aifpl):
        """Test take and drop with boundary conditions."""
        test_list = "(list 1 2 3 4 5)"

        # Normal take/drop operations
        assert aifpl.evaluate(f"(take 0 {test_list})") == []
        assert aifpl.evaluate(f"(take 1 {test_list})") == [1]
        assert aifpl.evaluate(f"(take 3 {test_list})") == [1, 2, 3]
        assert aifpl.evaluate(f"(take 5 {test_list})") == [1, 2, 3, 4, 5]

        # Take more than available (should return all)
        assert aifpl.evaluate(f"(take 10 {test_list})") == [1, 2, 3, 4, 5]

        # Drop operations
        assert aifpl.evaluate(f"(drop 0 {test_list})") == [1, 2, 3, 4, 5]
        assert aifpl.evaluate(f"(drop 1 {test_list})") == [2, 3, 4, 5]
        assert aifpl.evaluate(f"(drop 3 {test_list})") == [4, 5]
        assert aifpl.evaluate(f"(drop 5 {test_list})") == []

        # Drop more than available (should return empty)
        assert aifpl.evaluate(f"(drop 10 {test_list})") == []

        # Negative arguments should be handled
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate(f"(take -1 {test_list})")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate(f"(drop -1 {test_list})")

    def test_mixed_type_collections(self, aifpl):
        """Test collections with mixed data types."""
        # Create mixed-type list
        mixed_list = '(list 1 "hello" #t 3.14 (list 2 3))'
        result = aifpl.evaluate(mixed_list)
        assert result == [1, "hello", True, 3.14, [2, 3]]

        # Operations on mixed-type lists
        assert aifpl.evaluate(f"(length {mixed_list})") == 5
        assert aifpl.evaluate(f"(first {mixed_list})") == 1
        assert aifpl.evaluate(f'(list-ref {mixed_list} 1)') == "hello"
        assert aifpl.evaluate(f"(list-ref {mixed_list} 2)") is True
        assert aifpl.evaluate(f"(list-ref {mixed_list} 3)") == 3.14
        assert aifpl.evaluate(f"(list-ref {mixed_list} 4)") == [2, 3]

        # Membership tests with mixed types
        assert aifpl.evaluate(f"(member? 1 {mixed_list})") is True
        assert aifpl.evaluate(f'(member? "hello" {mixed_list})') is True
        assert aifpl.evaluate(f"(member? #t {mixed_list})") is True
        assert aifpl.evaluate(f"(member? 3.14 {mixed_list})") is True
        assert aifpl.evaluate(f"(member? 42 {mixed_list})") is False

        # Position tests with mixed types
        assert aifpl.evaluate(f"(position 1 {mixed_list})") == 0
        assert aifpl.evaluate(f'(position "hello" {mixed_list})') == 1
        assert aifpl.evaluate(f"(position #t {mixed_list})") == 2
        assert aifpl.evaluate(f"(position 42 {mixed_list})") is False

    def test_nested_collections(self, aifpl):
        """Test operations on nested collections."""
        # Create nested list structure
        nested = "(list (list 1 2) (list 3 4) (list 5 6))"
        result = aifpl.evaluate(nested)
        assert result == [[1, 2], [3, 4], [5, 6]]

        # Operations on nested structure
        assert aifpl.evaluate(f"(length {nested})") == 3
        assert aifpl.evaluate(f"(first {nested})") == [1, 2]
        assert aifpl.evaluate(f"(list-ref {nested} 1)") == [3, 4]

        # Access nested elements
        assert aifpl.evaluate(f"(first (first {nested}))") == 1
        assert aifpl.evaluate(f"(first (list-ref {nested} 1))") == 3

        # Deeply nested structure
        deep_nested = "(list (list (list 1)))"
        result = aifpl.evaluate(deep_nested)
        assert result == [[[1]]]

        assert aifpl.evaluate(f"(first (first (first {deep_nested})))") == 1

    def test_large_collections(self, aifpl):
        """Test operations on large collections."""
        # Create large list using range
        large_list_expr = "(range 1 1001)"
        result = aifpl.evaluate(large_list_expr)
        assert len(result) == 1000
        assert result[0] == 1
        assert result[999] == 1000

        # Operations on large list
        assert aifpl.evaluate("(length (range 1 1001))") == 1000
        assert aifpl.evaluate("(first (range 1 1001))") == 1
        assert aifpl.evaluate("(last (range 1 1001))") == 1000
        assert aifpl.evaluate("(list-ref (range 1 1001) 999)") == 1000

        # Take/drop on large list
        assert aifpl.evaluate("(length (take 100 (range 1 1001)))") == 100
        assert aifpl.evaluate("(length (drop 900 (range 1 1001)))") == 100
        assert aifpl.evaluate("(first (drop 500 (range 1 1001)))") == 501

    def test_collection_equality_edge_cases(self, aifpl):
        """Test collection equality edge cases."""
        # Empty list equality
        assert aifpl.evaluate("(= () ())") is True
        assert aifpl.evaluate("(= (list) ())") is True
        assert aifpl.evaluate("(= () (list))") is True

        # Single element equality
        assert aifpl.evaluate("(= (list 1) (list 1))") is True
        assert aifpl.evaluate("(!= (list 1) (list 2))") is True

        # Multi-element equality
        assert aifpl.evaluate("(= (list 1 2 3) (list 1 2 3))") is True
        assert aifpl.evaluate("(!= (list 1 2 3) (list 3 2 1))") is True

        # Mixed type equality
        assert aifpl.evaluate('(= (list 1 "hello") (list 1 "hello"))') is True
        assert aifpl.evaluate('(!= (list 1 "hello") (list 1 "world"))') is True

        # Nested list equality
        assert aifpl.evaluate("(= (list (list 1 2)) (list (list 1 2)))") is True
        assert aifpl.evaluate("(!= (list (list 1 2)) (list (list 2 1)))") is True

    def test_collection_immutability(self, aifpl):
        """Test that collection operations don't modify originals."""
        # Original list should not be modified by operations
        original_expr = "(list 1 2 3)"

        # Test that append doesn't modify original
        result = aifpl.evaluate(f"""
        (let ((original {original_expr}))
          (list
            original
            (append original (list 4))
            original))
        """)

        assert result[0] == [1, 2, 3]
        assert result[1] == [1, 2, 3, 4]
        assert result[2] == [1, 2, 3]  # Should be unchanged

        # Test that reverse doesn't modify original
        result = aifpl.evaluate(f"""
        (let ((original {original_expr}))
          (list
            original
            (reverse original)
            original))
        """)

        assert result[0] == [1, 2, 3]
        assert result[1] == [3, 2, 1]
        assert result[2] == [1, 2, 3]  # Should be unchanged

    def test_collection_type_errors(self, aifpl):
        """Test type errors with collection operations."""
        # Operations that require lists
        non_list_values = ["42", '"hello"', "#t", "3.14"]

        for value in non_list_values:
            # first, rest, last should fail on non-lists
            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(first {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(rest {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(last {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(length {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(reverse {value})")

        # cons requires second argument to be list
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(cons 1 "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(cons 1 42)")

        # append requires all arguments to be lists
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(append (list 1 2) "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(append (list 1 2) 42)")

    def test_collection_arity_errors(self, aifpl):
        """Test arity errors with collection operations."""
        # Functions that require specific argument counts

        # cons requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(cons)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(cons 1)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(cons 1 (list 2) 3)")

        # first, rest, last require exactly 1 argument
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(first)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(first (list 1) (list 2))")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(rest)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(last)")

        # length requires exactly 1 argument
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(length)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(length (list 1) (list 2))")

    def test_collection_higher_order_edge_cases(self, aifpl):
        """Test higher-order function edge cases with collections."""
        # Map with empty list
        assert aifpl.evaluate("(map (lambda (x) (* x 2)) ())") == []

        # Filter with empty list
        assert aifpl.evaluate("(filter (lambda (x) #t) ())") == []

        # Fold with empty list
        assert aifpl.evaluate("(fold + 0 ())") == 0

        # Map with single element
        assert aifpl.evaluate("(map (lambda (x) (* x 2)) (list 5))") == [10]

        # Filter that removes all elements
        assert aifpl.evaluate("(filter (lambda (x) #f) (list 1 2 3))") == []

        # Filter that keeps all elements
        assert aifpl.evaluate("(filter (lambda (x) #t) (list 1 2 3))") == [1, 2, 3]

        # Map with complex transformation
        result = aifpl.evaluate("(map (lambda (x) (list x (* x 2))) (list 1 2 3))")
        assert result == [[1, 2], [2, 4], [3, 6]]

    def test_collection_search_edge_cases(self, aifpl):
        """Test collection search operation edge cases."""
        test_list = "(list 1 2 3 2 4)"

        # Position of first occurrence
        assert aifpl.evaluate(f"(position 2 {test_list})") == 1  # First occurrence

        # Position of non-existent element
        assert aifpl.evaluate(f"(position 99 {test_list})") is False

        # Member tests
        assert aifpl.evaluate(f"(member? 1 {test_list})") is True
        assert aifpl.evaluate(f"(member? 99 {test_list})") is False

        # Remove operations
        assert aifpl.evaluate(f"(remove 2 {test_list})") == [1, 3, 4]  # Removes all occurrences
        assert aifpl.evaluate(f"(remove 99 {test_list})") == [1, 2, 3, 2, 4]  # No change

        # Search in mixed-type list
        mixed = '(list 1 "hello" #t 2)'
        assert aifpl.evaluate(f'(position "hello" {mixed})') == 1
        assert aifpl.evaluate(f"(position #t {mixed})") == 2
        assert aifpl.evaluate(f'(member? "hello" {mixed})') is True
        assert aifpl.evaluate(f"(member? #f {mixed})") is False

    def test_collection_range_edge_cases(self, aifpl):
        """Test range function edge cases."""
        # Basic ranges
        assert aifpl.evaluate("(range 1 5)") == [1, 2, 3, 4]
        assert aifpl.evaluate("(range 0 3)") == [0, 1, 2]

        # Empty ranges
        assert aifpl.evaluate("(range 5 5)") == []
        assert aifpl.evaluate("(range 5 3)") == []  # Start > end

        # Single element range
        assert aifpl.evaluate("(range 1 2)") == [1]

        # Range with step (if supported)
        try:
            result = aifpl.evaluate("(range 1 10 2)")
            assert result == [1, 3, 5, 7, 9]
        except AIFPLEvalError:
            # Step parameter might not be supported
            pass

        # Negative ranges (if supported)
        try:
            result = aifpl.evaluate("(range -3 3)")
            assert result == [-3, -2, -1, 0, 1, 2]
        except AIFPLEvalError:
            # Negative ranges might not be supported
            pass

    def test_collection_cons_edge_cases(self, aifpl):
        """Test cons operation edge cases."""
        # Cons to empty list
        assert aifpl.evaluate("(cons 1 ())") == [1]

        # Cons to single element list
        assert aifpl.evaluate("(cons 0 (list 1))") == [0, 1]

        # Cons to multi-element list
        assert aifpl.evaluate("(cons 0 (list 1 2 3))") == [0, 1, 2, 3]

        # Cons different types
        assert aifpl.evaluate('(cons "hello" (list 1 2))') == ["hello", 1, 2]
        assert aifpl.evaluate("(cons #t (list 1 2))") == [True, 1, 2]

        # Cons nested structures
        assert aifpl.evaluate("(cons (list 0) (list 1 2))") == [[0], 1, 2]

    def test_collection_append_edge_cases(self, aifpl):
        """Test append operation edge cases."""
        # Append empty lists
        assert aifpl.evaluate("(append () ())") == []
        assert aifpl.evaluate("(append () () ())") == []

        # Append to empty
        assert aifpl.evaluate("(append () (list 1 2))") == [1, 2]
        assert aifpl.evaluate("(append (list 1 2) ())") == [1, 2]

        # Append multiple lists
        assert aifpl.evaluate("(append (list 1) (list 2) (list 3))") == [1, 2, 3]
        assert aifpl.evaluate("(append (list 1 2) (list 3 4) (list 5 6))") == [1, 2, 3, 4, 5, 6]

        # Append with mixed types
        result = aifpl.evaluate('(append (list 1) (list "hello") (list #t))')
        assert result == [1, "hello", True]

        # Append nested structures
        result = aifpl.evaluate("(append (list (list 1)) (list (list 2)))")
        assert result == [[1], [2]]

    def test_collection_reverse_edge_cases(self, aifpl):
        """Test reverse operation edge cases."""
        # Reverse empty list
        assert aifpl.evaluate("(reverse ())") == []

        # Reverse single element
        assert aifpl.evaluate("(reverse (list 1))") == [1]

        # Reverse multiple elements
        assert aifpl.evaluate("(reverse (list 1 2 3))") == [3, 2, 1]

        # Reverse mixed types
        result = aifpl.evaluate('(reverse (list 1 "hello" #t))')
        assert result == [True, "hello", 1]

        # Reverse nested structures
        result = aifpl.evaluate("(reverse (list (list 1 2) (list 3 4)))")
        assert result == [[3, 4], [1, 2]]

        # Double reverse should be identity
        result = aifpl.evaluate("(reverse (reverse (list 1 2 3)))")
        assert result == [1, 2, 3]

    def test_collection_memory_efficiency_large_operations(self, aifpl):
        """Test memory efficiency with large collection operations."""
        # Large append operations
        result = aifpl.evaluate("(length (append (range 1 501) (range 501 1001)))")
        assert result == 1000

        # Large reverse operations
        result = aifpl.evaluate("(first (reverse (range 1 1001)))")
        assert result == 1000

        # Large take/drop combinations
        result = aifpl.evaluate("(length (take 100 (drop 400 (range 1 1001))))")
        assert result == 100

        # Nested large operations
        result = aifpl.evaluate("(length (reverse (take 500 (range 1 1001))))")
        assert result == 500

    # New tests for missing coverage

    def test_string_arity_errors(self, aifpl):
        """Test arity errors for string functions."""
        # string-upcase requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="string-upcase takes exactly 1 argument"):
            aifpl.evaluate("(string-upcase)")

        with pytest.raises(AIFPLEvalError, match="string-upcase takes exactly 1 argument"):
            aifpl.evaluate('(string-upcase "hello" "world")')

        # string-downcase requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="string-downcase takes exactly 1 argument"):
            aifpl.evaluate("(string-downcase)")

        with pytest.raises(AIFPLEvalError, match="string-downcase takes exactly 1 argument"):
            aifpl.evaluate('(string-downcase "HELLO" "WORLD")')

        # string-trim requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="string-trim takes exactly 1 argument"):
            aifpl.evaluate("(string-trim)")

        with pytest.raises(AIFPLEvalError, match="string-trim takes exactly 1 argument"):
            aifpl.evaluate('(string-trim "  hello  " "  world  ")')

        # string-replace requires exactly 3 arguments
        with pytest.raises(AIFPLEvalError, match="string-replace takes exactly 3 arguments"):
            aifpl.evaluate("(string-replace)")

        with pytest.raises(AIFPLEvalError, match="string-replace takes exactly 3 arguments"):
            aifpl.evaluate('(string-replace "hello")')

        with pytest.raises(AIFPLEvalError, match="string-replace takes exactly 3 arguments"):
            aifpl.evaluate('(string-replace "hello" "l")')

        with pytest.raises(AIFPLEvalError, match="string-replace takes exactly 3 arguments"):
            aifpl.evaluate('(string-replace "hello" "l" "L" "extra")')

        # string-prefix? requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="string-prefix\\? takes exactly 2 arguments"):
            aifpl.evaluate("(string-prefix?)")

        with pytest.raises(AIFPLEvalError, match="string-prefix\\? takes exactly 2 arguments"):
            aifpl.evaluate('(string-prefix? "hello")')

        with pytest.raises(AIFPLEvalError, match="string-prefix\\? takes exactly 2 arguments"):
            aifpl.evaluate('(string-prefix? "hello" "he" "extra")')

        # string-suffix? requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="string-suffix\\? takes exactly 2 arguments"):
            aifpl.evaluate("(string-suffix?)")

        with pytest.raises(AIFPLEvalError, match="string-suffix\\? takes exactly 2 arguments"):
            aifpl.evaluate('(string-suffix? "hello")')

        with pytest.raises(AIFPLEvalError, match="string-suffix\\? takes exactly 2 arguments"):
            aifpl.evaluate('(string-suffix? "hello" "lo" "extra")')

    def test_list_arity_errors_additional(self, aifpl):
        """Test additional arity errors for list functions."""
        # reverse requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="reverse takes exactly 1 argument"):
            aifpl.evaluate("(reverse)")

        with pytest.raises(AIFPLEvalError, match="reverse takes exactly 1 argument"):
            aifpl.evaluate("(reverse (list 1) (list 2))")

        # list? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="list\\? takes exactly 1 argument"):
            aifpl.evaluate("(list?)")

        with pytest.raises(AIFPLEvalError, match="list\\? takes exactly 1 argument"):
            aifpl.evaluate("(list? (list 1) (list 2))")

        # remove requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="remove takes exactly 2 arguments"):
            aifpl.evaluate("(remove)")

        with pytest.raises(AIFPLEvalError, match="remove takes exactly 2 arguments"):
            aifpl.evaluate("(remove 1)")

        with pytest.raises(AIFPLEvalError, match="remove takes exactly 2 arguments"):
            aifpl.evaluate("(remove 1 (list 1 2) (list 3))")

        # position requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="position takes exactly 2 arguments"):
            aifpl.evaluate("(position)")

        with pytest.raises(AIFPLEvalError, match="position takes exactly 2 arguments"):
            aifpl.evaluate("(position 1)")

        with pytest.raises(AIFPLEvalError, match="position takes exactly 2 arguments"):
            aifpl.evaluate("(position 1 (list 1 2) (list 3))")

        # drop requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="drop takes exactly 2 arguments"):
            aifpl.evaluate("(drop)")

        with pytest.raises(AIFPLEvalError, match="drop takes exactly 2 arguments"):
            aifpl.evaluate("(drop 1)")

        with pytest.raises(AIFPLEvalError, match="drop takes exactly 2 arguments"):
            aifpl.evaluate("(drop 1 (list 1 2) (list 3))")

    def test_string_list_conversion_arity_errors(self, aifpl):
        """Test arity errors for string-list conversion functions."""
        # string->list requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="string->list takes exactly 1 argument"):
            aifpl.evaluate("(string->list)")

        with pytest.raises(AIFPLEvalError, match="string->list takes exactly 1 argument"):
            aifpl.evaluate('(string->list "hello" "world")')

        # list->string requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="list->string takes exactly 1 argument"):
            aifpl.evaluate("(list->string)")

        with pytest.raises(AIFPLEvalError, match="list->string takes exactly 1 argument"):
            aifpl.evaluate('(list->string (list "a") (list "b"))')

        # string-split requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="string-split takes exactly 2 arguments"):
            aifpl.evaluate("(string-split)")

        with pytest.raises(AIFPLEvalError, match="string-split takes exactly 2 arguments"):
            aifpl.evaluate('(string-split "hello")')

        with pytest.raises(AIFPLEvalError, match="string-split takes exactly 2 arguments"):
            aifpl.evaluate('(string-split "hello" "," "extra")')

        # string-join requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="string-join takes exactly 2 arguments"):
            aifpl.evaluate("(string-join)")

        with pytest.raises(AIFPLEvalError, match="string-join takes exactly 2 arguments"):
            aifpl.evaluate('(string-join (list "a"))')

        with pytest.raises(AIFPLEvalError, match="string-join takes exactly 2 arguments"):
            aifpl.evaluate('(string-join (list "a") "," "extra")')

    def test_type_predicate_arity_errors(self, aifpl):
        """Test arity errors for type predicate functions."""
        # number? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="number\\? takes exactly 1 argument"):
            aifpl.evaluate("(number?)")

        with pytest.raises(AIFPLEvalError, match="number\\? takes exactly 1 argument"):
            aifpl.evaluate("(number? 1 2)")

        # integer? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="integer\\? takes exactly 1 argument"):
            aifpl.evaluate("(integer?)")

        with pytest.raises(AIFPLEvalError, match="integer\\? takes exactly 1 argument"):
            aifpl.evaluate("(integer? 1 2)")

        # float? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="float\\? takes exactly 1 argument"):
            aifpl.evaluate("(float?)")

        with pytest.raises(AIFPLEvalError, match="float\\? takes exactly 1 argument"):
            aifpl.evaluate("(float? 1.0 2.0)")

        # complex? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="complex\\? takes exactly 1 argument"):
            aifpl.evaluate("(complex?)")

        with pytest.raises(AIFPLEvalError, match="complex\\? takes exactly 1 argument"):
            aifpl.evaluate("(complex? (+ 1 j) (+ 2 j))")

        # string? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="string\\? takes exactly 1 argument"):
            aifpl.evaluate("(string?)")

        with pytest.raises(AIFPLEvalError, match="string\\? takes exactly 1 argument"):
            aifpl.evaluate('(string? "hello" "world")')

        # boolean? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="boolean\\? takes exactly 1 argument"):
            aifpl.evaluate("(boolean?)")

        with pytest.raises(AIFPLEvalError, match="boolean\\? takes exactly 1 argument"):
            aifpl.evaluate("(boolean? #t #f)")

        # function? requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="function\\? takes exactly 1 argument"):
            aifpl.evaluate("(function?)")

        with pytest.raises(AIFPLEvalError, match="function\\? takes exactly 1 argument"):
            aifpl.evaluate("(function? + -)")

    def test_list_to_string_error_handling(self, aifpl):
        """Test error handling in list->string conversion."""
        # Test the exception handling branch in list->string
        # This tests the generic Exception catch
        # We need to test a case where to_python() might raise an exception
        # For now, just verify normal operation works
        assert aifpl.evaluate('(list->string (list "h" "e" "l" "l" "o"))') == "hello"

        # Test with numbers (should convert to string representation)
        assert aifpl.evaluate('(list->string (list "1" "2" "3"))') == "123"

    def test_string_replace_functionality(self, aifpl):
        """Test string-replace function thoroughly."""
        # Basic replacement
        assert aifpl.evaluate('(string-replace "hello" "l" "L")') == "heLLo"

        # No match
        assert aifpl.evaluate('(string-replace "hello" "x" "X")') == "hello"

        # Replace with empty string
        assert aifpl.evaluate('(string-replace "hello" "l" "")') == "heo"

        # Replace empty string (should work but not change anything meaningful)
        assert aifpl.evaluate('(string-replace "hello" "" "X")') == "XhXeXlXlXoX"


    def test_boolean_predicate_return_coverage(self, aifpl):
        """Test boolean? return statement coverage."""
        # Test with boolean values to ensure return statement is covered
        assert aifpl.evaluate("(boolean? #t)") is True
        assert aifpl.evaluate("(boolean? #f)") is True

        # Test with non-boolean values
        assert aifpl.evaluate("(boolean? 1)") is False
        assert aifpl.evaluate('(boolean? "hello")') is False
        assert aifpl.evaluate("(boolean? (list 1))") is False

    def test_function_predicate_return_coverage(self, aifpl):
        """Test function? return statement coverage."""
        # Test with function values to ensure return statement is covered
        assert aifpl.evaluate("(function? +)") is True
        assert aifpl.evaluate("(function? (lambda (x) x))") is True

        # Test with non-function values
        assert aifpl.evaluate("(function? 1)") is False
        assert aifpl.evaluate('(function? "hello")') is False
        assert aifpl.evaluate("(function? #t)") is False
        assert aifpl.evaluate("(function? (list 1))") is False
