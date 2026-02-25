"""Tests for list operations and list manipulation functions."""

import pytest

from aifpl import AIFPL, AIFPLEvalError


class TestLists:
    """Test list operations and manipulation functions."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic list construction
        ('(list)', '()'),
        ('(list 1)', '(1)'),
        ('(list 1 2)', '(1 2)'),
        ('(list 1 2 3)', '(1 2 3)'),

        # Mixed type lists
        ('(list 1 "hello" #t)', '(1 "hello" #t)'),
        ('(list "a" 2 #f 3.14)', '("a" 2 #f 3.14)'),

        # Nested lists
        ('(list (list 1 2) (list 3 4))', '((1 2) (3 4))'),
        ('(list 1 (list 2 3) 4)', '(1 (2 3) 4)'),

        # Lists with complex numbers
        ('(list (integer->complex 1 2) 1j)', '(1+2j 1j)'),
    ])
    def test_list_construction(self, aifpl, expression, expected):
        """Test list construction with various element types."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_list_construction_python_objects(self, aifpl):
        """Test that list construction returns proper Python lists."""
        result = aifpl.evaluate('(list 1 2 3)')
        assert result == [1, 2, 3]
        assert isinstance(result, list)

        # Mixed types
        result = aifpl.evaluate('(list 1 "hello" #t)')
        assert result == [1, "hello", True]

        # Nested lists
        result = aifpl.evaluate('(list (list 1 2) (list 3 4))')
        assert result == [[1, 2], [3, 4]]

    @pytest.mark.parametrize("expression,expected", [
        # Basic cons operations
        ('(list-prepend (list 2 3) 1)', '(1 2 3)'),
        ('(list-prepend (list "world") "hello")', '("hello" "world")'),
        ('(list-prepend (list #f) #t)', '(#t #f)'),

        # Cons with empty list
        ('(list-prepend (list) 1)', '(1)'),

        # Cons with mixed types
        ('(list-prepend (list "hello" #t) 1)', '(1 "hello" #t)'),

        # Nested cons
        ('(list-prepend (list (list 3 4)) (list 1 2))', '((1 2) (3 4))'),
    ])
    def test_cons_operation(self, aifpl, expression, expected):
        """Test cons operation for prepending elements."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_cons_requires_list_as_second_argument(self, aifpl):
        """Test that list-prepend requires a list as the first argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-prepend 2 1)')  # First arg must be list

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-prepend "hello" 1)')  # First arg must be list

    @pytest.mark.parametrize("expression,expected", [
        # Basic list-concat operations
        # Zero-arg and one-arg identity cases
        ('(list-concat)', '()'),
        ('(list-concat (list 1 2))', '(1 2)'),
        ('(list-concat (list 1 2) (list 3 4))', '(1 2 3 4)'),
        ('(list-concat (list) (list 1 2))', '(1 2)'),
        ('(list-concat (list 1 2) (list))', '(1 2)'),
        ('(list-concat (list) (list))', '()'),

        # Multiple list-concat
        ('(list-concat (list 1) (list 2) (list 3))', '(1 2 3)'),
        ('(list-concat (list 1 2) (list 3 4) (list 5 6))', '(1 2 3 4 5 6)'),

        # Mixed type list-concat
        ('(list-concat (list 1 "hello") (list #t 3.14))', '(1 "hello" #t 3.14)'),

        # Nested list-concat
        ('(list-concat (list (list 1 2)) (list (list 3 4)))', '((1 2) (3 4))'),
    ])
    def test_append_operation(self, aifpl, expression, expected):
        """Test list-concat operation for concatenating lists."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_append_requires_all_list_arguments(self, aifpl):
        """Test that list-concat requires all arguments to be lists."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-concat (list 1 2) 3)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-concat "hello" (list 1 2))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-concat (list 1) #t (list 2))')

    def test_append_zero_arg_identity(self, aifpl):
        """Test that (list-concat) returns the empty list identity, consistent with (integer+) → 0."""
        assert aifpl.evaluate_and_format('(list-concat)') == '()'

    @pytest.mark.parametrize("expression,expected", [
        # Basic reverse operations
        ('(list-reverse (list 1 2 3))', '(3 2 1)'),
        ('(list-reverse (list))', '()'),
        ('(list-reverse (list 1))', '(1)'),

        # Mixed type reverse
        ('(list-reverse (list 1 "hello" #t))', '(#t "hello" 1)'),

        # Nested list reverse (only reverses top level)
        ('(list-reverse (list (list 1 2) (list 3 4)))', '((3 4) (1 2))'),
    ])
    def test_reverse_operation(self, aifpl, expression, expected):
        """Test reverse operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_reverse_requires_list_argument(self, aifpl):
        """Test that reverse requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-reverse "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-reverse 42)')

    @pytest.mark.parametrize("expression,expected", [
        # First element access
        ('(list-first (list 1 2 3))', '1'),
        ('(list-first (list "hello" "world"))', '"hello"'),
        ('(list-first (list #t #f))', '#t'),
        ('(list-first (list (list 1 2) 3))', '(1 2)'),  # First element is a list
    ])
    def test_first_operation(self, aifpl, expression, expected):
        """Test first operation for accessing first element."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_first_empty_list_error(self, aifpl):
        """Test that first raises error on empty list."""
        with pytest.raises(AIFPLEvalError, match="Cannot get first element of empty list"):
            aifpl.evaluate('(list-first (list))')

    def test_first_requires_list_argument(self, aifpl):
        """Test that first requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-first "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-first 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Rest element access
        ('(list-rest (list 1 2 3))', '(2 3)'),
        ('(list-rest (list "hello" "world" "test"))', '("world" "test")'),
        ('(list-rest (list 1))', '()'),  # Rest of single-element list is empty
        ('(list-rest (list (list 1 2) 3 4))', '(3 4)'),  # Rest after nested list
    ])
    def test_rest_operation(self, aifpl, expression, expected):
        """Test rest operation for accessing all but first element."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_rest_empty_list_error(self, aifpl):
        """Test that rest raises error on empty list."""
        with pytest.raises(AIFPLEvalError, match="Cannot get rest of empty list"):
            aifpl.evaluate('(list-rest (list))')

    def test_rest_requires_list_argument(self, aifpl):
        """Test that rest requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-rest "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-rest 42)')

    @pytest.mark.parametrize("expression,expected", [
        # List reference by index
        ('(list-ref (list "a" "b" "c") 0)', '"a"'),
        ('(list-ref (list "a" "b" "c") 1)', '"b"'),
        ('(list-ref (list "a" "b" "c") 2)', '"c"'),

        # Mixed type list reference
        ('(list-ref (list 1 "hello" #t) 0)', '1'),
        ('(list-ref (list 1 "hello" #t) 1)', '"hello"'),
        ('(list-ref (list 1 "hello" #t) 2)', '#t'),

        # Nested list reference
        ('(list-ref (list (list 1 2) (list 3 4)) 0)', '(1 2)'),
        ('(list-ref (list (list 1 2) (list 3 4)) 1)', '(3 4)'),
    ])
    def test_list_ref_operation(self, aifpl, expression, expected):
        """Test list-ref operation for accessing elements by index."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_list_ref_index_errors(self, aifpl):
        """Test list-ref with invalid indices."""
        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate('(list-ref (list 1 2 3) 3)')  # Index too high

        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate('(list-ref (list 1 2 3) -1)')  # Negative index

    def test_list_ref_requires_list_argument(self, aifpl):
        """Test that list-ref requires a list as first argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref "hello" 0)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref 42 0)')

    def test_list_ref_requires_integer_index(self, aifpl):
        """Test that list-ref requires integer index."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref (list 1 2 3) "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref (list 1 2 3) 1.5)')

    @pytest.mark.parametrize("expression,expected", [
        # Length of various lists
        ('(list-length (list))', '0'),
        ('(list-length (list 1))', '1'),
        ('(list-length (list 1 2 3))', '3'),
        ('(list-length (list 1 "hello" #t 3.14))', '4'),

        # Length of nested lists (only counts top-level elements)
        ('(list-length (list (list 1 2) (list 3 4 5)))', '2'),
    ])
    def test_length_operation(self, aifpl, expression, expected):
        """Test length operation for getting list size."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_length_requires_list_argument(self, aifpl):
        """Test that length requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-length "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-length 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Null predicate
        ('(list-null? (list))', '#t'),
        ('(list-null? (list 1))', '#f'),
        ('(list-null? (list 1 2 3))', '#f'),
    ])
    def test_null_predicate(self, aifpl, expression, expected):
        """Test null? predicate for checking empty lists."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_null_requires_list_argument(self, aifpl):
        """Test that null? requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-null? "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-null? 42)')

    @pytest.mark.parametrize("expression,expected", [
        # List predicate
        ('(list? (list))', '#t'),
        ('(list? (list 1 2 3))', '#t'),
        ('(list? "hello")', '#f'),
        ('(list? 42)', '#f'),
        ('(list? #t)', '#f'),
        ('(list? (integer->complex 1 2))', '#f'),
    ])
    def test_list_predicate(self, aifpl, expression, expected):
        """Test list? predicate for checking if value is a list."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Member predicate
        ('(list-member? (list 1 2 3) 2)', '#t'),
        ('(list-member? (list 1 2 3) 5)', '#f'),
        ('(list-member? (list "hello" "world") "hello")', '#t'),
        ('(list-member? (list "hello" "world") "test")', '#f'),
        ('(list-member? (list #t #f) #t)', '#t'),
        ('(list-member? (list #t) #f)', '#f'),

        # Member with mixed types
        ('(list-member? (list 1 "hello" #t) 1)', '#t'),
        ('(list-member? (list 1 "hello" #t) "hello")', '#t'),
        ('(list-member? (list 1 "hello" #t) #t)', '#t'),
        ('(list-member? (list 1 "hello" #t) 42)', '#f'),

        # Member with nested lists
        ('(list-member? (list (list 1 2) (list 3 4)) (list 1 2))', '#t'),
        ('(list-member? (list (list 1 2) (list 3 4)) (list 5 6))', '#f'),
    ])
    def test_member_predicate(self, aifpl, expression, expected):
        """Test member? predicate for checking list membership."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_member_requires_list_argument(self, aifpl):
        """Test that member? requires a list as second argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-member? "hello" 1)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-member? 42 1)')

    @pytest.mark.parametrize("expression,expected", [
        # Take operation
        ('(list-slice (list 1 2 3 4 5) 0 0)', '()'),
        ('(list-slice (list 1 2 3 4 5) 0 1)', '(1)'),
        ('(list-slice (list 1 2 3 4 5) 0 3)', '(1 2 3)'),
        ('(list-slice (list 1 2 3 4 5) 0 5)', '(1 2 3 4 5)'),

        # Take with mixed types
        ('(list-slice (list 1 "hello" #t 3.14) 0 2)', '(1 "hello")'),

        # Take from empty list
        ('(list-slice (list) 0 0)', '()'),
    ])
    def test_take_operation(self, aifpl, expression, expected):
        """Test take operation for getting first n elements."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_take_negative_count_error(self, aifpl):
        """Test that take rejects negative counts."""
        with pytest.raises(AIFPLEvalError, match="cannot be negative"):
            aifpl.evaluate('(list-slice (list 1 2 3) 0 -1)')

    def test_take_out_of_range_error(self, aifpl):
        """Test that list-slice rejects out-of-range indices."""
        with pytest.raises(AIFPLEvalError, match="out of range"):
            aifpl.evaluate('(list-slice (list 1 2 3) 0 10)')

    def test_take_requires_list_argument(self, aifpl):
        """Test that take requires a list as second argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice "hello" 0 2)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice 42 0 2)')

    def test_take_requires_integer_count(self, aifpl):
        """Test that take requires integer count."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice (list 1 2 3) 0 "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice (list 1 2 3) 0 2.5)')

    @pytest.mark.parametrize("expression,expected", [
        # Drop operation
        ('(list-slice (list 1 2 3 4 5) 0)', '(1 2 3 4 5)'),
        ('(list-slice (list 1 2 3 4 5) 1)', '(2 3 4 5)'),
        ('(list-slice (list 1 2 3 4 5) 3)', '(4 5)'),
        ('(list-slice (list 1 2 3 4 5) 5)', '()'),

        # Drop with mixed types
        ('(list-slice (list 1 "hello" #t 3.14) 2)', '(#t 3.14)'),

        # Drop from empty list
        ('(list-slice (list) 0)', '()'),
    ])
    def test_drop_operation(self, aifpl, expression, expected):
        """Test drop operation for removing first n elements."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_drop_negative_count_error(self, aifpl):
        """Test that drop rejects negative counts."""
        with pytest.raises(AIFPLEvalError, match="cannot be negative"):
            aifpl.evaluate('(list-slice (list 1 2 3) -1)')

    def test_drop_out_of_range_error(self, aifpl):
        """Test that list-slice rejects out-of-range start index."""
        with pytest.raises(AIFPLEvalError, match="out of range"):
            aifpl.evaluate('(list-slice (list 1 2 3) 10)')

    def test_drop_requires_list_argument(self, aifpl):
        """Test that drop requires a list as second argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice "hello" 2)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice 42 2)')

    def test_drop_requires_integer_count(self, aifpl):
        """Test that drop requires integer count."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice (list 1 2 3) "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice (list 1 2 3) 2.5)')

    @pytest.mark.parametrize("expression,expected", [
        # List equality
        ('(list=? (list) (list))', '#t'),
        ('(list=? (list 1) (list 1))', '#t'),
        ('(list=? (list 1 2 3) (list 1 2 3))', '#t'),
        ('(list=? (list 1 2) (list 1 2 3))', '#f'),  # Different lengths
        ('(list=? (list 1 2 3) (list 1 3 2))', '#f'),  # Different order

        # Mixed type list equality
        ('(list=? (list 1 "hello" #t) (list 1 "hello" #t))', '#t'),
        ('(list=? (list 1 "hello") (list 1 "world"))', '#f'),

        # Nested list equality
        ('(list=? (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))', '#t'),
        ('(list=? (list (list 1 2)) (list (list 1 3)))', '#f'),

        # Multiple list equality
        ('(list=? (list 1 2) (list 1 2) (list 1 2))', '#t'),
        ('(list=? (list 1 2) (list 1 2) (list 1 3))', '#f'),
    ])
    def test_list_equality(self, aifpl, expression, expected):
        """Test list equality using list=? operator."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_list_comparison_operators_not_supported(self, aifpl):
        """Test that lists don't support comparison operators other than =."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(< (list 1 2) (list 3 4))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(> (list 1 2) (list 3 4))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(<= (list 1 2) (list 3 4))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(>= (list 1 2) (list 3 4))')

    def test_list_arithmetic_not_supported(self, aifpl):
        """Test that lists don't support arithmetic operations."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(integer+ (list 1 2) (list 3 4))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(integer* (list 1 2) 3)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(integer- (list 5 6) (list 1 2))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(integer/ (list 10) (list 2))')

    def test_list_function_arity_validation(self, aifpl):
        """Test that list functions validate argument counts."""
        # cons requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-prepend 1)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-prepend 1 (list 2) (list 3))')

        # first requires exactly 1 argument
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(first)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-first (list 1) (list 2))')

        # list-ref requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref (list 1 2 3))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref (list 1) 0 1)')

        # slice requires 2 or 3 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice (list 1 2 3))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-slice (list 1 2 3) 1 2 3)')

    def test_complex_list_operations(self, aifpl, helpers):
        """Test complex combinations of list operations."""
        # Reverse of list-concat
        helpers.assert_evaluates_to(
            aifpl,
            '(list-reverse (list-concat (list 1 2) (list 3 4)))',
            '(4 3 2 1)'
        )

        # First of rest
        helpers.assert_evaluates_to(
            aifpl,
            '(list-first (list-rest (list 1 2 3 4)))',
            '2'
        )

        # Length of reverse (should be same)
        helpers.assert_evaluates_to(
            aifpl,
            '(list-length (list-reverse (list 1 2 3 4 5)))',
            '5'
        )

        # Nested list operations
        helpers.assert_evaluates_to(
            aifpl,
            '(list-first (list-first (list (list 1 2) (list 3 4))))',
            '1'
        )

        # Take and drop complementarity
        original_list = '(list 1 2 3 4 5)'
        take_expr = f'(list-slice {original_list} 0 3)'
        drop_expr = f'(list-slice {original_list} 3)'

        take_result = aifpl.evaluate_and_format(take_expr)
        drop_result = aifpl.evaluate_and_format(drop_expr)

        assert take_result == '(1 2 3)'
        assert drop_result == '(4 5)'

        # list-concat take and drop should reconstruct original
        reconstruct_expr = f'(list-concat (list-slice {original_list} 0 3) (list-slice {original_list} 3))'
        helpers.assert_evaluates_to(aifpl, reconstruct_expr, '(1 2 3 4 5)')

    def test_list_with_all_data_types(self, aifpl, helpers):
        """Test lists containing all supported data types."""
        complex_list = '''
        (list 
          42 
          3.14 
          (integer->complex 1 2) 
          "hello" 
          #t 
          #f 
          (list 1 2 3)
        )
        '''

        result = aifpl.evaluate_and_format(complex_list)
        expected = '(42 3.14 1+2j "hello" #t #f (1 2 3))'
        assert result == expected

        # Test operations on this complex list
        helpers.assert_evaluates_to(
            aifpl,
            f'(list-length {complex_list})',
            '7'
        )

        helpers.assert_evaluates_to(
            aifpl,
            f'(list-first {complex_list})',
            '42'
        )

        helpers.assert_evaluates_to(
            aifpl,
            f'(list-ref {complex_list} 3)',
            '"hello"'
        )

    def test_deeply_nested_lists(self, aifpl, helpers):
        """Test operations on deeply nested lists."""
        # Create a deeply nested list structure
        nested_expr = '(list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8)))'

        helpers.assert_evaluates_to(
            aifpl,
            nested_expr,
            '(((1 2) (3 4)) ((5 6) (7 8)))'
        )

        # Access nested elements
        helpers.assert_evaluates_to(
            aifpl,
            f'(list-first (list-first {nested_expr}))',
            '(1 2)'
        )

        helpers.assert_evaluates_to(
            aifpl,
            f'(list-first (list-first (list-first {nested_expr})))',
            '1'
        )

    def test_list_identity_operations(self, aifpl, helpers):
        """Test operations that should preserve list identity."""
        test_list = '(list 1 2 3 4 5)'

        # Reverse twice should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(list-reverse (list-reverse {test_list}))',
            '(1 2 3 4 5)'
        )

        # Take all elements should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(list-slice {test_list} 0 (list-length {test_list}))',
            '(1 2 3 4 5)'
        )

        # Drop zero elements should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(list-slice {test_list} 0)',
            '(1 2 3 4 5)'
        )

        # list-concat empty list should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(list-concat {test_list} (list))',
            '(1 2 3 4 5)'
        )

        helpers.assert_evaluates_to(
            aifpl,
            f'(list-concat (list) {test_list})',
            '(1 2 3 4 5)'
        )
