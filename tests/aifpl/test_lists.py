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
        ('(list (complex 1 2) j)', '((1+2j) 1j)'),
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
        ('(cons 1 (list 2 3))', '(1 2 3)'),
        ('(cons "hello" (list "world"))', '("hello" "world")'),
        ('(cons #t (list #f))', '(#t #f)'),
        
        # Cons with empty list
        ('(cons 1 (list))', '(1)'),
        
        # Cons with mixed types
        ('(cons 1 (list "hello" #t))', '(1 "hello" #t)'),
        
        # Nested cons
        ('(cons (list 1 2) (list (list 3 4)))', '((1 2) (3 4))'),
    ])
    def test_cons_operation(self, aifpl, expression, expected):
        """Test cons operation for prepending elements."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_cons_requires_list_as_second_argument(self, aifpl):
        """Test that cons requires a list as the second argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(cons 1 2)')  # Second arg must be list
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(cons 1 "hello")')  # Second arg must be list

    @pytest.mark.parametrize("expression,expected", [
        # Basic append operations
        ('(append (list 1 2) (list 3 4))', '(1 2 3 4)'),
        ('(append (list) (list 1 2))', '(1 2)'),
        ('(append (list 1 2) (list))', '(1 2)'),
        ('(append (list) (list))', '()'),
        
        # Multiple list append
        ('(append (list 1) (list 2) (list 3))', '(1 2 3)'),
        ('(append (list 1 2) (list 3 4) (list 5 6))', '(1 2 3 4 5 6)'),
        
        # Mixed type append
        ('(append (list 1 "hello") (list #t 3.14))', '(1 "hello" #t 3.14)'),
        
        # Nested list append
        ('(append (list (list 1 2)) (list (list 3 4)))', '((1 2) (3 4))'),
    ])
    def test_append_operation(self, aifpl, expression, expected):
        """Test append operation for concatenating lists."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_append_requires_all_list_arguments(self, aifpl):
        """Test that append requires all arguments to be lists."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(append (list 1 2) 3)')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(append "hello" (list 1 2))')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(append (list 1) #t (list 2))')

    def test_append_minimum_arguments(self, aifpl):
        """Test that append requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(append)')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(append (list 1 2))')

    @pytest.mark.parametrize("expression,expected", [
        # Basic reverse operations
        ('(reverse (list 1 2 3))', '(3 2 1)'),
        ('(reverse (list))', '()'),
        ('(reverse (list 1))', '(1)'),
        
        # Mixed type reverse
        ('(reverse (list 1 "hello" #t))', '(#t "hello" 1)'),
        
        # Nested list reverse (only reverses top level)
        ('(reverse (list (list 1 2) (list 3 4)))', '((3 4) (1 2))'),
    ])
    def test_reverse_operation(self, aifpl, expression, expected):
        """Test reverse operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_reverse_requires_list_argument(self, aifpl):
        """Test that reverse requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(reverse "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(reverse 42)')

    @pytest.mark.parametrize("expression,expected", [
        # First element access
        ('(first (list 1 2 3))', '1'),
        ('(first (list "hello" "world"))', '"hello"'),
        ('(first (list #t #f))', '#t'),
        ('(first (list (list 1 2) 3))', '(1 2)'),  # First element is a list
    ])
    def test_first_operation(self, aifpl, expression, expected):
        """Test first operation for accessing first element."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_first_empty_list_error(self, aifpl):
        """Test that first raises error on empty list."""
        with pytest.raises(AIFPLEvalError, match="Cannot get first element of empty list"):
            aifpl.evaluate('(first (list))')

    def test_first_requires_list_argument(self, aifpl):
        """Test that first requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(first "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(first 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Rest element access
        ('(rest (list 1 2 3))', '(2 3)'),
        ('(rest (list "hello" "world" "test"))', '("world" "test")'),
        ('(rest (list 1))', '()'),  # Rest of single-element list is empty
        ('(rest (list (list 1 2) 3 4))', '(3 4)'),  # Rest after nested list
    ])
    def test_rest_operation(self, aifpl, expression, expected):
        """Test rest operation for accessing all but first element."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_rest_empty_list_error(self, aifpl):
        """Test that rest raises error on empty list."""
        with pytest.raises(AIFPLEvalError, match="Cannot get rest of empty list"):
            aifpl.evaluate('(rest (list))')

    def test_rest_requires_list_argument(self, aifpl):
        """Test that rest requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(rest "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(rest 42)')

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
        ('(length (list))', '0'),
        ('(length (list 1))', '1'),
        ('(length (list 1 2 3))', '3'),
        ('(length (list 1 "hello" #t 3.14))', '4'),
        
        # Length of nested lists (only counts top-level elements)
        ('(length (list (list 1 2) (list 3 4 5)))', '2'),
    ])
    def test_length_operation(self, aifpl, expression, expected):
        """Test length operation for getting list size."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_length_requires_list_argument(self, aifpl):
        """Test that length requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(length "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(length 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Null predicate
        ('(null? (list))', '#t'),
        ('(null? (list 1))', '#f'),
        ('(null? (list 1 2 3))', '#f'),
    ])
    def test_null_predicate(self, aifpl, expression, expected):
        """Test null? predicate for checking empty lists."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_null_requires_list_argument(self, aifpl):
        """Test that null? requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(null? "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(null? 42)')

    @pytest.mark.parametrize("expression,expected", [
        # List predicate
        ('(list? (list))', '#t'),
        ('(list? (list 1 2 3))', '#t'),
        ('(list? "hello")', '#f'),
        ('(list? 42)', '#f'),
        ('(list? #t)', '#f'),
        ('(list? (complex 1 2))', '#f'),
    ])
    def test_list_predicate(self, aifpl, expression, expected):
        """Test list? predicate for checking if value is a list."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Member predicate
        ('(member? 2 (list 1 2 3))', '#t'),
        ('(member? 5 (list 1 2 3))', '#f'),
        ('(member? "hello" (list "hello" "world"))', '#t'),
        ('(member? "test" (list "hello" "world"))', '#f'),
        ('(member? #t (list #t #f))', '#t'),
        ('(member? #f (list #t))', '#f'),
        
        # Member with mixed types
        ('(member? 1 (list 1 "hello" #t))', '#t'),
        ('(member? "hello" (list 1 "hello" #t))', '#t'),
        ('(member? #t (list 1 "hello" #t))', '#t'),
        ('(member? 42 (list 1 "hello" #t))', '#f'),
        
        # Member with nested lists
        ('(member? (list 1 2) (list (list 1 2) (list 3 4)))', '#t'),
        ('(member? (list 5 6) (list (list 1 2) (list 3 4)))', '#f'),
    ])
    def test_member_predicate(self, aifpl, expression, expected):
        """Test member? predicate for checking list membership."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_member_requires_list_argument(self, aifpl):
        """Test that member? requires a list as second argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(member? 1 "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(member? 1 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Take operation
        ('(take 0 (list 1 2 3 4 5))', '()'),
        ('(take 1 (list 1 2 3 4 5))', '(1)'),
        ('(take 3 (list 1 2 3 4 5))', '(1 2 3)'),
        ('(take 5 (list 1 2 3 4 5))', '(1 2 3 4 5)'),
        ('(take 10 (list 1 2 3))', '(1 2 3)'),  # Take more than available
        
        # Take with mixed types
        ('(take 2 (list 1 "hello" #t 3.14))', '(1 "hello")'),
        
        # Take from empty list
        ('(take 0 (list))', '()'),
        ('(take 5 (list))', '()'),  # Take from empty list returns empty
    ])
    def test_take_operation(self, aifpl, expression, expected):
        """Test take operation for getting first n elements."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_take_negative_count_error(self, aifpl):
        """Test that take rejects negative counts."""
        with pytest.raises(AIFPLEvalError, match="cannot be negative"):
            aifpl.evaluate('(take -1 (list 1 2 3))')

    def test_take_requires_list_argument(self, aifpl):
        """Test that take requires a list as second argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(take 2 "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(take 2 42)')

    def test_take_requires_integer_count(self, aifpl):
        """Test that take requires integer count."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(take "hello" (list 1 2 3))')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(take 2.5 (list 1 2 3))')

    @pytest.mark.parametrize("expression,expected", [
        # Drop operation
        ('(drop 0 (list 1 2 3 4 5))', '(1 2 3 4 5)'),
        ('(drop 1 (list 1 2 3 4 5))', '(2 3 4 5)'),
        ('(drop 3 (list 1 2 3 4 5))', '(4 5)'),
        ('(drop 5 (list 1 2 3 4 5))', '()'),
        ('(drop 10 (list 1 2 3))', '()'),  # Drop more than available
        
        # Drop with mixed types
        ('(drop 2 (list 1 "hello" #t 3.14))', '(#t 3.14)'),
        
        # Drop from empty list
        ('(drop 0 (list))', '()'),
        ('(drop 5 (list))', '()'),  # Drop from empty list returns empty
    ])
    def test_drop_operation(self, aifpl, expression, expected):
        """Test drop operation for removing first n elements."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_drop_negative_count_error(self, aifpl):
        """Test that drop rejects negative counts."""
        with pytest.raises(AIFPLEvalError, match="cannot be negative"):
            aifpl.evaluate('(drop -1 (list 1 2 3))')

    def test_drop_requires_list_argument(self, aifpl):
        """Test that drop requires a list as second argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(drop 2 "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(drop 2 42)')

    def test_drop_requires_integer_count(self, aifpl):
        """Test that drop requires integer count."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(drop "hello" (list 1 2 3))')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(drop 2.5 (list 1 2 3))')

    @pytest.mark.parametrize("expression,expected", [
        # List equality
        ('(= (list) (list))', '#t'),
        ('(= (list 1) (list 1))', '#t'),
        ('(= (list 1 2 3) (list 1 2 3))', '#t'),
        ('(= (list 1 2) (list 1 2 3))', '#f'),  # Different lengths
        ('(= (list 1 2 3) (list 1 3 2))', '#f'),  # Different order
        
        # Mixed type list equality
        ('(= (list 1 "hello" #t) (list 1 "hello" #t))', '#t'),
        ('(= (list 1 "hello") (list 1 "world"))', '#f'),
        
        # Nested list equality
        ('(= (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))', '#t'),
        ('(= (list (list 1 2)) (list (list 1 3)))', '#f'),
        
        # Multiple list equality
        ('(= (list 1 2) (list 1 2) (list 1 2))', '#t'),
        ('(= (list 1 2) (list 1 2) (list 1 3))', '#f'),
    ])
    def test_list_equality(self, aifpl, expression, expected):
        """Test list equality using = operator."""
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
            aifpl.evaluate('(+ (list 1 2) (list 3 4))')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(* (list 1 2) 3)')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(- (list 5 6) (list 1 2))')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(/ (list 10) (list 2))')

    def test_list_function_arity_validation(self, aifpl):
        """Test that list functions validate argument counts."""
        # cons requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(cons 1)')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(cons 1 (list 2) (list 3))')
        
        # first requires exactly 1 argument
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(first)')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(first (list 1) (list 2))')
        
        # list-ref requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref (list 1 2 3))')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list-ref (list 1) 0 1)')
        
        # take requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(take 3)')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(take 3 (list 1 2) (list 3 4))')

    def test_complex_list_operations(self, aifpl, helpers):
        """Test complex combinations of list operations."""
        # Reverse of append
        helpers.assert_evaluates_to(
            aifpl,
            '(reverse (append (list 1 2) (list 3 4)))',
            '(4 3 2 1)'
        )
        
        # First of rest
        helpers.assert_evaluates_to(
            aifpl,
            '(first (rest (list 1 2 3 4)))',
            '2'
        )
        
        # Length of reverse (should be same)
        helpers.assert_evaluates_to(
            aifpl,
            '(length (reverse (list 1 2 3 4 5)))',
            '5'
        )
        
        # Nested list operations
        helpers.assert_evaluates_to(
            aifpl,
            '(first (first (list (list 1 2) (list 3 4))))',
            '1'
        )
        
        # Take and drop complementarity
        original_list = '(list 1 2 3 4 5)'
        take_expr = f'(take 3 {original_list})'
        drop_expr = f'(drop 3 {original_list})'
        
        take_result = aifpl.evaluate_and_format(take_expr)
        drop_result = aifpl.evaluate_and_format(drop_expr)
        
        assert take_result == '(1 2 3)'
        assert drop_result == '(4 5)'
        
        # Append take and drop should reconstruct original
        reconstruct_expr = f'(append (take 3 {original_list}) (drop 3 {original_list}))'
        helpers.assert_evaluates_to(aifpl, reconstruct_expr, '(1 2 3 4 5)')

    def test_list_with_all_data_types(self, aifpl, helpers):
        """Test lists containing all supported data types."""
        complex_list = '''
        (list 
          42 
          3.14 
          (complex 1 2) 
          "hello" 
          #t 
          #f 
          (list 1 2 3)
        )
        '''
        
        result = aifpl.evaluate_and_format(complex_list)
        expected = '(42 3.14 (1+2j) "hello" #t #f (1 2 3))'
        assert result == expected
        
        # Test operations on this complex list
        helpers.assert_evaluates_to(
            aifpl,
            f'(length {complex_list})',
            '7'
        )
        
        helpers.assert_evaluates_to(
            aifpl,
            f'(first {complex_list})',
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
            f'(first (first {nested_expr}))',
            '(1 2)'
        )
        
        helpers.assert_evaluates_to(
            aifpl,
            f'(first (first (first {nested_expr})))',
            '1'
        )

    def test_list_identity_operations(self, aifpl, helpers):
        """Test operations that should preserve list identity."""
        test_list = '(list 1 2 3 4 5)'
        
        # Reverse twice should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(reverse (reverse {test_list}))',
            '(1 2 3 4 5)'
        )
        
        # Take all elements should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(take (length {test_list}) {test_list})',
            '(1 2 3 4 5)'
        )
        
        # Drop zero elements should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(drop 0 {test_list})',
            '(1 2 3 4 5)'
        )
        
        # Append empty list should give original
        helpers.assert_evaluates_to(
            aifpl,
            f'(append {test_list} (list))',
            '(1 2 3 4 5)'
        )
        
        helpers.assert_evaluates_to(
            aifpl,
            f'(append (list) {test_list})',
            '(1 2 3 4 5)'
        )