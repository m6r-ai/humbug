"""Tests for conditional operations and boolean logic."""

import pytest

from aifpl import AIFPL, AIFPLEvalError


class TestConditionals:
    """Test conditional operations and boolean logic."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic if expressions
        ('(if #t "yes" "no")', '"yes"'),
        ('(if #f "yes" "no")', '"no"'),

        # If with numeric conditions
        ('(if (> 5 3) "greater" "less")', '"greater"'),
        ('(if (< 5 3) "greater" "less")', '"less"'),
        ('(if (= 5 5) "equal" "not equal")', '"equal"'),

        # If with different result types
        ('(if #t 42 0)', '42'),
        ('(if #f 42 0)', '0'),
        ('(if #t (list 1 2) (list 3 4))', '(1 2)'),
        ('(if #f (list 1 2) (list 3 4))', '(3 4)'),

        # If with complex expressions in branches
        ('(if #t (+ 1 2) (* 3 4))', '3'),
        ('(if #f (+ 1 2) (* 3 4))', '12'),
    ])
    def test_basic_if_expressions(self, aifpl, expression, expected):
        """Test basic if expressions with various conditions and result types."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_if_lazy_evaluation_prevents_errors(self, aifpl):
        """Test that if expressions use lazy evaluation to prevent errors."""
        # Division by zero in unused branch should not cause error
        result = aifpl.evaluate_and_format('(if #t 42 (/ 1 0))')
        assert result == '42'

        result = aifpl.evaluate_and_format('(if #f (/ 1 0) 24)')
        assert result == '24'

        # Undefined symbol in unused branch should not cause error
        result = aifpl.evaluate_and_format('(if #t "safe" undefined-symbol)')
        assert result == '"safe"'

        result = aifpl.evaluate_and_format('(if #f undefined-symbol "safe")')
        assert result == '"safe"'

    def test_if_lazy_evaluation_with_complex_conditions(self, aifpl):
        """Test lazy evaluation with more complex scenarios."""
        # Safe list operations
        result = aifpl.evaluate_and_format('(if (null? (list)) "empty" (first (list)))')
        assert result == '"empty"'

        # The false branch would cause an error if evaluated
        result = aifpl.evaluate_and_format('(if (> 10 5) "big" (first (list)))')
        assert result == '"big"'

    def test_if_requires_boolean_condition(self, aifpl):
        """Test that if expressions require boolean conditions."""
        with pytest.raises(AIFPLEvalError, match=r"condition must be boolean"):
            aifpl.evaluate('(if 1 "yes" "no")')

        with pytest.raises(AIFPLEvalError, match=r"condition must be boolean"):
            aifpl.evaluate('(if "hello" "yes" "no")')

        with pytest.raises(AIFPLEvalError, match=r"condition must be boolean"):
            aifpl.evaluate('(if (list 1 2) "yes" "no")')

        with pytest.raises(AIFPLEvalError, match=r"condition must be boolean"):
            aifpl.evaluate('(if 0 "yes" "no")')  # 0 is not false in AIFPL

    def test_if_requires_exactly_three_arguments(self, aifpl):
        """Test that if expressions require exactly 3 arguments."""
        with pytest.raises(AIFPLEvalError, match=r"wrong number of arguments[\s\S]*Exactly 3 arguments"):
            aifpl.evaluate('(if #t "yes")')  # Missing else branch

        with pytest.raises(AIFPLEvalError, match=r"wrong number of arguments[\s\S]*Exactly 3 arguments"):
            aifpl.evaluate('(if #t)')  # Missing both branches

        with pytest.raises(AIFPLEvalError, match=r"wrong number of arguments[\s\S]*Exactly 3 arguments"):
            aifpl.evaluate('(if #t "yes" "no" "extra")')  # Too many arguments

    @pytest.mark.parametrize("expression,expected", [
        # Nested if expressions
        ('(if #t (if #t "inner-true" "inner-false") "outer-false")', '"inner-true"'),
        ('(if #t (if #f "inner-true" "inner-false") "outer-false")', '"inner-false"'),
        ('(if #f (if #t "inner-true" "inner-false") "outer-false")', '"outer-false"'),

        # Complex nested conditions
        ('(if (> 10 5) (if (< 3 7) "both-true" "first-true-second-false") "first-false")', '"both-true"'),
        ('(if (> 10 5) (if (> 3 7) "both-true" "first-true-second-false") "first-false")', '"first-true-second-false"'),
        ('(if (< 10 5) (if (< 3 7) "both-true" "first-false-second-true") "first-false")', '"first-false"'),
    ])
    def test_nested_if_expressions(self, aifpl, expression, expected):
        """Test nested if expressions."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_nested_if_lazy_evaluation(self, aifpl):
        """Test that nested if expressions maintain lazy evaluation."""
        # Inner if should not be evaluated if outer condition is false
        result = aifpl.evaluate_and_format('(if #f (if undefined-condition "inner" "inner") "outer")')
        assert result == '"outer"'

        # Only the chosen inner branch should be evaluated
        result = aifpl.evaluate_and_format('(if #t (if #t "chosen" (/ 1 0)) "not-chosen")')
        assert result == '"chosen"'

    @pytest.mark.parametrize("expression,expected", [
        # Basic boolean AND
        ('(and)', '#t'),  # Identity case (empty and is true)
        ('(and #t)', '#t'),
        ('(and #f)', '#f'),
        ('(and #t #t)', '#t'),
        ('(and #t #f)', '#f'),
        ('(and #f #t)', '#f'),
        ('(and #f #f)', '#f'),

        # Multiple arguments
        ('(and #t #t #t)', '#t'),
        ('(and #t #t #f)', '#f'),
        ('(and #f #t #t)', '#f'),  # Short-circuit evaluation
    ])
    def test_boolean_and_operation(self, aifpl, expression, expected):
        """Test boolean AND operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic boolean OR
        ('(or)', '#f'),  # Identity case (empty or is false)
        ('(or #t)', '#t'),
        ('(or #f)', '#f'),
        ('(or #t #t)', '#t'),
        ('(or #t #f)', '#t'),
        ('(or #f #t)', '#t'),
        ('(or #f #f)', '#f'),

        # Multiple arguments
        ('(or #f #f #f)', '#f'),
        ('(or #f #f #t)', '#t'),
        ('(or #t #f #f)', '#t'),  # Short-circuit evaluation
    ])
    def test_boolean_or_operation(self, aifpl, expression, expected):
        """Test boolean OR operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Boolean NOT
        ('(not #t)', '#f'),
        ('(not #f)', '#t'),
    ])
    def test_boolean_not_operation(self, aifpl, expression, expected):
        """Test boolean NOT operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_boolean_operations_require_boolean_arguments(self, aifpl):
        """Test that boolean operations require boolean arguments."""
        # AND with non-boolean arguments
        with pytest.raises(AIFPLEvalError, match=r"must be boolean"):
            aifpl.evaluate('(and #t 1)')

        with pytest.raises(AIFPLEvalError, match=r"must be boolean"):
            aifpl.evaluate('(and "hello" #t)')

        with pytest.raises(AIFPLEvalError, match=r"must be boolean"):
            aifpl.evaluate('(and #t (list 1 2))')

        # OR with non-boolean arguments
        with pytest.raises(AIFPLEvalError, match=r"must be boolean"):
            aifpl.evaluate('(or #f 1)')

        with pytest.raises(AIFPLEvalError, match=r"must be boolean"):
            aifpl.evaluate('(or "hello" #f)')

        # NOT with non-boolean arguments
        with pytest.raises(AIFPLEvalError, match=r"requires boolean arguments"):
            aifpl.evaluate('(not 1)')

        with pytest.raises(AIFPLEvalError, match=r"requires boolean arguments"):
            aifpl.evaluate('(not "hello")')

    def test_not_requires_exactly_one_argument(self, aifpl):
        """Test that NOT requires exactly one argument."""
        with pytest.raises(AIFPLEvalError, match="requires exactly 1 argument"):
            aifpl.evaluate('(not)')

        with pytest.raises(AIFPLEvalError, match="requires exactly 1 argument"):
            aifpl.evaluate('(not #t #f)')

    @pytest.mark.parametrize("expression,expected", [
        # Numeric equality
        ('(= 1 1)', '#t'),
        ('(= 1 2)', '#f'),
        ('(= 1 1 1)', '#t'),
        ('(= 1 1 2)', '#f'),
        ('(= 5 5 5 5)', '#t'),

        # Mixed numeric types
        ('(= 1 1.0)', '#t'),
        ('(= 2 2.0)', '#t'),
        ('(= 3.14 3.14)', '#t'),

        # String equality
        ('(= "hello" "hello")', '#t'),
        ('(= "hello" "world")', '#f'),
        ('(= "test" "test" "test")', '#t'),

        # Boolean equality
        ('(= #t #t)', '#t'),
        ('(= #f #f)', '#t'),
        ('(= #t #f)', '#f'),

        # List equality
        ('(= (list 1 2) (list 1 2))', '#t'),
        ('(= (list 1 2) (list 2 1))', '#f'),
        ('(= (list) (list))', '#t'),

        # Complex number equality
        ('(= (complex 1 2) (complex 1 2))', '#t'),
        ('(= (complex 1 2) (complex 2 1))', '#f'),
    ])
    def test_equality_comparison(self, aifpl, expression, expected):
        """Test equality comparison operator."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_equality_requires_at_least_two_arguments(self, aifpl):
        """Test that equality requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(=)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(= 1)')

    @pytest.mark.parametrize("expression,expected", [
        # Less than
        ('(< 1 2)', '#t'),
        ('(< 2 1)', '#f'),
        ('(< 1 1)', '#f'),
        ('(< 1 2 3)', '#t'),  # Chain: 1 < 2 < 3
        ('(< 1 3 2)', '#f'),  # Chain fails: 3 < 2 is false

        # Less than or equal
        ('(<= 1 2)', '#t'),
        ('(<= 2 1)', '#f'),
        ('(<= 1 1)', '#t'),  # Equal case
        ('(<= 1 1 2)', '#t'),
        ('(<= 1 2 1)', '#f'),

        # Greater than
        ('(> 2 1)', '#t'),
        ('(> 1 2)', '#f'),
        ('(> 1 1)', '#f'),
        ('(> 3 2 1)', '#t'),  # Chain: 3 > 2 > 1
        ('(> 3 1 2)', '#f'),  # Chain fails: 1 > 2 is false

        # Greater than or equal
        ('(>= 2 1)', '#t'),
        ('(>= 1 2)', '#f'),
        ('(>= 1 1)', '#t'),  # Equal case
        ('(>= 2 1 1)', '#t'),
        ('(>= 1 1 2)', '#f'),
    ])
    def test_numeric_comparison_operations(self, aifpl, expression, expected):
        """Test numeric comparison operations."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_comparison_operations_with_mixed_numeric_types(self, aifpl, helpers):
        """Test comparison operations with mixed numeric types."""
        helpers.assert_evaluates_to(aifpl, '(< 1 2.5)', '#t')
        helpers.assert_evaluates_to(aifpl, '(> 3.14 3)', '#t')
        helpers.assert_evaluates_to(aifpl, '(<= 2.0 2)', '#t')
        helpers.assert_evaluates_to(aifpl, '(>= 5 4.9)', '#t')

    def test_comparison_operations_require_numeric_arguments(self, aifpl):
        """Test that comparison operations require numeric arguments."""
        # Less than with non-numeric arguments
        with pytest.raises(AIFPLEvalError, match="requires numeric arguments"):
            aifpl.evaluate('(< "hello" "world")')

        with pytest.raises(AIFPLEvalError, match="requires numeric arguments"):
            aifpl.evaluate('(< 1 #t)')

        with pytest.raises(AIFPLEvalError, match="requires numeric arguments"):
            aifpl.evaluate('(< (list 1) (list 2))')

        # Other comparison operators
        with pytest.raises(AIFPLEvalError, match="requires numeric arguments"):
            aifpl.evaluate('(> "a" "b")')

        with pytest.raises(AIFPLEvalError, match="requires numeric arguments"):
            aifpl.evaluate('(<= #t #f)')

        with pytest.raises(AIFPLEvalError, match="requires numeric arguments"):
            aifpl.evaluate('(>= (list 1) (list 2))')

    def test_comparison_operations_require_at_least_two_arguments(self, aifpl):
        """Test that comparison operations require at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(=)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(= 1)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(!=)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(!= 1)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(<)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(< 1)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(>)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(> 5)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(<=)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(<= 1)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(>=)')

        with pytest.raises(AIFPLEvalError, match="requires at least 2 arguments"):
            aifpl.evaluate('(>= 5)')

    def test_complex_boolean_expressions(self, aifpl, helpers):
        """Test complex combinations of boolean operations."""
        # De Morgan's laws
        helpers.assert_evaluates_to(
            aifpl,
            '(= (not (and #t #f)) (or (not #t) (not #f)))',
            '#t'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(= (not (or #t #f)) (and (not #t) (not #f)))',
            '#t'
        )

        # Complex nested boolean logic
        helpers.assert_evaluates_to(
            aifpl,
            '(and (or #t #f) (not #f))',
            '#t'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(or (and #t #f) (and #t #t))',
            '#t'
        )

    def test_conditional_with_comparison_operations(self, aifpl, helpers):
        """Test conditionals using comparison operations."""
        # Safe division based on condition
        helpers.assert_evaluates_to(
            aifpl,
            '(if (> 10 0) (/ 20 10) "undefined")',
            '2.0'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (= 0 0) "zero" "not zero")',
            '"zero"'
        )

        # Multiple conditions
        helpers.assert_evaluates_to(
            aifpl,
            '(if (and (> 5 3) (< 2 4)) "both true" "at least one false")',
            '"both true"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (or (> 5 10) (< 2 4)) "at least one true" "both false")',
            '"at least one true"'
        )

    def test_conditional_with_list_predicates(self, aifpl, helpers):
        """Test conditionals using list predicates."""
        # Safe list operations
        helpers.assert_evaluates_to(
            aifpl,
            '(if (null? (list)) "empty" "not empty")',
            '"empty"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (member? 2 (list 1 2 3)) "found" "not found")',
            '"found"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (list? (list 1 2)) "is list" "not list")',
            '"is list"'
        )

    def test_conditional_with_string_predicates(self, aifpl, helpers):
        """Test conditionals using string predicates."""
        helpers.assert_evaluates_to(
            aifpl,
            '(if (string-contains? "hello world" "world") "found" "not found")',
            '"found"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (string-prefix? "hello" "he") "has prefix" "no prefix")',
            '"has prefix"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (string=? "test" "test") "equal" "not equal")',
            '"equal"'
        )

    def test_conditional_result_type_consistency(self, aifpl, helpers):
        """Test that conditionals can return any type consistently."""
        # Return different numbers
        helpers.assert_evaluates_to(aifpl, '(if #t 42 3.14)', '42')
        helpers.assert_evaluates_to(aifpl, '(if #f 42 3.14)', '3.14')

        # Return different strings
        helpers.assert_evaluates_to(aifpl, '(if #t "hello" "world")', '"hello"')
        helpers.assert_evaluates_to(aifpl, '(if #f "hello" "world")', '"world"')

        # Return different booleans
        helpers.assert_evaluates_to(aifpl, '(if #t #t #f)', '#t')
        helpers.assert_evaluates_to(aifpl, '(if #f #t #f)', '#f')

        # Return different lists
        helpers.assert_evaluates_to(aifpl, '(if #t (list 1 2) (list 3 4))', '(1 2)')
        helpers.assert_evaluates_to(aifpl, '(if #f (list 1 2) (list 3 4))', '(3 4)')

        # Return different types (mixed)
        helpers.assert_evaluates_to(aifpl, '(if #t 42 "hello")', '42')
        helpers.assert_evaluates_to(aifpl, '(if #f 42 "hello")', '"hello"')

    def test_deeply_nested_conditionals(self, aifpl, helpers):
        """Test deeply nested conditional expressions."""
        # Nested ternary-like logic
        nested_expr = '''
        (if (> 10 5)
            (if (< 3 7)
                (if (= 2 2) "all true" "third false")
                "second false")
            "first false")
        '''
        helpers.assert_evaluates_to(aifpl, nested_expr, '"all true"')

        # Complex decision tree
        decision_tree = '''
        (if (> 15 10)
            (if (< 5 8)
                (if (= 3 3) 
                    (if #t "deeply nested true" "impossible")
                    "equality false")
                "comparison false")
            "initial false")
        '''
        helpers.assert_evaluates_to(aifpl, decision_tree, '"deeply nested true"')

    def test_conditional_with_error_prone_expressions(self, aifpl, helpers):
        """Test conditionals that prevent errors through lazy evaluation."""
        # Division by zero prevention
        helpers.assert_evaluates_to(
            aifpl,
            '(if (= 5 0) (/ 10 5) "divisor is zero")',
            '"divisor is zero"'
        )

        # Empty list access prevention
        helpers.assert_evaluates_to(
            aifpl,
            '(if (null? (list)) "list is empty" (first (list)))',
            '"list is empty"'
        )

        # Invalid string index prevention
        helpers.assert_evaluates_to(
            aifpl,
            '(if (< (string-length "hi") 5) "short string" (string-ref "hi" 10))',
            '"short string"'
        )

    def test_boolean_short_circuit_evaluation(self, aifpl):
        """Test that boolean operations use short-circuit evaluation."""
        # AND short-circuit: if first is false, don't evaluate second
        result = aifpl.evaluate_and_format('(and #f (/ 1 0))')  # Should not cause division by zero
        assert result == '#f'

        # OR short-circuit: if first is true, don't evaluate second
        result = aifpl.evaluate_and_format('(or #t (/ 1 0))')  # Should not cause division by zero
        assert result == '#t'

    def test_comparison_chain_evaluation(self, aifpl, helpers):
        """Test that comparison operations evaluate as chains."""
        # All comparisons in chain must be true
        helpers.assert_evaluates_to(aifpl, '(< 1 2 3 4 5)', '#t')
        helpers.assert_evaluates_to(aifpl, '(< 1 2 5 4)', '#f')  # 5 < 4 is false

        helpers.assert_evaluates_to(aifpl, '(> 5 4 3 2 1)', '#t')
        helpers.assert_evaluates_to(aifpl, '(> 5 4 1 3)', '#f')  # 1 > 3 is false

        helpers.assert_evaluates_to(aifpl, '(<= 1 1 2 2 3)', '#t')
        helpers.assert_evaluates_to(aifpl, '(>= 3 2 2 1 1)', '#t')

    def test_mixed_type_equality_edge_cases(self, aifpl, helpers):
        """Test equality comparison with mixed types."""
        # Different types should not be equal
        helpers.assert_evaluates_to(aifpl, '(= 1 "1")', '#f')
        helpers.assert_evaluates_to(aifpl, '(= #t 1)', '#f')
        helpers.assert_evaluates_to(aifpl, '(= #f 0)', '#f')
        helpers.assert_evaluates_to(aifpl, '(= (list 1) 1)', '#f')

        # Complex numbers and reals
        helpers.assert_evaluates_to(aifpl, '(= 5 (complex 5 0))', '#t')  # Should be equal
        helpers.assert_evaluates_to(aifpl, '(= 5 (complex 5 1))', '#f')  # Should not be equal
