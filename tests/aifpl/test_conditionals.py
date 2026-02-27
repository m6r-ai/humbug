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
        ('(if (integer>? 5 3) "greater" "less")', '"greater"'),
        ('(if (integer<? 5 3) "greater" "less")', '"less"'),
        ('(if (integer=? 5 5) "equal" "not equal")', '"equal"'),

        # If with different result types
        ('(if #t 42 0)', '42'),
        ('(if #f 42 0)', '0'),
        ('(if #t (list 1 2) (list 3 4))', '(1 2)'),
        ('(if #f (list 1 2) (list 3 4))', '(3 4)'),

        # If with complex expressions in branches
        ('(if #t (integer+ 1 2) (integer* 3 4))', '3'),
        ('(if #f (integer+ 1 2) (integer* 3 4))', '12'),
    ])
    def test_basic_if_expressions(self, aifpl, expression, expected):
        """Test basic if expressions with various conditions and result types."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_if_lazy_evaluation_prevents_errors(self, aifpl):
        """Test that if expressions use lazy evaluation to prevent errors."""
        # Division by zero in unused branch should not cause error
        result = aifpl.evaluate_and_format('(if #t 42 (integer/ 1 0))')
        assert result == '42'

        result = aifpl.evaluate_and_format('(if #f (integer/ 1 0) 24)')
        assert result == '24'

        # Undefined symbol in unused branch should not cause error
        result = aifpl.evaluate_and_format('(if #t "safe" undefined-symbol)')
        assert result == '"safe"'

        result = aifpl.evaluate_and_format('(if #f undefined-symbol "safe")')
        assert result == '"safe"'

    def test_if_lazy_evaluation_with_complex_conditions(self, aifpl):
        """Test lazy evaluation with more complex scenarios."""
        # Safe list operations
        result = aifpl.evaluate_and_format('(if (list-null? (list)) "empty" (list-first (list)))')
        assert result == '"empty"'

        # The false branch would cause an error if evaluated
        result = aifpl.evaluate_and_format('(if (integer>? 10 5) "big" (list-first (list)))')
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
        ('(if (integer>? 10 5) (if (integer<? 3 7) "both-true" "first-true-second-false") "first-false")', '"both-true"'),
        ('(if (integer>? 10 5) (if (integer>? 3 7) "both-true" "first-true-second-false") "first-false")', '"first-true-second-false"'),
        ('(if (integer<? 10 5) (if (integer<? 3 7) "both-true" "first-false-second-true") "first-false")', '"first-false"'),
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
        ('(boolean-not #t)', '#f'),
        ('(boolean-not #f)', '#t'),
    ])
    def test_boolean_not_operation(self, aifpl, expression, expected):
        """Test boolean NOT operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_boolean_operations_require_boolean_arguments(self, aifpl):
        """Test that boolean operations require boolean condition arguments.

        Since and/or are lowered to if-chains, only the condition arguments
        (all but the last) are type-checked as booleans by JUMP_IF_FALSE/TRUE.
        Non-boolean conditions always error. The last argument (the value
        returned when all conditions are true/false) is not type-checked.
        """
        # Non-boolean used as condition always errors
        with pytest.raises(AIFPLEvalError, match=r"must be boolean"):
            aifpl.evaluate('(and "hello" #t)')

        with pytest.raises(AIFPLEvalError, match=r"must be boolean"):
            aifpl.evaluate('(or 1 #f)')

        # NOT with non-boolean arguments (boolean-not is still a typed opcode)
        with pytest.raises(AIFPLEvalError, match=r"requires boolean arguments"):
            aifpl.evaluate('(boolean-not 1)')

        with pytest.raises(AIFPLEvalError, match=r"requires boolean arguments"):
            aifpl.evaluate('(boolean-not "hello")')

    def test_not_requires_exactly_one_argument(self, aifpl):
        """Test that NOT requires exactly one argument."""
        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(boolean-not)')

        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(boolean-not #t #f)')

    @pytest.mark.parametrize("expression,expected", [
        # Numeric equality
        ('(integer=? 1 1)', '#t'),
        ('(integer=? 1 2)', '#f'),
        ('(integer=? 1 1 1)', '#t'),
        ('(integer=? 1 1 2)', '#f'),
        ('(integer=? 5 5 5 5)', '#t'),

        # String equality
        ('(string=? "hello" "hello")', '#t'),
        ('(string=? "hello" "world")', '#f'),
        ('(string=? "test" "test" "test")', '#t'),

        # Boolean equality
        ('(boolean=? #t #t)', '#t'),
        ('(boolean=? #f #f)', '#t'),
        ('(boolean=? #t #f)', '#f'),

        # List equality
        ('(list=? (list 1 2) (list 1 2))', '#t'),
        ('(list=? (list 1 2) (list 2 1))', '#f'),
        ('(list=? (list) (list))', '#t'),

        # Complex number equality
        ('(complex=? (integer->complex 1 2) (integer->complex 1 2))', '#t'),
        ('(complex=? (integer->complex 1 2) (integer->complex 2 1))', '#f'),
    ])
    def test_equality_comparison(self, aifpl, expression, expected):
        """Test equality comparison operator."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_equality_requires_at_least_two_arguments(self, aifpl):
        """Test that equality requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(integer=?)')

        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(integer=? 1)')

    @pytest.mark.parametrize("expression,expected", [
        # Less than
        ('(integer<? 1 2)', '#t'),
        ('(integer<? 2 1)', '#f'),
        ('(integer<? 1 1)', '#f'),
        ('(integer<? 1 2 3)', '#t'),  # Chain: 1 < 2 < 3
        ('(integer<? 1 3 2)', '#f'),  # Chain fails: 3 < 2 is false

        # Less than or equal
        ('(integer<=? 1 2)', '#t'),
        ('(integer<=? 2 1)', '#f'),
        ('(integer<=? 1 1)', '#t'),  # Equal case
        ('(integer<=? 1 1 2)', '#t'),
        ('(integer<=? 1 2 1)', '#f'),

        # Greater than
        ('(integer>? 2 1)', '#t'),
        ('(integer>? 1 2)', '#f'),
        ('(integer>? 1 1)', '#f'),
        ('(integer>? 3 2 1)', '#t'),  # Chain: 3 > 2 > 1
        ('(integer>? 3 1 2)', '#f'),  # Chain fails: 1 > 2 is false

        # Greater than or equal
        ('(integer>=? 2 1)', '#t'),
        ('(integer>=? 1 2)', '#f'),
        ('(integer>=? 1 1)', '#t'),  # Equal case
        ('(integer>=? 2 1 1)', '#t'),
        ('(integer>=? 1 1 2)', '#f'),
    ])
    def test_numeric_comparison_operations(self, aifpl, expression, expected):
        """Test integer ordered comparison operations."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_comparison_operations_require_numeric_arguments(self, aifpl):
        """Test that typed comparison operations require the correct argument types."""
        # integer ops reject floats, strings, and booleans
        with pytest.raises(AIFPLEvalError, match="integer<\\?.*requires integer arguments.*string"):
            aifpl.evaluate('(integer<? "hello" 1)')

        with pytest.raises(AIFPLEvalError, match="integer>\\?.*requires integer arguments.*boolean"):
            aifpl.evaluate('(integer>? #t 1)')

        with pytest.raises(AIFPLEvalError, match="integer<=\\?.*requires integer arguments.*float"):
            aifpl.evaluate('(integer<=? 1 2.0)')

        with pytest.raises(AIFPLEvalError, match="integer>=\\?.*requires integer arguments.*list"):
            aifpl.evaluate('(integer>=? (list 1) 2)')

        # float ops reject integers and strings
        with pytest.raises(AIFPLEvalError, match="float<\\?.*requires float arguments.*integer"):
            aifpl.evaluate('(float<? 1.0 2)')

        with pytest.raises(AIFPLEvalError, match="float>\\?.*requires float arguments.*string"):
            aifpl.evaluate('(float>? "a" 1.0)')

        # string ops reject integers and booleans
        with pytest.raises(AIFPLEvalError, match="string<=\\?.*requires string arguments.*integer"):
            aifpl.evaluate('(string<=? "a" 1)')

        with pytest.raises(AIFPLEvalError, match="string>=\\?.*requires string arguments.*boolean"):
            aifpl.evaluate('(string>=? #t "a")')

    def test_comparison_operations_require_at_least_two_arguments(self, aifpl):
        """Test that comparison operations require at least 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(integer=?)')

        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(integer=? 1)')

        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(integer!=?)')

        with pytest.raises(AIFPLEvalError, match="has wrong number of arguments"):
            aifpl.evaluate('(integer!=? 1)')

    def test_complex_boolean_expressions(self, aifpl, helpers):
        """Test complex combinations of boolean operations."""
        # De Morgan's laws
        helpers.assert_evaluates_to(
            aifpl,
            '(boolean=? (boolean-not (and #t #f)) (or (boolean-not #t) (boolean-not #f)))',
            '#t'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(boolean=? (boolean-not (or #t #f)) (and (boolean-not #t) (boolean-not #f)))',
            '#t'
        )

        # Complex nested boolean logic
        helpers.assert_evaluates_to(
            aifpl,
            '(and (or #t #f) (boolean-not #f))',
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
            '(if (integer>? 10 0) (float/ 20.0 10.0) "undefined")',
            '2.0'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (integer=? 0 0) "zero" "not zero")',
            '"zero"'
        )

        # Multiple conditions
        helpers.assert_evaluates_to(
            aifpl,
            '(if (and (integer>? 5 3) (integer<? 2 4)) "both true" "at least one false")',
            '"both true"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (or (integer>? 5 10) (integer<? 2 4)) "at least one true" "both false")',
            '"at least one true"'
        )

    def test_conditional_with_list_predicates(self, aifpl, helpers):
        """Test conditionals using list predicates."""
        # Safe list operations
        helpers.assert_evaluates_to(
            aifpl,
            '(if (list-null? (list)) "empty" "not empty")',
            '"empty"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(if (list-member? (list 1 2 3) 2) "found" "not found")',
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
            '(if (integer? (string-index "hello world" "world")) "found" "not found")',
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
        (if (integer>? 10 5)
            (if (integer<? 3 7)
                (if (integer=? 2 2) "all true" "third false")
                "second false")
            "first false")
        '''
        helpers.assert_evaluates_to(aifpl, nested_expr, '"all true"')

        # Complex decision tree
        decision_tree = '''
        (if (integer>? 15 10)
            (if (integer<? 5 8)
                (if (integer=? 3 3) 
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
            '(if (integer=? 5 0) (/ 10 5) "divisor is zero")',
            '"divisor is zero"'
        )

        # Empty list access prevention
        helpers.assert_evaluates_to(
            aifpl,
            '(if (list-null? (list)) "list is empty" (list-first (list)))',
            '"list is empty"'
        )

        # Invalid string index prevention
        helpers.assert_evaluates_to(
            aifpl,
            '(if (integer<? (string-length "hi") 5) "short string" (string-ref "hi" 10))',
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
        """Test that typed comparison operations evaluate as variadic chains."""
        # All comparisons in chain must be true
        helpers.assert_evaluates_to(aifpl, '(integer<? 1 2 3 4 5)', '#t')
        helpers.assert_evaluates_to(aifpl, '(integer<? 1 2 5 4)', '#f')  # 5 < 4 is false

        helpers.assert_evaluates_to(aifpl, '(integer>? 5 4 3 2 1)', '#t')
        helpers.assert_evaluates_to(aifpl, '(integer>? 5 4 1 3)', '#f')  # 1 > 3 is false

        helpers.assert_evaluates_to(aifpl, '(integer<=? 1 1 2 2 3)', '#t')
        helpers.assert_evaluates_to(aifpl, '(integer>=? 3 2 2 1 1)', '#t')
