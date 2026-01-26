"""Tests for pattern matching functionality in AIFPL.

This module provides comprehensive tests for the new pattern matching feature,
including literal patterns, variable binding, wildcard patterns, type patterns,
list structure patterns, nested patterns, and error cases.
"""

import pytest

from aifpl import AIFPLEvalError


class TestPatternMatching:
    """Test pattern matching functionality."""

    # ========== Basic Literal Pattern Matching ==========

    @pytest.mark.parametrize("expression,expected", [
        # Number literal patterns
        ('(match 42 (42 "found") (_ "not found"))', '"found"'),
        ('(match 42 (41 "wrong") (42 "correct"))', '"correct"'),
        ('(match 3.14 (3.14 "pi") (_ "not pi"))', '"pi"'),
        ('(match 0 (0 "zero") (_ "non-zero"))', '"zero"'),
        ('(match -5 (-5 "negative five") (_ "other"))', '"negative five"'),

        # String literal patterns
        ('(match "hello" ("hello" "greeting") (_ "other"))', '"greeting"'),
        ('(match "world" ("hello" "greeting") ("world" "planet"))', '"planet"'),
        ('(match "" ("" "empty") (_ "non-empty"))', '"empty"'),
        ('(match "test" ("TEST" "upper") ("test" "lower"))', '"lower"'),

        # Boolean literal patterns
        ('(match #t (#t "true") (#f "false"))', '"true"'),
        ('(match #f (#t "true") (#f "false"))', '"false"'),
        ('(match #t (#f "wrong") (#t "right"))', '"right"'),

        # Complex number literal patterns
        ('(match j (j "imaginary unit") (_ "other"))', '"imaginary unit"'),
    ])
    def test_literal_pattern_matching(self, aifpl, expression, expected):
        """Test basic literal pattern matching for all AIFPL types."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_literal_pattern_first_match_wins(self, aifpl, helpers):
        """Test that pattern matching uses first-match-wins semantics."""
        helpers.assert_evaluates_to(
            aifpl,
            '(match 42 (42 "first") (42 "second") (_ "default"))',
            '"first"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(match "test" ("test" "first match") (_ "wildcard") ("test" "never reached"))',
            '"first match"'
        )

    # ========== Variable Binding Patterns ==========

    @pytest.mark.parametrize("expression,expected", [
        # Simple variable binding
        ('(match 42 (x (* x 2)))', '84'),
        ('(match "hello" (s (string-length s)))', '5'),
        ('(match #t (b (if b "yes" "no")))', '"yes"'),

        # Variable binding with multiple patterns
        ('(match 10 (5 "five") (x (+ x 1)))', '11'),
        ('(match "world" ("hello" "greeting") (s (string-upcase s)))', '"WORLD"'),

        # Variable binding in complex expressions
        ('(match 7 (n (if (> n 5) "big" "small")))', '"big"'),
        ('(match 3 (n (if (> n 5) "big" "small")))', '"small"'),
    ])
    def test_variable_binding_patterns(self, aifpl, expression, expected):
        """Test variable binding in patterns."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_variable_binding_environment_isolation(self, aifpl, helpers):
        """Test that pattern variables don't leak outside match expressions."""
        # Variable bound in pattern should not be available outside
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((result (match 42 (x (* x 2))))) result)',
            '84'
        )

        # Variable x should not be defined outside the match
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate('(let ((result (match 42 (x (* x 2))))) (+ result x))')

    def test_variable_binding_shadowing(self, aifpl, helpers):
        """Test that pattern variables can shadow outer bindings."""
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((x 10)) (match 5 (x (* x 3))))',
            '15'  # Inner x (5) shadows outer x (10)
        )

        # Outer x should still be accessible after match
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((x 10)) (let ((result (match 5 (x (* x 3))))) (+ result x)))',
            '25'  # 15 + 10
        )

    # ========== Wildcard Patterns ==========

    @pytest.mark.parametrize("expression,expected", [
        # Basic wildcard patterns
        ('(match 42 (100 "hundred") (_ "other"))', '"other"'),
        ('(match "test" ("hello" "greeting") (_ "unknown"))', '"unknown"'),
        ('(match #f (#t "true") (_ "not true"))', '"not true"'),

        # Wildcard as catch-all
        ('(match (list 1 2 3) ((list 1 2) "short") (_ "other"))', '"other"'),
        ('(match 3.14 (2.71 "e") (_ "not e"))', '"not e"'),

        # Multiple wildcards (should all match but first wins)
        ('(match 42 (_ "first wildcard") (_ "second wildcard"))', '"first wildcard"'),
    ])
    def test_wildcard_patterns(self, aifpl, expression, expected):
        """Test wildcard pattern matching."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_wildcard_no_binding(self, aifpl):
        """Test that wildcard patterns don't create variable bindings."""
        # Wildcard should not create a binding for _
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate('(match 42 (_ (+ _ 1)))')

    # ========== Type Pattern Matching ==========

    @pytest.mark.parametrize("expression,expected", [
        # Number type patterns
        ('(match 42 ((number? n) (+ n 1)) (_ "not number"))', '43'),
        ('(match "hello" ((number? n) (+ n 1)) (_ "not number"))', '"not number"'),

        # Integer type patterns
        ('(match 42 ((integer? i) (* i 2)) (_ "not integer"))', '84'),
        ('(match 3.14 ((integer? i) (* i 2)) (_ "not integer"))', '"not integer"'),

        # Float type patterns
        ('(match 3.14 ((float? f) (round f)) (_ "not float"))', '3'),
        ('(match 42 ((float? f) (round f)) (_ "not float"))', '"not float"'),

        # Complex type patterns
        ('(match (complex 1 2) ((complex? c) (real c)) (_ "not complex"))', '1'),
        ('(match 42 ((complex? c) (real c)) (_ "not complex"))', '"not complex"'),

        # String type patterns
        ('(match "hello" ((string? s) (string-length s)) (_ "not string"))', '5'),
        ('(match 42 ((string? s) (string-length s)) (_ "not string"))', '"not string"'),

        # Boolean type patterns
        ('(match #t ((boolean? b) (not b)) (_ "not boolean"))', '#f'),
        ('(match 42 ((boolean? b) (not b)) (_ "not boolean"))', '"not boolean"'),

        # List type patterns
        ('(match (list 1 2 3) ((list? l) (length l)) (_ "not list"))', '3'),
        ('(match 42 ((list? l) (length l)) (_ "not list"))', '"not list"'),

        # Function type patterns
        ('(match (lambda (x) x) ((function? f) "is function") (_ "not function"))', '"is function"'),
        ('(match 42 ((function? f) "is function") (_ "not function"))', '"not function"'),
    ])
    def test_type_pattern_matching(self, aifpl, expression, expected):
        """Test type-based pattern matching."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_type_pattern_binding(self, aifpl, helpers):
        """Test that type patterns bind variables correctly."""
        # Variable should be bound and usable in the body
        helpers.assert_evaluates_to(
            aifpl,
            '(match 42 ((number? n) (let ((doubled (* n 2))) doubled)))',
            '84'
        )

        # Type pattern with complex usage
        helpers.assert_evaluates_to(
            aifpl,
            '(match "hello world" ((string? s) (string-contains? s "world")))',
            '#t'
        )

    def test_type_pattern_precedence(self, aifpl, helpers):
        """Test type pattern matching with overlapping types."""
        # Integer is also a number, first match should win
        helpers.assert_evaluates_to(
            aifpl,
            '(match 42 ((number? n) "number") ((integer? i) "integer"))',
            '"number"'
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(match 42 ((integer? i) "integer") ((number? n) "number"))',
            '"integer"'
        )

    # ========== List Structure Pattern Matching ==========

    @pytest.mark.parametrize("expression,expected", [
        # Empty list pattern
        ('(match (list) (() "empty") (_ "non-empty"))', '"empty"'),
        ('(match (list 1) (() "empty") (_ "non-empty"))', '"non-empty"'),

        # Fixed-length list patterns
        ('(match (list 1 2 3) ((a b c) (+ a b c)) (_ "wrong length"))', '6'),
        ('(match (list 1 2) ((a b c) (+ a b c)) (_ "wrong length"))', '"wrong length"'),
        ('(match (list "x" "y") ((a b) (string-append a b)) (_ "other"))', '"xy"'),

        # Single element list patterns
        ('(match (list 42) ((x) (* x 2)) (_ "not single"))', '84'),
        ('(match (list 1 2) ((x) (* x 2)) (_ "not single"))', '"not single"'),

        # Nested list patterns
        ('(match (list (list 1 2) (list 3 4)) (((a b) (c d)) (+ a b c d)) (_ "other"))', '10'),
    ])
    def test_list_structure_patterns(self, aifpl, expression, expected):
        """Test list structure pattern matching."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Head/tail patterns
        ('(match (list 1 2 3) ((head . tail) head) (_ "not list"))', '1'),
        ('(match (list 1 2 3) ((head . tail) (length tail)) (_ "not list"))', '2'),
        ('(match (list 42) ((head . tail) (list head (null? tail))) (_ "other"))', '(42 #t)'),

        # Multiple elements with tail
        ('(match (list 1 2 3 4) ((a b . rest) (list a b (length rest))) (_ "other"))', '(1 2 2)'),
        ('(match (list 1 2) ((a b . rest) (list a b (null? rest))) (_ "other"))', '(1 2 #t)'),

        # Empty tail cases
        ('(match (list) ((head . tail) "non-empty") (_ "empty"))', '"empty"'),
    ])
    def test_head_tail_patterns(self, aifpl, expression, expected):
        """Test head/tail (cons) pattern matching."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_list_pattern_variable_binding(self, aifpl, helpers):
        """Test variable binding in list patterns."""
        # All variables should be bound
        helpers.assert_evaluates_to(
            aifpl,
            '(match (list 10 20 30) ((x y z) (list z y x)))',
            '(30 20 10)'
        )

        # Head/tail binding
        helpers.assert_evaluates_to(
            aifpl,
            '(match (list 1 2 3 4 5) ((first second . others) (list first (length others))))',
            '(1 3)'
        )

    # ========== Nested Pattern Combinations ==========

    def test_nested_type_and_structure_patterns(self, aifpl, helpers):
        """Test combinations of type patterns and structure patterns."""
        # List of numbers
        helpers.assert_evaluates_to(
            aifpl,
            '(match (list 1 2 3) ((list? l) (if (> (length l) 2) "long list" "short list")))',
            '"long list"'
        )

        # Number in specific position
        helpers.assert_evaluates_to(
            aifpl,
            '(match (list 10 "hello" #t) ((a (string? s) c) (list a (string-length s) c)))',
            '(10 5 #t)'
        )

    def test_deeply_nested_patterns(self, aifpl, helpers):
        """Test deeply nested pattern combinations."""
        # Nested list with type patterns
        complex_pattern = '''
        (match (list (list 1 2) "test" (list 3 4))
               (((a b) (string? s) (c d)) (list (+ a b c d) (string-length s)))
               (_ "no match"))
        '''
        helpers.assert_evaluates_to(aifpl, complex_pattern, '(10 4)')

        # Mixed patterns with multiple levels
        mixed_pattern = '''
        (match (list 42 (list "x" "y") #t)
               (((number? n) (a b) (boolean? flag))
                (if flag (+ n (string-length (string-append a b))) n))
               (_ "no match"))
        '''
        helpers.assert_evaluates_to(aifpl, mixed_pattern, '44')  # 42 + 2

    def test_pattern_with_guards(self, aifpl, helpers):
        """Test patterns combined with conditional logic."""
        # Pattern matching with additional conditions
        guarded_pattern = '''
        (match 15
               ((number? n) (if (> n 10) (* n 2) n))
               (_ "not number"))
        '''
        helpers.assert_evaluates_to(aifpl, guarded_pattern, '30')

        guarded_pattern_2 = '''
        (match 5
               ((number? n) (if (> n 10) (* n 2) n))
               (_ "not number"))
        '''
        helpers.assert_evaluates_to(aifpl, guarded_pattern_2, '5')

    # ========== Integration with Other AIFPL Constructs ==========

    def test_pattern_matching_with_let(self, aifpl, helpers):
        """Test pattern matching integration with let bindings."""
        let_with_match = '''
        (let ((data (list 1 2 3)))
          (match data
                 ((a b c) (+ a b c))
                 (_ 0)))
        '''
        helpers.assert_evaluates_to(aifpl, let_with_match, '6')

        # Let binding used in pattern result
        let_in_result = '''
        (match 42
               (x (let ((doubled (* x 2))
                        (tripled (* x 3)))
                    (+ doubled tripled)))
               (_ 0))
        '''
        helpers.assert_evaluates_to(aifpl, let_in_result, '210')  # 84 + 126

    def test_pattern_matching_with_lambda(self, aifpl, helpers):
        """Test pattern matching with lambda expressions."""
        # Lambda in pattern result
        lambda_result = '''
        (match 5
               (x ((lambda (n) (* n n)) x))
               (_ 0))
        '''
        helpers.assert_evaluates_to(aifpl, lambda_result, '25')

        # Pattern matching function
        pattern_function = '''
        (let ((matcher (lambda (val)
                        (match val
                               ((number? n) (* n 2))
                               ((string? s) (string-length s))
                               (_ 0)))))
          (list (matcher 21) (matcher "hello") (matcher #t)))
        '''
        helpers.assert_evaluates_to(aifpl, pattern_function, '(42 5 0)')

    def test_pattern_matching_with_higher_order_functions(self, aifpl, helpers):
        """Test pattern matching with map, filter, fold."""
        # Map with pattern matching
        map_with_match = '''
        (map (lambda (item)
               (match item
                      ((number? n) (* n 2))
                      ((string? s) (string-length s))
                      (_ 0)))
             (list 5 "hello" #t 10))
        '''
        helpers.assert_evaluates_to(aifpl, map_with_match, '(10 5 0 20)')

        # Filter with pattern matching
        filter_with_match = '''
        (filter (lambda (item)
                  (match item
                         ((number? n) (> n 5))
                         (_ #f)))
                (list 1 10 "hello" 7 3))
        '''
        helpers.assert_evaluates_to(aifpl, filter_with_match, '(10 7)')

    def test_nested_match_expressions(self, aifpl, helpers):
        """Test nested match expressions."""
        nested_match = '''
        (match (list 1 (list 2 3))
               ((a (list? inner))
                (match inner
                       ((b c) (+ a b c))
                       (_ a)))
               (_ 0))
        '''
        helpers.assert_evaluates_to(aifpl, nested_match, '6')  # 1 + 2 + 3

    # ========== Error Cases ==========

    def test_match_requires_minimum_arguments(self, aifpl):
        """Test that match requires at least a value and one pattern."""
        # No arguments
        with pytest.raises(AIFPLEvalError, match="Match expression has wrong number of arguments"):
            aifpl.evaluate('(match)')

        # Only value, no patterns
        with pytest.raises(AIFPLEvalError, match="Match expression has wrong number of arguments"):
            aifpl.evaluate('(match 42)')

    def test_match_requires_pattern_value_pairs(self, aifpl):
        """Test that match requires pattern-value pairs."""
        # Odd number of pattern arguments (missing result for last pattern)
        with pytest.raises(AIFPLEvalError, match="Match clause 2 has wrong number of elements"):
            aifpl.evaluate('(match 42 (42 "found") (43))')

        with pytest.raises(AIFPLEvalError, match="Match clause 1 has wrong number of elements"):
            aifpl.evaluate('(match 42 (x))')

    def test_no_matching_pattern_error(self, aifpl):
        """Test error when no pattern matches."""
        with pytest.raises(AIFPLEvalError, match="No patterns matched in match expression"):
            aifpl.evaluate('(match 42 (43 "wrong") (44 "also wrong"))')

        with pytest.raises(AIFPLEvalError, match="No patterns matched in match expression"):
            aifpl.evaluate('(match "hello" ("world" "wrong") ("test" "also wrong"))')

    def test_invalid_pattern_syntax_errors(self, aifpl):
        """Test errors for invalid pattern syntax."""
        # Invalid type pattern (not a proper type predicate call)
        with pytest.raises(AIFPLEvalError, match="Invalid type pattern"):
            aifpl.evaluate('(match 42 ((number?) "invalid") (_ "other"))')

        # Invalid type pattern (wrong number of arguments)
        with pytest.raises(AIFPLEvalError, match="Invalid type pattern"):
            aifpl.evaluate('(match 42 ((number? x y) "invalid") (_ "other"))')

        # Invalid cons pattern (more than one dot)
        with pytest.raises(AIFPLEvalError, match="Invalid cons pattern"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b . c) "invalid") (_ "other"))')

        # Invalid cons pattern (dot at beginning)
        with pytest.raises(AIFPLEvalError, match="Invalid cons pattern"):
            aifpl.evaluate('(match (list 1 2 3) ((. a b) "invalid") (_ "other"))')

        with pytest.raises(AIFPLEvalError, match="Invalid type pattern"):
            aifpl.evaluate('(match (3) ((number2? x) "invalid") (_ "other"))')

    def test_invalid_variable_patterns(self, aifpl):
        """Test errors for invalid variable patterns."""
        # String as variable pattern in type pattern - this should fail validation
        with pytest.raises(AIFPLEvalError, match="Pattern variable must be a symbol"):
            aifpl.evaluate('(match 42 ((number? "x") "invalid") (_ "other"))')

    def test_list_pattern_length_mismatch(self, aifpl):
        """Test that list patterns with wrong length fall through to wildcard."""
        # Pattern expects 3 elements, value has 2 - should match wildcard
        result = aifpl.evaluate_and_format('(match (list 1 2) ((a b c) "three") (_ "other"))')
        assert result == '"other"'

        # Pattern expects 2 elements, value has 3 - should match wildcard
        result = aifpl.evaluate_and_format('(match (list 1 2 3) ((a b) "two") (_ "other"))')
        assert result == '"other"'

        # Test actual no-match error (no wildcard)
        with pytest.raises(AIFPLEvalError, match="No patterns matched"):
            aifpl.evaluate('(match (list 1 2) ((a b c) "three"))')

    def test_type_pattern_with_wrong_type(self, aifpl):
        """Test type patterns with wrong types fall through correctly."""
        # String doesn't match number? pattern, should match string? pattern
        result = aifpl.evaluate_and_format('(match "hello" ((number? n) "number") ((string? s) "string"))')
        assert result == '"string"'

        # Test actual no-match error (no matching patterns)
        with pytest.raises(AIFPLEvalError, match="No patterns matched"):
            aifpl.evaluate('(match "hello" ((number? n) "number"))')

    def test_cons_pattern_with_non_list(self, aifpl):
        """Test cons patterns with non-list values fall through to wildcard."""
        # Number doesn't match cons pattern, should match wildcard
        result = aifpl.evaluate_and_format('(match 42 ((head . tail) "list") (_ "other"))')
        assert result == '"other"'

        # Test actual no-match error (no wildcard)
        with pytest.raises(AIFPLEvalError, match="No patterns matched"):
            aifpl.evaluate('(match 42 ((head . tail) "list"))')

    def test_error_in_pattern_result_evaluation(self, aifpl):
        """Test error handling in pattern result evaluation."""
        # Division by zero in pattern result
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate('(match 42 (x (/ x 0)) (_ "other"))')

        # Type error in pattern result
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(match 42 (x (+ x "hello")) (_ "other"))')

    def test_undefined_variable_in_pattern_result(self, aifpl):
        """Test undefined variable errors in pattern results."""
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate('(match 42 (x (+ x undefined-var)) (_ "other"))')

    # ========== Practical Examples and Real-World Usage ==========

    def test_list_processing_examples(self, aifpl, helpers):
        """Test practical list processing with pattern matching."""
        # Safe list operations
        safe_head = '''
        (let ((safe-first (lambda (lst)
                           (match lst
                                  (() "empty")
                                  ((head . tail) head)))))
          (list (safe-first (list 1 2 3))
                (safe-first (list))))
        '''
        helpers.assert_evaluates_to(aifpl, safe_head, '(1 "empty")')

        # List length calculation
        list_length = '''
        (let ((my-length (lambda (lst)
                          (match lst
                                 (() 0)
                                 ((head . tail) (+ 1 (my-length tail)))))))
          (my-length (list 1 2 3 4 5)))
        '''
        helpers.assert_evaluates_to(aifpl, list_length, '5')

    def test_data_structure_processing(self, aifpl, helpers):
        """Test pattern matching for data structure processing."""
        # Processing different data types
        data_processor = '''
        (let ((process (lambda (data)
                        (match data
                               ((number? n) (if (> n 0) "positive" "non-positive"))
                               ((string? s) (if (> (string-length s) 5) "long" "short"))
                               ((list? l) (if (null? l) "empty-list" "non-empty-list"))
                               (_ "unknown")))))
          (list (process 42)
                (process -5)
                (process "hello world")
                (process "hi")
                (process (list 1 2))
                (process (list))
                (process #t)))
        '''
        helpers.assert_evaluates_to(
            aifpl,
            data_processor,
            '("positive" "non-positive" "long" "short" "non-empty-list" "empty-list" "unknown")'
        )

    def test_tree_like_structure_processing(self, aifpl, helpers):
        """Test pattern matching with tree-like structures."""
        # Simple binary tree sum (represented as nested lists)
        tree_sum = '''
        (let ((sum-tree (lambda (tree)
                         (match tree
                                ((number? n) n)
                                ((left right) (+ (sum-tree left) (sum-tree right)))
                                (_ 0)))))
          (sum-tree (list (list 1 2) (list 3 4))))
        '''
        helpers.assert_evaluates_to(aifpl, tree_sum, '10')

    def test_option_type_simulation(self, aifpl, helpers):
        """Test simulating option types with pattern matching."""
        # Simulating Maybe/Option type with lists
        option_example = '''
        (let ((safe-divide (lambda (a b)
                            (if (= b 0)
                                (list "none")
                                (list "some" (/ a b)))))
              (get-value (lambda (option)
                          (match option
                                 (("none") "no value")
                                 (("some" value) value)
                                 (_ "invalid option")))))
          (list (get-value (safe-divide 10 2))
                (get-value (safe-divide 10 0))))
        '''
        helpers.assert_evaluates_to(aifpl, option_example, '(5.0 "no value")')

    def test_command_pattern_matching(self, aifpl, helpers):
        """Test pattern matching for command-like structures."""
        # Simple command processor
        command_processor = '''
        (let ((execute (lambda (cmd)
                        (match cmd
                               (("add" (number? a) (number? b)) (+ a b))
                               (("multiply" (number? a) (number? b)) (* a b))
                               (("greet" (string? name)) (string-append "Hello, " name))
                               (("length" (string? s)) (string-length s))
                               (_ "unknown command")))))
          (list (execute (list "add" 5 3))
                (execute (list "multiply" 4 7))
                (execute (list "greet" "World"))
                (execute (list "length" "testing"))
                (execute (list "unknown" 1 2))))
        '''
        helpers.assert_evaluates_to(
            aifpl,
            command_processor,
            '(8 28 "Hello, World" 7 "unknown command")'
        )

    def test_pattern_matching_performance_edge_cases(self, aifpl, helpers):
        """Test pattern matching with edge cases and performance considerations."""
        # Many patterns (should still use first-match-wins)
        many_patterns = '''
        (match 50
               (1 "one") (2 "two") (3 "three") (4 "four") (5 "five")
               (10 "ten") (20 "twenty") (30 "thirty") (40 "forty")
               ((number? n) (if (> n 45) "big number" "medium number"))
               (_ "unknown"))
        '''
        helpers.assert_evaluates_to(aifpl, many_patterns, '"big number"')

        # Deeply nested list patterns
        deep_nesting = '''
        (match (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8)))
               ((((a b) (c d)) ((e f) (g h))) (+ a b c d e f g h))
               (_ "no match"))
        '''
        helpers.assert_evaluates_to(aifpl, deep_nesting, '36')  # 1+2+3+4+5+6+7+8

    def test_pattern_matching_with_complex_expressions(self, aifpl, helpers):
        """Test pattern matching integrated with complex AIFPL expressions."""
        # Pattern matching in fold operation
        complex_fold = '''
        (fold (lambda (acc item)
                (match item
                       ((number? n) (+ acc n))
                       ((string? s) (+ acc (string-length s)))
                       (_ acc)))
              0
              (list 10 "hello" #t 20 "world" (list 1 2)))
        '''
        helpers.assert_evaluates_to(aifpl, complex_fold, '40')  # 10 + 5 + 20 + 5

        # Pattern matching with recursive function
        recursive_pattern = '''
        (let ((process-nested (lambda (data)
                               (match data
                                      ((number? n) n)
                                      ((string? s) (string-length s))
                                      ((list? l) (fold + 0 (map process-nested l)))
                                      (_ 0)))))
          (process-nested (list 10 "test" (list 5 "hi") 20)))
        '''
        helpers.assert_evaluates_to(aifpl, recursive_pattern, '41')  # 10 + 4 + (5 + 2) + 20

    # ========== CORRECTED DOT PATTERN VALIDATION TESTS ==========

    def test_dot_pattern_validation_comprehensive_fixed(self, aifpl):
        """Comprehensive tests for dot pattern validation edge cases - updated for refactored code."""

        # These tests target the validation logic that now happens entirely upfront
        # in _validate_list_pattern_syntax (after refactoring)

        # Test cases that should be caught by early validation
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((. a b) "invalid"))')

        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b . c) "invalid"))')

        # Test cases that are now also caught by early validation (after refactoring)
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((a .) "invalid"))')

        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b c) "invalid"))')

    def test_dot_pattern_error_specificity_fixed(self, aifpl):
        """Test that dot pattern errors provide specific and helpful messages."""

        # Test specific error message content for dot at end
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(match (list 1 2) ((head .) "test"))')

        error = str(exc_info.value)
        assert "Invalid pattern in clause 1" in error
        assert "dot at end" in error

        # Test specific error message content for multiple elements after dot
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(match (list 1 2 3) ((head . tail extra) "test"))')

        error = str(exc_info.value)
        assert "Invalid pattern in clause 1" in error
        assert "multiple elements after dot" in error

    def test_dot_pattern_validation_vs_matching_separation_fixed(self, aifpl):
        """Test that validation errors are clearly separated from matching failures."""

        # These should be validation errors (syntax problems)
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((a .) "invalid"))')

        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b c) "invalid"))')

        # This should be a matching failure (semantic problem - not enough elements)
        result = aifpl.evaluate_and_format('(match (list 1) ((a b . rest) "matched") (_ "no match"))')
        assert result == '"no match"'

        # This should be a no-patterns-matched error (semantic problem)
        with pytest.raises(AIFPLEvalError, match="No patterns matched"):
            aifpl.evaluate('(match (list 1) ((a b . rest) "matched"))')

    def test_all_dot_validation_paths_covered_fixed(self, aifpl):
        """Ensure all validation paths in dot pattern handling are tested."""

        # All paths now go through early validation (after refactoring)

        # Path 1: Early validation catches multiple dots
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b . c) "invalid"))')

        # Path 2: Early validation catches dot at beginning
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((. a) "invalid"))')

        # Path 3: Early validation now catches dot at end (refactored!)
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1) ((a .) "invalid"))')

        # Path 4: Early validation now catches multiple elements after dot (refactored!)
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b c) "invalid"))')

        # Path 5: Valid dot patterns should work
        result = aifpl.evaluate_and_format('(match (list 1 2 3) ((head . tail) head))')
        assert result == '1'

        result = aifpl.evaluate_and_format('(match (list 1 2 3 4) ((a b . rest) (length rest)))')
        assert result == '2'
