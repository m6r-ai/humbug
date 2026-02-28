"""Tests for functional programming features: lambda, let, higher-order functions."""

import pytest

from menai import MenaiEvalError


class TestFunctional:
    """Test functional programming features."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic lambda expressions
        ('((lambda (x) x) 5)', '5'),  # Identity function
        ('((lambda (x) (integer* x 2)) 3)', '6'),  # Simple transformation
        ('((lambda (x y) (integer+ x y)) 3 4)', '7'),  # Multiple parameters

        # Lambda with no parameters
        ('((lambda () 42))', '42'),

        # Lambda with complex body
        ('((lambda (x) (integer+ (integer* x x) (integer* x 2))) 3)', '15'),  # x² + 2x where x=3
    ])
    def test_basic_lambda_expressions(self, menai, expression, expected):
        """Test basic lambda expressions and immediate calls."""
        assert menai.evaluate_and_format(expression) == expected

    def test_lambda_parameter_validation(self, menai):
        """Test lambda parameter validation (pure list approach - evaluation errors)."""
        # Duplicate parameters should cause error
        with pytest.raises(MenaiEvalError, match=r"parameters must be unique"):
            menai.evaluate('(lambda (x x) (integer+ x x))')

        with pytest.raises(MenaiEvalError, match=r"parameters must be unique"):
            menai.evaluate('(lambda (x y x) (integer+ x y))')

    def test_lambda_arity_checking(self, menai):
        """Test that lambda functions check argument count strictly."""
        # Too few arguments
        with pytest.raises(MenaiEvalError, match="expects 2 arguments, got 1"):
            menai.evaluate('((lambda (x y) (integer+ x y)) 5)')

        # Too many arguments
        with pytest.raises(MenaiEvalError, match="expects 2 arguments, got 3"):
            menai.evaluate('((lambda (x y) (integer+ x y)) 1 2 3)')

        # No parameters but arguments provided
        with pytest.raises(MenaiEvalError, match="expects 0 arguments, got 1"):
            menai.evaluate('((lambda () 42) 5)')

    @pytest.mark.parametrize("expression,expected", [
        # Basic let expressions
        ('(let ((x 5)) x)', '5'),
        ('(let ((x 5) (y 3)) (integer+ x y))', '8'),

        # Let with complex expressions
        ('(let ((x (integer+ 2 3)) (y (integer* 4 2))) (integer+ x y))', '13'),

        # Empty let (no bindings)
        ('(let () 42)', '42'),

        # Let with string and boolean bindings
        ('(let ((name "hello") (flag #t)) (if flag name "default"))', '"hello"'),
    ])
    def test_basic_let_expressions(self, menai, expression, expected):
        """Test basic let expressions with local bindings."""
        assert menai.evaluate_and_format(expression) == expected

    def test_let_sequential_binding(self, menai, helpers):
        """Test that let* bindings are sequential (later can reference earlier)."""
        helpers.assert_evaluates_to(
            menai,
            '(let* ((x 5) (y (integer* x 2))) (integer+ x y))',
            '15'  # x=5, y=10, sum=15
        )

        helpers.assert_evaluates_to(
            menai,
            '(let* ((a 3) (b (integer+ a 2)) (c (integer* b 2))) c)',
            '10'  # a=3, b=5, c=10
        )

    def test_let_variable_shadowing(self, menai, helpers):
        """Test that let variables shadow outer bindings."""
        # This requires nested scopes, which we can test with nested lets
        helpers.assert_evaluates_to(
            menai,
            '(let ((x 10)) (let ((x 5)) x))',
            '5'  # Inner x shadows outer x
        )

        helpers.assert_evaluates_to(
            menai,
            '(let ((x 10) (y 20)) (let ((x 5)) (integer+ x y)))',
            '25'  # Inner x=5, outer y=20
        )

    def test_let_duplicate_binding_error(self, menai):
        """Test that duplicate binding names in let cause error (pure list approach - evaluation errors)."""
        with pytest.raises(MenaiEvalError, match=r"variable names should be different"):
            menai.evaluate('(let ((x 1) (x 2)) x)')

        with pytest.raises(MenaiEvalError, match=r"variable names should be different"):
            menai.evaluate('(let ((x 1) (y 2) (x 3)) (integer+ x y))')

    def test_lambda_closures(self, menai, helpers):
        """Test that lambda expressions form closures over their environment."""
        # Lambda capturing variable from let
        closure_expr = '''
        (let ((multiplier 10))
          ((lambda (x) (integer* x multiplier)) 5))
        '''
        helpers.assert_evaluates_to(menai, closure_expr, '50')

        # More complex closure
        complex_closure = '''
        (let ((base 100) (factor 2))
          ((lambda (x y) (integer+ base (integer* factor (integer+ x y)))) 3 7))
        '''
        helpers.assert_evaluates_to(menai, complex_closure, '120')  # 100 + 2*(3+7) = 120

    def test_lambda_in_let_binding(self, menai, helpers):
        """Test lambda expressions defined in let bindings."""
        helpers.assert_evaluates_to(
            menai,
            '(let ((double (lambda (x) (integer* x 2)))) (double 21))',
            '42'
        )

        helpers.assert_evaluates_to(
            menai,
            '(let ((add (lambda (x y) (integer+ x y)))) (add 15 27))',
            '42'
        )

        # Multiple lambda bindings
        multi_lambda = '''
        (let ((double (lambda (x) (integer* x 2)))
              (square (lambda (x) (integer* x x))))
          (integer+ (double 5) (square 3)))
        '''
        helpers.assert_evaluates_to(menai, multi_lambda, '19')  # 10 + 9 = 19

    @pytest.mark.parametrize("expression,expected", [
        # Basic map operations
        ('(list-map (lambda (x) (integer* x 2)) (list 1 2 3))', '(2 4 6)'),
        ('(list-map (lambda (x) (integer+ x 1)) (list 0 1 2))', '(1 2 3)'),

        # Map with empty list
        ('(list-map (lambda (x) (integer* x 2)) (list))', '()'),

        # Map with single element
        ('(list-map (lambda (x) x) (list 42))', '(42)'),

        # Map with string transformation
        ('(list-map (lambda (s) (string-upcase s)) (list "hello" "world"))', '("HELLO" "WORLD")'),
    ])
    def test_map_function(self, menai, expression, expected):
        """Test map higher-order function."""
        assert menai.evaluate_and_format(expression) == expected

    def test_map_requires_function_and_list(self, menai):
        """Test that map requires exactly 2 arguments: function and list."""
        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 1"):
            menai.evaluate('(list-map (lambda (x) x))')

        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 3"):
            menai.evaluate('(list-map (lambda (x) x) (list 1 2) (list 3 4))')

        # Second argument must be list
        with pytest.raises(MenaiEvalError, match=r"requires list argument"):
            menai.evaluate('(list-map (lambda (x) x) 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Basic filter operations
        ('(list-filter (lambda (x) (integer>? x 0)) (list -1 2 -3 4))', '(2 4)'),
        ('(list-filter (lambda (x) (integer=? x 0)) (list 1 0 2 0 3))', '(0 0)'),

        # Filter with empty list
        ('(list-filter (lambda (x) #t) (list))', '()'),

        # Filter that matches nothing
        ('(list-filter (lambda (x) #f) (list 1 2 3))', '()'),

        # Filter that matches everything
        ('(list-filter (lambda (x) #t) (list 1 2 3))', '(1 2 3)'),

        # Filter with string predicate
        ('(list-filter (lambda (s) (integer? (string-index s "e"))) (list "hello" "world" "test"))', '("hello" "test")'),
    ])
    def test_filter_function(self, menai, expression, expected):
        """Test filter higher-order function."""
        assert menai.evaluate_and_format(expression) == expected

    def test_filter_requires_function_and_list(self, menai):
        """Test that filter requires exactly 2 arguments: function and list."""
        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 1"):
            menai.evaluate('(list-filter (lambda (x) #t))')

        # Second argument must be list
        with pytest.raises(MenaiEvalError, match=r"requires list argument"):
            menai.evaluate('(list-filter (lambda (x) #t) 42)')

    def test_filter_predicate_must_return_boolean(self, menai):
        """Test that filter predicate must return boolean."""
        with pytest.raises(MenaiEvalError, match="condition must be boolean"):
            menai.evaluate('(list-filter (lambda (x) x) (list 1 2 3))')

        with pytest.raises(MenaiEvalError, match="condition must be boolean"):
            menai.evaluate('(list-filter (lambda (x) "hello") (list 1 2 3))')

    @pytest.mark.parametrize("expression,expected", [
        # Basic fold operations (left fold)
        ('(list-fold integer+ 0 (list 1 2 3 4))', '10'),  # Sum
        ('(list-fold integer* 1 (list 1 2 3 4))', '24'),  # Product
        ('(list-fold integer- 0 (list 1 2 3))', '-6'),  # ((0-1)-2)-3 = -6

        # Fold with empty list returns initial value
        ('(list-fold integer+ 0 (list))', '0'),
        ('(list-fold integer* 1 (list))', '1'),

        # Fold with single element
        ('(list-fold integer+ 10 (list 5))', '15'),

        # Fold for list construction (reverse)
        ('(list-fold (lambda (acc x) (list-prepend acc x)) (list) (list 1 2 3))', '(3 2 1)'),
    ])
    def test_fold_function(self, menai, expression, expected):
        """Test fold higher-order function."""
        assert menai.evaluate_and_format(expression) == expected

    def test_fold_requires_three_arguments(self, menai):
        """Test that fold requires exactly 3 arguments: function, initial, list."""
        with pytest.raises(MenaiEvalError, match=r"expects 3 arguments, got 2"):
            menai.evaluate('(list-fold integer+ 0)')

        with pytest.raises(MenaiEvalError, match=r"expects 3 arguments, got 4"):
            menai.evaluate('(list-fold integer+ 0 (list 1 2) 99)')

        # Third argument must be list
        with pytest.raises(MenaiEvalError, match=r"requires list argument"):
            menai.evaluate('(list-fold integer+ 0 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Basic range generation
        ('(range 1 5)', '(1 2 3 4)'),
        ('(range 0 3)', '(0 1 2)'),
        ('(range 5 5)', '()'),  # Empty range

        # Range with step
        ('(range 0 10 2)', '(0 2 4 6 8)'),
        ('(range 1 8 3)', '(1 4 7)'),

        # Negative step
        ('(range 5 0 -1)', '(5 4 3 2 1)'),
        ('(range 10 0 -2)', '(10 8 6 4 2)'),

        # Single element ranges
        ('(range 0 1)', '(0)'),
        ('(range -1 0)', '(-1)'),
    ])
    def test_range_function(self, menai, expression, expected):
        """Test range function for generating sequences."""
        assert menai.evaluate_and_format(expression) == expected

    def test_range_requires_numeric_arguments(self, menai):
        """Test that range requires numeric arguments."""
        with pytest.raises(MenaiEvalError, match="requires integer argument"):
            menai.evaluate('(range "hello" 5)')

        with pytest.raises(MenaiEvalError, match="requires integer argument"):
            menai.evaluate('(range 1 "world")')

        with pytest.raises(MenaiEvalError, match="requires integer argument"):
            menai.evaluate('(range 1 5 "step")')

    def test_range_zero_step_error(self, menai):
        """Test that range with zero step raises error."""
        with pytest.raises(MenaiEvalError, match="step cannot be zero"):
            menai.evaluate('(range 1 5 0)')

    def test_range_argument_count(self, menai):
        """Test that range accepts 2 or 3 arguments."""
        with pytest.raises(MenaiEvalError, match=r"Function 'range' has wrong number of arguments"):
            menai.evaluate('(range 1)')

        with pytest.raises(MenaiEvalError, match=r"Function 'range' has wrong number of arguments"):
            menai.evaluate('(range 1 5 2 99)')

    @pytest.mark.parametrize("expression,expected", [
        # Basic find operations
        ('(list-find (lambda (x) (integer>? x 5)) (list 1 3 7 2))', '7'),
        ('(list-find (lambda (x) (integer=? x 0)) (list 1 2 0 3))', '0'),

        # Find with no match returns #f
        ('(list-find (lambda (x) (integer>? x 10)) (list 1 2 3))', '#none'),

        # Find in empty list returns #f
        ('(list-find (lambda (x) #t) (list))', '#none'),

        # Find first match (short-circuit)
        ('(list-find (lambda (x) (integer>? x 2)) (list 1 3 5 7))', '3'),

        # Find with string predicate
        ('(list-find (lambda (s) (integer? (string-index s "o"))) (list "hello" "world" "test"))', '"hello"'),
    ])
    def test_find_function(self, menai, expression, expected):
        """Test find higher-order function."""
        assert menai.evaluate_and_format(expression) == expected

    def test_find_requires_function_and_list(self, menai):
        """Test that find requires exactly 2 arguments."""
        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 1"):
            menai.evaluate('(list-find (lambda (x) #t))')

        # Second argument must be list
        with pytest.raises(MenaiEvalError, match=r"requires list argument"):
            menai.evaluate('(list-find (lambda (x) #t) 42)')

    def test_find_predicate_must_return_boolean(self, menai):
        """Test that find predicate must return boolean."""
        with pytest.raises(MenaiEvalError, match="condition must be boolean"):
            menai.evaluate('(list-find (lambda (x) x) (list 1 2 3))')

    @pytest.mark.parametrize("expression,expected", [
        # Basic any? operations
        ('(list-any? (lambda (x) (integer>? x 5)) (list 1 3 7))', '#t'),
        ('(list-any? (lambda (x) (integer>? x 10)) (list 1 3 7))', '#f'),

        # any? with empty list returns #f
        ('(list-any? (lambda (x) #t) (list))', '#f'),

        # any? short-circuits on first true
        ('(list-any? (lambda (x) (integer=? x 2)) (list 1 2 3))', '#t'),
    ])
    def test_any_predicate_function(self, menai, expression, expected):
        """Test any? higher-order predicate function."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic all? operations
        ('(list-all? (lambda (x) (integer>? x 0)) (list 1 3 7))', '#t'),
        ('(list-all? (lambda (x) (integer>? x 5)) (list 1 3 7))', '#f'),

        # all? with empty list returns #t (vacuous truth)
        ('(list-all? (lambda (x) #f) (list))', '#t'),

        # all? short-circuits on first false
        ('(list-all? (lambda (x) (integer<? x 5)) (list 1 2 6 3))', '#f'),
    ])
    def test_all_predicate_function(self, menai, expression, expected):
        """Test all? higher-order predicate function."""
        assert menai.evaluate_and_format(expression) == expected

    def test_any_all_require_function_and_list(self, menai):
        """Test that any? and all? require function and list arguments."""
        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 1"):
            menai.evaluate('(list-any? (lambda (x) #t))')

        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 1"):
            menai.evaluate('(list-all? (lambda (x) #t))')

        # Second argument must be list
        with pytest.raises(MenaiEvalError, match=r"requires list argument"):
            menai.evaluate('(list-any? (lambda (x) #t) 42)')

        with pytest.raises(MenaiEvalError, match=r"requires list argument"):
            menai.evaluate('(list-all? (lambda (x) #t) "hello")')

    def test_any_all_predicates_must_return_boolean(self, menai):
        """Test that any? and all? predicates must return boolean."""
        with pytest.raises(MenaiEvalError, match="condition must be boolean"):
            menai.evaluate('(list-any? (lambda (x) x) (list 1 2 3))')

        with pytest.raises(MenaiEvalError, match="condition must be boolean"):
            menai.evaluate('(list-all? (lambda (x) "hello") (list 1 2 3))')

    def test_complex_functional_compositions(self, menai, helpers):
        """Test complex combinations of functional operations."""
        # Map followed by filter
        pipeline1 = '''
        (list-filter (lambda (x) (integer>? x 5))
                (list-map (lambda (x) (integer* x 2)) (list 1 2 3 4 5)))
        '''
        helpers.assert_evaluates_to(menai, pipeline1, '(6 8 10)')

        # Filter followed by fold
        pipeline2 = '''
        (list-fold integer+ 0
              (list-filter (lambda (x) (integer>? x 0)) (list -1 2 -3 4 5)))
        '''
        helpers.assert_evaluates_to(menai, pipeline2, '11')  # 2 + 4 + 5

        # Map, filter, and fold together
        pipeline3 = '''
        (list-fold integer*
              1
              (list-filter (lambda (x) (integer>? x 1))
                      (list-map (lambda (x) (integer* x x))
                           (list 1 2 3 4))))
        '''
        helpers.assert_evaluates_to(menai, pipeline3, '576')  # 4 * 9 * 16 = 576

    def test_recursive_lambda_functions(self, menai, helpers):
        """Test recursive lambda functions with tail call optimization."""
        # Factorial using tail recursion
        factorial_expr = '''
        (letrec ((factorial (lambda (n acc)
                             (if (integer<=? n 1) acc (factorial (integer- n 1) (integer* n acc))))))
          (factorial 5 1))
        '''
        helpers.assert_evaluates_to(menai, factorial_expr, '120')

        # Sum of list using tail recursion
        sum_list_expr = '''
        (letrec ((sum-list (lambda (lst acc)
                            (if (list-null? lst) 
                                acc 
                                (sum-list (list-rest lst) (integer+ acc (list-first lst)))))))
          (sum-list (list 1 2 3 4) 0))
        '''
        helpers.assert_evaluates_to(menai, sum_list_expr, '10')

    def test_mutual_recursion(self, menai, helpers):
        """Test mutual recursion between lambda functions."""
        # Even/odd mutual recursion
        even_odd_expr = '''
        (letrec ((is-even (lambda (n) (if (integer=? n 0) #t (is-odd (integer- n 1)))))
                 (is-odd (lambda (n) (if (integer=? n 0) #f (is-even (integer- n 1))))))
          (list (is-even 4) (is-odd 4) (is-even 7) (is-odd 7)))
        '''
        helpers.assert_evaluates_to(menai, even_odd_expr, '(#t #f #f #t)')

    def test_higher_order_function_composition(self, menai, helpers):
        """Test functions that take and return functions."""
        # Function composition
        compose_expr = '''
        (let* ((compose (lambda (f g) (lambda (x) (f (g x)))))
               (double (lambda (x) (integer* x 2)))
               (square (lambda (x) (integer* x x)))
               (double-then-square (compose square double)))
          (double-then-square 3))
        '''
        helpers.assert_evaluates_to(menai, compose_expr, '36')  # (3*2)² = 36

        # Curried functions
        curry_expr = '''
        (let* ((add (lambda (x) (lambda (y) (integer+ x y))))
               (add5 (add 5)))
          (add5 10))
        '''
        helpers.assert_evaluates_to(menai, curry_expr, '15')

    def test_lambda_with_complex_closures(self, menai, helpers):
        """Test lambda expressions with complex closure environments."""
        # Multiple nested closures
        nested_closure = '''
        (let ((x 10))
          (let ((make-adder (lambda (y) 
                             (lambda (z) (integer+ x y z)))))
            (let ((add-x-5 (make-adder 5)))
              (add-x-5 7))))
        '''
        helpers.assert_evaluates_to(menai, nested_closure, '22')  # 10 + 5 + 7

        # Closure capturing mutable-like behavior through let
        counter_like = '''
        (let ((base 100))
          (let ((increment (lambda (x) (integer+ base x)))
                (decrement (lambda (x) (integer- base x))))
            (list (increment 5) (decrement 3))))
        '''
        helpers.assert_evaluates_to(menai, counter_like, '(105 97)')

    def test_functional_data_processing_patterns(self, menai, helpers):
        """Test common functional programming patterns for data processing."""
        # Process list of numbers: square evens, filter > 10, sum
        data_processing = '''
        (list-fold integer+
              0
              (list-filter (lambda (x) (integer>? x 10))
                      (list-map (lambda (x) (if (integer=? (integer% x 2) 0) (integer* x x) x))
                           (list 1 2 3 4 5 6))))
        '''
        # 1->1, 2->4, 3->3, 4->16, 5->5, 6->36
        # Filter >10: 16, 36
        # Sum: 52
        helpers.assert_evaluates_to(menai, data_processing, '52')

        # String processing pipeline
        string_processing = '''
        (list-fold (lambda (acc s) (integer+ acc (string-length s)))
              0
              (list-filter (lambda (s) (integer? (string-index s "E")))
                      (list-map (lambda (s) (string-upcase s))
                           (list "hello" "world" "test" "code"))))
        '''
        # Map to uppercase: "HELLO", "WORLD", "TEST", "CODE"
        # Filter containing "E": "HELLO", "TEST", "CODE"
        # Sum lengths: 5 + 4 + 4 = 13
        helpers.assert_evaluates_to(menai, string_processing, '13')

    def test_range_with_functional_operations(self, menai, helpers):
        """Test range generation combined with functional operations."""
        # Sum of squares from 1 to 5
        sum_of_squares = '''
        (list-fold integer+ 0 (list-map (lambda (x) (integer* x x)) (range 1 6)))
        '''
        helpers.assert_evaluates_to(menai, sum_of_squares, '55')  # 1+4+9+16+25

        # Filter even numbers from range and double them
        even_doubled = '''
        (list-map (lambda (x) (integer* x 2))
             (list-filter (lambda (x) (integer=? (integer% x 2) 0))
                     (range 1 11)))
        '''
        helpers.assert_evaluates_to(menai, even_doubled, '(4 8 12 16 20)')

    def test_lambda_error_handling(self, menai):
        """Test error handling in lambda expressions."""
        # Undefined variable in lambda body
        with pytest.raises(MenaiEvalError, match="Undefined variable"):
            menai.evaluate('((lambda (x) (integer+ x undefined-var)) 5)')

        # Type error in lambda body
        with pytest.raises(MenaiEvalError):
            menai.evaluate('((lambda (x) (integer+ x "hello")) 5)')

        # Division by zero in lambda body
        with pytest.raises(MenaiEvalError):
            menai.evaluate('((lambda (x) (integer/ x 0)) 5)')

    def test_let_error_handling(self, menai):
        """Test error handling in let expressions."""
        # Error in binding expression
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(let ((x (integer/ 1 0))) x)')

        # Undefined variable in binding
        with pytest.raises(MenaiEvalError, match="Undefined variable"):
            menai.evaluate('(let ((x undefined-var)) x)')

        # Error in let body
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(let ((x 5)) (integer/ x 0))')

    def test_first_class_functions(self, menai, helpers):
        """Test that functions are first-class values."""
        # Store functions in lists
        function_list = '''
        (let ((funcs (list (lambda (x) (integer* x 2))
                           (lambda (x) (integer+ x 10))
                           (lambda (x) (integer* x x)))))
          (list ((list-first funcs) 5)
                ((list-ref funcs 1) 5)
                ((list-ref funcs 2) 5)))
        '''
        helpers.assert_evaluates_to(menai, function_list, '(10 15 25)')

        # Pass functions as arguments
        apply_twice = '''
        (let* ((apply-twice (lambda (f x) (f (f x))))
               (increment (lambda (x) (integer+ x 1))))
          (apply-twice increment 5))
        '''
        helpers.assert_evaluates_to(menai, apply_twice, '7')

        # Return functions from functions
        make_multiplier = '''
        (let* ((make-multiplier (lambda (n) (lambda (x) (integer* x n))))
               (times3 (make-multiplier 3)))
          (times3 7))
        '''
        helpers.assert_evaluates_to(menai, make_multiplier, '21')

    def test_complex_let_lambda_interaction(self, menai, helpers):
        """Test complex interactions between let and lambda."""
        # Lambda using multiple let-bound variables
        complex_interaction = '''
        (let ((a 5) (b 10) (c 2))
          (let ((compute (lambda (x) (float+ (integer->float (integer* a x)) (float* (integer->float b) (float/ (integer->float x) (integer->float c))))))
                (value 4))
            (compute value)))
        '''
        helpers.assert_evaluates_to(menai, complex_interaction, '40.0')  # 5*4 + 10*(4/2) = 20 + 20

        # Nested let with lambda closures
        nested_complex = '''
        (let ((outer 100))
          (let ((make-func (lambda (middle)
                            (lambda (inner) (integer+ outer middle inner)))))
            (let ((my-func (make-func 10)))
              (my-func 5))))
        '''
        helpers.assert_evaluates_to(menai, nested_complex, '115')  # 100 + 10 + 5

    def test_nested_lambdas_recursion_bug(self, menai, helpers):
        """
        Test cases that expose the _is_recursive_call bug.

        These tests fail with the original implementation due to false positive
        recursion detection when nested lambdas have the same name ("<lambda>").
        They should pass once the bug is fixed.
        """

        # Simple case: map with nested lambda
        helpers.assert_evaluates_to(
            menai,
            '(list-map (lambda (x) ((lambda (y) (integer* y 2)) x)) (list 1 2 3))',
            '(2 4 6)'
        )

        # Filter with nested lambda
        helpers.assert_evaluates_to(
            menai,
            '(list-filter (lambda (x) ((lambda (y) (integer>? y 0)) x)) (list -1 2 -3 4))',
            '(2 4)'
        )

        # More complex case: conditional nested lambdas in map
        helpers.assert_evaluates_to(
            menai,
            '''(let* ((process-list (lambda (lst)
                                     (list-map (lambda (x) 
                                            (if (integer>? x 0)
                                                ((lambda (y) (integer* y y)) x)
                                                ((lambda (z) (integer-neg z)) x)))
                                          lst))))
                  (process-list (list -2 3 -1 4)))''',
            '(2 9 1 16)'
        )

        # Fold with nested lambda
        helpers.assert_evaluates_to(
            menai,
            '(list-fold (lambda (acc x) ((lambda (y) (integer+ acc y)) (integer* x 2))) 0 (list 1 2 3))',
            '12'  # (0 + 2) + 4 + 6 = 12
        )

        # any? with nested lambda
        helpers.assert_evaluates_to(
            menai,
            '(list-any? (lambda (x) ((lambda (y) (integer>? y 5)) x)) (list 1 3 7))',
            '#t'
        )

        # all? with nested lambda
        helpers.assert_evaluates_to(
            menai,
            '(list-all? (lambda (x) ((lambda (y) (integer>? y 0)) x)) (list 1 3 7))',
            '#t'
        )

        # find with nested lambda
        helpers.assert_evaluates_to(
            menai,
            '(list-find (lambda (x) ((lambda (y) (integer=? y 3)) x)) (list 1 3 7))',
            '3'
        )

        # Complex nested structure with multiple levels
        helpers.assert_evaluates_to(
            menai,
            '''(list-map (lambda (x) 
                       (let ((helper (lambda (z) (integer* z 2))))
                         ((lambda (w) (integer+ (helper w) 1)) x)))
                   (list 1 2 3))''',
            '(3 5 7)'  # For each x: helper(x) + 1 = (x*2) + 1
        )

        # Test that ensures actual recursive functions still work correctly
        # (this should work both before and after the fix)
        helpers.assert_evaluates_to(
            menai,
            '''(letrec ((factorial (lambda (n)
                                   (if (integer<=? n 1) 
                                       1 
                                       (integer* n (factorial (integer- n 1)))))))
                 (factorial 4))''',
            '24'
        )

    def test_nested_lambdas_edge_cases(self, menai, helpers):
        """
        Additional edge cases for nested lambda recursion detection.

        These test more subtle scenarios that could expose the bug.
        """

        # Three levels of nesting
        helpers.assert_evaluates_to(
            menai,
            '''(list-map (lambda (x) 
                       ((lambda (y) 
                          ((lambda (z) (integer+ z 1)) (integer* y 2))) x))
                   (list 1 2 3))''',
            '(3 5 7)'
        )

        # Mix of named and anonymous functions
        helpers.assert_evaluates_to(
            menai,
            '''(let ((named-func (lambda (x) (integer* x 3))))
                 (list-map (lambda (x) 
                        ((lambda (y) (integer+ y 1)) (named-func x)))
                      (list 1 2 3)))''',
            '(4 7 10)'  # For each x: (x*3) + 1
        )

        # Nested lambdas in different higher-order function contexts
        helpers.assert_evaluates_to(
            menai,
            '''(let ((data (list 1 2 3 4 5)))
                 (list-fold integer+ 0 
                       (list-filter (lambda (x) ((lambda (y) (integer>? y 2)) x))
                               (list-map (lambda (x) ((lambda (y) (integer* y 2)) x)) 
                                    data))))''',
            '28'  # map: (2 4 6 8 10), filter: (4 6 8 10), fold: 4+6+8+10 = 28
        )

    @pytest.mark.parametrize("expression,expected", [
        # These are the core failing cases that expose the bug
        ('(list-map (lambda (x) ((lambda (y) (integer* y 2)) x)) (list 1 2 3))', '(2 4 6)'),
        ('(list-filter (lambda (x) ((lambda (y) (integer>? y 0)) x)) (list -1 2 -3 4))', '(2 4)'),
        ('(list-any? (lambda (x) ((lambda (y) (integer>? y 5)) x)) (list 1 3 7))', '#t'),
        ('(list-all? (lambda (x) ((lambda (y) (integer>? y 0)) x)) (list 1 3 7))', '#t'),
        ('(list-find (lambda (x) ((lambda (y) (integer=? y 3)) x)) (list 1 3 7))', '3'),
        ('(list-fold (lambda (acc x) ((lambda (y) (integer+ acc y)) (integer* x 2))) 0 (list 1 2 3))', '12'),
    ])
    def test_nested_lambda_bug_cases(self, menai, expression, expected):
        """
        Parameterized test for the core cases that expose the nested lambda bug.

        These expressions currently fail with "Unexpected tail call in higher-order 
        function context" due to the _is_recursive_call bug, but should work correctly
        once the bug is fixed.
        """
        assert menai.evaluate_and_format(expression) == expected
