"""Tests for functional programming features: lambda, let, higher-order functions."""

import pytest

from aifpl import AIFPL, AIFPLEvalError


class TestFunctional:
    """Test functional programming features."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic lambda expressions
        ('((lambda (x) x) 5)', '5'),  # Identity function
        ('((lambda (x) (* x 2)) 3)', '6'),  # Simple transformation
        ('((lambda (x y) (+ x y)) 3 4)', '7'),  # Multiple parameters
        
        # Lambda with no parameters
        ('((lambda () 42))', '42'),
        
        # Lambda with complex body
        ('((lambda (x) (+ (* x x) (* x 2))) 3)', '15'),  # x² + 2x where x=3
    ])
    def test_basic_lambda_expressions(self, aifpl, expression, expected):
        """Test basic lambda expressions and immediate calls."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_lambda_parameter_validation(self, aifpl):
        """Test lambda parameter validation."""
        # Duplicate parameters should cause error
        with pytest.raises(AIFPLEvalError, match="Duplicate lambda parameters"):
            aifpl.evaluate('(lambda (x x) (+ x x))')
        
        with pytest.raises(AIFPLEvalError, match="Duplicate lambda parameters"):
            aifpl.evaluate('(lambda (x y x) (+ x y))')

    def test_lambda_arity_checking(self, aifpl):
        """Test that lambda functions check argument count strictly."""
        # Too few arguments
        with pytest.raises(AIFPLEvalError, match="expects 2 arguments, got 1"):
            aifpl.evaluate('((lambda (x y) (+ x y)) 5)')
        
        # Too many arguments
        with pytest.raises(AIFPLEvalError, match="expects 2 arguments, got 3"):
            aifpl.evaluate('((lambda (x y) (+ x y)) 1 2 3)')
        
        # No parameters but arguments provided
        with pytest.raises(AIFPLEvalError, match="expects 0 arguments, got 1"):
            aifpl.evaluate('((lambda () 42) 5)')

    @pytest.mark.parametrize("expression,expected", [
        # Basic let expressions
        ('(let ((x 5)) x)', '5'),
        ('(let ((x 5) (y 3)) (+ x y))', '8'),
        
        # Let with complex expressions
        ('(let ((x (+ 2 3)) (y (* 4 2))) (+ x y))', '13'),
        
        # Empty let (no bindings)
        ('(let () 42)', '42'),
        
        # Let with string and boolean bindings
        ('(let ((name "hello") (flag #t)) (if flag name "default"))', '"hello"'),
    ])
    def test_basic_let_expressions(self, aifpl, expression, expected):
        """Test basic let expressions with local bindings."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_let_sequential_binding(self, aifpl, helpers):
        """Test that let bindings are sequential (later can reference earlier)."""
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((x 5) (y (* x 2))) (+ x y))',
            '15'  # x=5, y=10, sum=15
        )
        
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((a 3) (b (+ a 2)) (c (* b 2))) c)',
            '10'  # a=3, b=5, c=10
        )

    def test_let_variable_shadowing(self, aifpl, helpers):
        """Test that let variables shadow outer bindings."""
        # This requires nested scopes, which we can test with nested lets
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((x 10)) (let ((x 5)) x))',
            '5'  # Inner x shadows outer x
        )
        
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((x 10) (y 20)) (let ((x 5)) (+ x y)))',
            '25'  # Inner x=5, outer y=20
        )

    def test_let_duplicate_binding_error(self, aifpl):
        """Test that duplicate binding names in let cause error."""
        with pytest.raises(AIFPLEvalError, match="Duplicate let binding variables"):
            aifpl.evaluate('(let ((x 1) (x 2)) x)')
        
        with pytest.raises(AIFPLEvalError, match="Duplicate let binding variables"):
            aifpl.evaluate('(let ((x 1) (y 2) (x 3)) (+ x y))')

    def test_lambda_closures(self, aifpl, helpers):
        """Test that lambda expressions form closures over their environment."""
        # Lambda capturing variable from let
        closure_expr = '''
        (let ((multiplier 10))
          ((lambda (x) (* x multiplier)) 5))
        '''
        helpers.assert_evaluates_to(aifpl, closure_expr, '50')
        
        # More complex closure
        complex_closure = '''
        (let ((base 100) (factor 2))
          ((lambda (x y) (+ base (* factor (+ x y)))) 3 7))
        '''
        helpers.assert_evaluates_to(aifpl, complex_closure, '120')  # 100 + 2*(3+7) = 120

    def test_lambda_in_let_binding(self, aifpl, helpers):
        """Test lambda expressions defined in let bindings."""
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((double (lambda (x) (* x 2)))) (double 21))',
            '42'
        )
        
        helpers.assert_evaluates_to(
            aifpl,
            '(let ((add (lambda (x y) (+ x y)))) (add 15 27))',
            '42'
        )
        
        # Multiple lambda bindings
        multi_lambda = '''
        (let ((double (lambda (x) (* x 2)))
              (square (lambda (x) (* x x))))
          (+ (double 5) (square 3)))
        '''
        helpers.assert_evaluates_to(aifpl, multi_lambda, '19')  # 10 + 9 = 19

    @pytest.mark.parametrize("expression,expected", [
        # Basic map operations
        ('(map (lambda (x) (* x 2)) (list 1 2 3))', '(2 4 6)'),
        ('(map (lambda (x) (+ x 1)) (list 0 1 2))', '(1 2 3)'),
        
        # Map with empty list
        ('(map (lambda (x) (* x 2)) (list))', '()'),
        
        # Map with single element
        ('(map (lambda (x) x) (list 42))', '(42)'),
        
        # Map with string transformation
        ('(map (lambda (s) (string-upcase s)) (list "hello" "world"))', '("HELLO" "WORLD")'),
    ])
    def test_map_function(self, aifpl, expression, expected):
        """Test map higher-order function."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_map_requires_function_and_list(self, aifpl):
        """Test that map requires exactly 2 arguments: function and list."""
        with pytest.raises(AIFPLEvalError, match="requires exactly 2 arguments"):
            aifpl.evaluate('(map (lambda (x) x))')
        
        with pytest.raises(AIFPLEvalError, match="requires exactly 2 arguments"):
            aifpl.evaluate('(map (lambda (x) x) (list 1 2) (list 3 4))')
        
        # Second argument must be list
        with pytest.raises(AIFPLEvalError, match="requires list as second argument"):
            aifpl.evaluate('(map (lambda (x) x) 42)')

    @pytest.mark.parametrize("expression,expected", [
        # Basic filter operations
        ('(filter (lambda (x) (> x 0)) (list -1 2 -3 4))', '(2 4)'),
        ('(filter (lambda (x) (= x 0)) (list 1 0 2 0 3))', '(0 0)'),
        
        # Filter with empty list
        ('(filter (lambda (x) #t) (list))', '()'),
        
        # Filter that matches nothing
        ('(filter (lambda (x) #f) (list 1 2 3))', '()'),
        
        # Filter that matches everything
        ('(filter (lambda (x) #t) (list 1 2 3))', '(1 2 3)'),
        
        # Filter with string predicate
        ('(filter (lambda (s) (string-contains? s "e")) (list "hello" "world" "test"))', '("hello" "test")'),
    ])
    def test_filter_function(self, aifpl, expression, expected):
        """Test filter higher-order function."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_filter_requires_function_and_list(self, aifpl):
        """Test that filter requires exactly 2 arguments: function and list."""
        with pytest.raises(AIFPLEvalError, match="requires exactly 2 arguments"):
            aifpl.evaluate('(filter (lambda (x) #t))')
        
        # Second argument must be list
        with pytest.raises(AIFPLEvalError, match="requires list as second argument"):
            aifpl.evaluate('(filter (lambda (x) #t) 42)')

    def test_filter_predicate_must_return_boolean(self, aifpl):
        """Test that filter predicate must return boolean."""
        with pytest.raises(AIFPLEvalError, match="must return boolean"):
            aifpl.evaluate('(filter (lambda (x) x) (list 1 2 3))')
        
        with pytest.raises(AIFPLEvalError, match="must return boolean"):
            aifpl.evaluate('(filter (lambda (x) "hello") (list 1 2 3))')

    @pytest.mark.parametrize("expression,expected", [
        # Basic fold operations (left fold)
        ('(fold + 0 (list 1 2 3 4))', '10'),  # Sum
        ('(fold * 1 (list 1 2 3 4))', '24'),  # Product
        ('(fold - 0 (list 1 2 3))', '-6'),  # ((0-1)-2)-3 = -6
        
        # Fold with empty list returns initial value
        ('(fold + 0 (list))', '0'),
        ('(fold * 1 (list))', '1'),
        
        # Fold with single element
        ('(fold + 10 (list 5))', '15'),
        
        # Fold for list construction (reverse)
        ('(fold (lambda (acc x) (cons x acc)) (list) (list 1 2 3))', '(3 2 1)'),
    ])
    def test_fold_function(self, aifpl, expression, expected):
        """Test fold higher-order function."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_fold_requires_three_arguments(self, aifpl):
        """Test that fold requires exactly 3 arguments: function, initial, list."""
        with pytest.raises(AIFPLEvalError, match="requires exactly 3 arguments"):
            aifpl.evaluate('(fold + 0)')
        
        with pytest.raises(AIFPLEvalError, match="requires exactly 3 arguments"):
            aifpl.evaluate('(fold + 0 (list 1 2) extra)')
        
        # Third argument must be list
        with pytest.raises(AIFPLEvalError, match="requires list as third argument"):
            aifpl.evaluate('(fold + 0 42)')

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
    def test_range_function(self, aifpl, expression, expected):
        """Test range function for generating sequences."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_range_requires_numeric_arguments(self, aifpl):
        """Test that range requires numeric arguments."""
        with pytest.raises(AIFPLEvalError, match="must be numeric"):
            aifpl.evaluate('(range "hello" 5)')
        
        with pytest.raises(AIFPLEvalError, match="must be numeric"):
            aifpl.evaluate('(range 1 "world")')
        
        with pytest.raises(AIFPLEvalError, match="must be numeric"):
            aifpl.evaluate('(range 1 5 "step")')

    def test_range_zero_step_error(self, aifpl):
        """Test that range with zero step raises error."""
        with pytest.raises(AIFPLEvalError, match="step cannot be zero"):
            aifpl.evaluate('(range 1 5 0)')

    def test_range_argument_count(self, aifpl):
        """Test that range accepts 2 or 3 arguments."""
        with pytest.raises(AIFPLEvalError, match="requires 2 or 3 arguments"):
            aifpl.evaluate('(range 1)')
        
        with pytest.raises(AIFPLEvalError, match="requires 2 or 3 arguments"):
            aifpl.evaluate('(range 1 5 2 extra)')

    @pytest.mark.parametrize("expression,expected", [
        # Basic find operations
        ('(find (lambda (x) (> x 5)) (list 1 3 7 2))', '7'),
        ('(find (lambda (x) (= x 0)) (list 1 2 0 3))', '0'),
        
        # Find with no match returns #f
        ('(find (lambda (x) (> x 10)) (list 1 2 3))', '#f'),
        
        # Find in empty list returns #f
        ('(find (lambda (x) #t) (list))', '#f'),
        
        # Find first match (short-circuit)
        ('(find (lambda (x) (> x 2)) (list 1 3 5 7))', '3'),
        
        # Find with string predicate
        ('(find (lambda (s) (string-contains? s "o")) (list "hello" "world" "test"))', '"hello"'),
    ])
    def test_find_function(self, aifpl, expression, expected):
        """Test find higher-order function."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_find_requires_function_and_list(self, aifpl):
        """Test that find requires exactly 2 arguments."""
        with pytest.raises(AIFPLEvalError, match="requires exactly 2 arguments"):
            aifpl.evaluate('(find (lambda (x) #t))')
        
        # Second argument must be list
        with pytest.raises(AIFPLEvalError, match="requires list as second argument"):
            aifpl.evaluate('(find (lambda (x) #t) 42)')

    def test_find_predicate_must_return_boolean(self, aifpl):
        """Test that find predicate must return boolean."""
        with pytest.raises(AIFPLEvalError, match="must return boolean"):
            aifpl.evaluate('(find (lambda (x) x) (list 1 2 3))')

    @pytest.mark.parametrize("expression,expected", [
        # Basic any? operations
        ('(any? (lambda (x) (> x 5)) (list 1 3 7))', '#t'),
        ('(any? (lambda (x) (> x 10)) (list 1 3 7))', '#f'),
        
        # any? with empty list returns #f
        ('(any? (lambda (x) #t) (list))', '#f'),
        
        # any? short-circuits on first true
        ('(any? (lambda (x) (= x 2)) (list 1 2 3))', '#t'),
    ])
    def test_any_predicate_function(self, aifpl, expression, expected):
        """Test any? higher-order predicate function."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic all? operations
        ('(all? (lambda (x) (> x 0)) (list 1 3 7))', '#t'),
        ('(all? (lambda (x) (> x 5)) (list 1 3 7))', '#f'),
        
        # all? with empty list returns #t (vacuous truth)
        ('(all? (lambda (x) #f) (list))', '#t'),
        
        # all? short-circuits on first false
        ('(all? (lambda (x) (< x 5)) (list 1 2 6 3))', '#f'),
    ])
    def test_all_predicate_function(self, aifpl, expression, expected):
        """Test all? higher-order predicate function."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_any_all_require_function_and_list(self, aifpl):
        """Test that any? and all? require function and list arguments."""
        with pytest.raises(AIFPLEvalError, match="requires exactly 2 arguments"):
            aifpl.evaluate('(any? (lambda (x) #t))')
        
        with pytest.raises(AIFPLEvalError, match="requires exactly 2 arguments"):
            aifpl.evaluate('(all? (lambda (x) #t))')
        
        # Second argument must be list
        with pytest.raises(AIFPLEvalError, match="requires list as second argument"):
            aifpl.evaluate('(any? (lambda (x) #t) 42)')
        
        with pytest.raises(AIFPLEvalError, match="requires list as second argument"):
            aifpl.evaluate('(all? (lambda (x) #t) "hello")')

    def test_any_all_predicates_must_return_boolean(self, aifpl):
        """Test that any? and all? predicates must return boolean."""
        with pytest.raises(AIFPLEvalError, match="must return boolean"):
            aifpl.evaluate('(any? (lambda (x) x) (list 1 2 3))')
        
        with pytest.raises(AIFPLEvalError, match="must return boolean"):
            aifpl.evaluate('(all? (lambda (x) "hello") (list 1 2 3))')

    def test_complex_functional_compositions(self, aifpl, helpers):
        """Test complex combinations of functional operations."""
        # Map followed by filter
        pipeline1 = '''
        (filter (lambda (x) (> x 5))
                (map (lambda (x) (* x 2)) (list 1 2 3 4 5)))
        '''
        helpers.assert_evaluates_to(aifpl, pipeline1, '(6 8 10)')
        
        # Filter followed by fold
        pipeline2 = '''
        (fold + 0
              (filter (lambda (x) (> x 0)) (list -1 2 -3 4 5)))
        '''
        helpers.assert_evaluates_to(aifpl, pipeline2, '11')  # 2 + 4 + 5
        
        # Map, filter, and fold together
        pipeline3 = '''
        (fold *
              1
              (filter (lambda (x) (> x 1))
                      (map (lambda (x) (* x x))
                           (list 1 2 3 4))))
        '''
        helpers.assert_evaluates_to(aifpl, pipeline3, '576')  # 4 * 9 * 16 = 576

    def test_recursive_lambda_functions(self, aifpl, helpers):
        """Test recursive lambda functions with tail call optimization."""
        # Factorial using tail recursion
        factorial_expr = '''
        (let ((factorial (lambda (n acc)
                          (if (<= n 1) acc (factorial (- n 1) (* n acc))))))
          (factorial 5 1))
        '''
        helpers.assert_evaluates_to(aifpl, factorial_expr, '120')
        
        # Sum of list using tail recursion
        sum_list_expr = '''
        (let ((sum-list (lambda (lst acc)
                         (if (null? lst) 
                             acc 
                             (sum-list (rest lst) (+ acc (first lst)))))))
          (sum-list (list 1 2 3 4) 0))
        '''
        helpers.assert_evaluates_to(aifpl, sum_list_expr, '10')

    def test_deep_recursion_tail_optimization(self, aifpl_custom):
        """Test that tail recursion optimization prevents stack overflow."""
        # Create AIFPL with high recursion limit for this test
        aifpl = aifpl_custom(max_depth=1000)
        
        # Deep tail recursion should not cause stack overflow
        deep_recursion = '''
        (let ((count-down (lambda (n)
                           (if (<= n 0) "done" (count-down (- n 1))))))
          (count-down 500))
        '''
        result = aifpl.evaluate_and_format(deep_recursion)
        assert result == '"done"'

    def test_mutual_recursion(self, aifpl, helpers):
        """Test mutual recursion between lambda functions."""
        # Even/odd mutual recursion
        even_odd_expr = '''
        (let ((is-even (lambda (n) (if (= n 0) #t (is-odd (- n 1)))))
              (is-odd (lambda (n) (if (= n 0) #f (is-even (- n 1))))))
          (list (is-even 4) (is-odd 4) (is-even 7) (is-odd 7)))
        '''
        helpers.assert_evaluates_to(aifpl, even_odd_expr, '(#t #f #f #t)')

    def test_higher_order_function_composition(self, aifpl, helpers):
        """Test functions that take and return functions."""
        # Function composition
        compose_expr = '''
        (let ((compose (lambda (f g) (lambda (x) (f (g x)))))
              (double (lambda (x) (* x 2)))
              (square (lambda (x) (* x x)))
              (double-then-square (compose square double)))
          (double-then-square 3))
        '''
        helpers.assert_evaluates_to(aifpl, compose_expr, '36')  # (3*2)² = 36
        
        # Curried functions
        curry_expr = '''
        (let ((add (lambda (x) (lambda (y) (+ x y))))
              (add5 (add 5)))
          (add5 10))
        '''
        helpers.assert_evaluates_to(aifpl, curry_expr, '15')

    def test_lambda_with_complex_closures(self, aifpl, helpers):
        """Test lambda expressions with complex closure environments."""
        # Multiple nested closures
        nested_closure = '''
        (let ((x 10))
          (let ((make-adder (lambda (y) 
                             (lambda (z) (+ x y z)))))
            (let ((add-x-5 (make-adder 5)))
              (add-x-5 7))))
        '''
        helpers.assert_evaluates_to(aifpl, nested_closure, '22')  # 10 + 5 + 7
        
        # Closure capturing mutable-like behavior through let
        counter_like = '''
        (let ((base 100))
          (let ((increment (lambda (x) (+ base x)))
                (decrement (lambda (x) (- base x))))
            (list (increment 5) (decrement 3))))
        '''
        helpers.assert_evaluates_to(aifpl, counter_like, '(105 97)')

    def test_functional_data_processing_patterns(self, aifpl, helpers):
        """Test common functional programming patterns for data processing."""
        # Process list of numbers: square evens, filter > 10, sum
        data_processing = '''
        (fold +
              0
              (filter (lambda (x) (> x 10))
                      (map (lambda (x) (if (= (% x 2) 0) (* x x) x))
                           (list 1 2 3 4 5 6))))
        '''
        # 1->1, 2->4, 3->3, 4->16, 5->5, 6->36
        # Filter >10: 16, 36
        # Sum: 52
        helpers.assert_evaluates_to(aifpl, data_processing, '52')
        
        # String processing pipeline
        string_processing = '''
        (fold (lambda (acc s) (+ acc (string-length s)))
              0
              (filter (lambda (s) (string-contains? s "e"))
                      (map (lambda (s) (string-upcase s))
                           (list "hello" "world" "test" "code"))))
        '''
        # Map to uppercase: "HELLO", "WORLD", "TEST", "CODE"
        # Filter containing "e": "HELLO", "TEST", "CODE"
        # Sum lengths: 5 + 4 + 4 = 13
        helpers.assert_evaluates_to(aifpl, string_processing, '13')

    def test_range_with_functional_operations(self, aifpl, helpers):
        """Test range generation combined with functional operations."""
        # Sum of squares from 1 to 5
        sum_of_squares = '''
        (fold + 0 (map (lambda (x) (* x x)) (range 1 6)))
        '''
        helpers.assert_evaluates_to(aifpl, sum_of_squares, '55')  # 1+4+9+16+25
        
        # Filter even numbers from range and double them
        even_doubled = '''
        (map (lambda (x) (* x 2))
             (filter (lambda (x) (= (% x 2) 0))
                     (range 1 11)))
        '''
        helpers.assert_evaluates_to(aifpl, even_doubled, '(4 8 12 16 20)')

    def test_lambda_error_handling(self, aifpl):
        """Test error handling in lambda expressions."""
        # Undefined variable in lambda body
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate('((lambda (x) (+ x undefined-var)) 5)')
        
        # Type error in lambda body
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('((lambda (x) (+ x "hello")) 5)')
        
        # Division by zero in lambda body
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('((lambda (x) (/ x 0)) 5)')

    def test_let_error_handling(self, aifpl):
        """Test error handling in let expressions."""
        # Error in binding expression
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(let ((x (/ 1 0))) x)')
        
        # Undefined variable in binding
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate('(let ((x undefined-var)) x)')
        
        # Error in let body
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(let ((x 5)) (/ x 0))')

    def test_first_class_functions(self, aifpl, helpers):
        """Test that functions are first-class values."""
        # Store functions in lists
        function_list = '''
        (let ((funcs (list (lambda (x) (* x 2))
                           (lambda (x) (+ x 10))
                           (lambda (x) (* x x)))))
          (list ((first funcs) 5)
                ((list-ref funcs 1) 5)
                ((list-ref funcs 2) 5)))
        '''
        helpers.assert_evaluates_to(aifpl, function_list, '(10 15 25)')
        
        # Pass functions as arguments
        apply_twice = '''
        (let ((apply-twice (lambda (f x) (f (f x))))
              (increment (lambda (x) (+ x 1))))
          (apply-twice increment 5))
        '''
        helpers.assert_evaluates_to(aifpl, apply_twice, '7')
        
        # Return functions from functions
        make_multiplier = '''
        (let ((make-multiplier (lambda (n) (lambda (x) (* x n))))
              (times3 (make-multiplier 3)))
          (times3 7))
        '''
        helpers.assert_evaluates_to(aifpl, make_multiplier, '21')

    def test_complex_let_lambda_interaction(self, aifpl, helpers):
        """Test complex interactions between let and lambda."""
        # Lambda using multiple let-bound variables
        complex_interaction = '''
        (let ((a 5) (b 10) (c 2))
          (let ((compute (lambda (x) (+ (* a x) (* b (/ x c)))))
                (value 4))
            (compute value)))
        '''
        helpers.assert_evaluates_to(aifpl, complex_interaction, '40')  # 5*4 + 10*(4/2) = 20 + 20
        
        # Nested let with lambda closures
        nested_complex = '''
        (let ((outer 100))
          (let ((make-func (lambda (middle)
                            (lambda (inner) (+ outer middle inner)))))
            (let ((my-func (make-func 10)))
              (my-func 5))))
        '''
        helpers.assert_evaluates_to(aifpl, nested_complex, '115')  # 100 + 10 + 5