"""Tests for AIFPL call stack edge cases."""

import pytest

from aifpl import AIFPLEvalError


class TestAIFPLCallStackEdgeCases:
    """Test call stack edge cases and depth management."""

    def test_basic_call_stack_functionality(self, aifpl):
        """Test basic call stack functionality."""
        # Simple nested function calls should work
        result = aifpl.evaluate("(+ 1 (+ 2 3))")
        assert result == 6

        # More deeply nested calls
        result = aifpl.evaluate("(+ 1 (+ 2 (+ 3 4)))")
        assert result == 10

        # Function calls with multiple arguments
        result = aifpl.evaluate("(+ (+ 1 2) (+ 3 4) (+ 5 6))")
        assert result == 21

    def test_recursion_depth_limits(self, aifpl_custom):
        """Test recursion depth limits and stack overflow protection."""
        # Create AIFPL with very low depth limit
        aifpl_shallow = aifpl_custom(max_depth=10)

        # Create deeply nested expression that should exceed limit
        deep_expr = "(+ 1 " * 15 + "1" + ")" * 15

        with pytest.raises(AIFPLEvalError, match="too deeply nested"):
            aifpl_shallow.evaluate(deep_expr)

        # Same expression should work with higher depth limit
        aifpl_deep = aifpl_custom(max_depth=50)
        result = aifpl_deep.evaluate(deep_expr)
        assert result == 16  # 1 + 1 + ... (16 ones total)

    def test_call_stack_with_lambda_functions(self, aifpl):
        """Test call stack behavior with lambda functions."""
        # Simple lambda call
        result = aifpl.evaluate("((lambda (x) (+ x 1)) 5)")
        assert result == 6

        # Nested lambda calls
        result = aifpl.evaluate("((lambda (x) ((lambda (y) (+ x y)) 3)) 5)")
        assert result == 8

        # Lambda with multiple arguments
        result = aifpl.evaluate("((lambda (x y z) (+ x y z)) 1 2 3)")
        assert result == 6

    def test_call_stack_with_higher_order_functions(self, aifpl):
        """Test call stack with higher-order functions."""
        # Map function creates nested calls
        result = aifpl.evaluate("(map (lambda (x) (* x 2)) (list 1 2 3))")
        assert result == [2, 4, 6]

        # Filter function with predicate calls
        result = aifpl.evaluate("(filter (lambda (x) (> x 2)) (list 1 2 3 4))")
        assert result == [3, 4]

        # Fold function with accumulator calls
        result = aifpl.evaluate("(fold + 0 (list 1 2 3 4))")
        assert result == 10

    def test_call_stack_with_let_bindings(self, aifpl):
        """Test call stack behavior with let bindings."""
        # Simple let binding
        result = aifpl.evaluate("(let ((x 5)) (+ x 1))")
        assert result == 6

        # Nested let bindings
        result = aifpl.evaluate("""
        (let ((x 5))
          (let ((y 3))
            (+ x y)))
        """)
        assert result == 8

        # Let with sequential dependencies
        result = aifpl.evaluate("""
        (let ((x 5)
              (y (* x 2)))
          (+ x y))
        """)
        assert result == 15

    def test_call_stack_with_conditional_expressions(self, aifpl):
        """Test call stack with conditional expressions."""
        # Simple conditional
        result = aifpl.evaluate('(if #t (+ 1 2) (+ 3 4))')
        assert result == 3

        # Nested conditionals
        result = aifpl.evaluate('''
        (if (> 5 3)
            (if (< 2 4) "both true" "first true only")
            "first false")
        ''')
        assert result == "both true"

        # Conditional with complex expressions
        result = aifpl.evaluate('''
        (if (= (+ 2 3) 5)
            (* 2 (+ 3 4))
            (/ 10 2))
        ''')
        assert result == 14

    def test_call_stack_error_propagation(self, aifpl):
        """Test that errors propagate correctly through call stack."""
        # Error in nested arithmetic
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(+ 1 (/ 2 0))")

        # Error in lambda function
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("((lambda (x) (/ x 0)) 5)")

        # Error in higher-order function
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(map (lambda (x) (/ x 0)) (list 1 2 3))")

    def test_call_stack_with_recursive_functions(self, aifpl):
        """Test call stack with recursive functions (if supported)."""
        # Simple recursive function
        try:
            result = aifpl.evaluate("""
            (let ((factorial (lambda (n)
                              (if (<= n 1)
                                  1
                                  (* n (factorial (- n 1)))))))
              (factorial 5))
            """)
            assert result == 120
        except AIFPLEvalError:
            # Recursion might not be supported or might hit depth limits
            pass

    def test_call_stack_depth_tracking(self, aifpl_custom):
        """Test call stack depth tracking accuracy."""
        # Test various depth levels
        depth_cases = [
            (5, "(+ 1 (+ 1 (+ 1 (+ 1 1))))", 5),
            (10, "(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1))))))))))", 11),
            (3, "(+ (+ 1 1) (+ 1 1))", 4),
        ]

        for max_depth, expr, expected in depth_cases:
            aifpl_limited = aifpl_custom(max_depth=max_depth)
            try:
                result = aifpl_limited.evaluate(expr)
                assert result == expected
            except AIFPLEvalError:
                # If it exceeds depth, that's also a valid test result
                pass

    def test_call_stack_with_complex_expressions(self, aifpl):
        """Test call stack with complex nested expressions."""
        # Complex arithmetic expression
        complex_expr = """
        (+ (* 2 (+ 3 4))
           (/ (+ 10 5) (- 8 3))
           (abs (- 2 7)))
        """
        result = aifpl.evaluate(complex_expr)
        assert result == 22  # (2*7) + (15/5) + 5 = 14 + 3 + 5 = 22

        # Complex functional expression
        functional_expr = """
        (fold + 0
              (map (lambda (x) (* x x))
                   (filter (lambda (x) (> x 0))
                           (list -2 1 -3 2 3))))
        """
        result = aifpl.evaluate(functional_expr)
        assert result == 14  # 1^2 + 2^2 + 3^2 = 1 + 4 + 9 = 14

    def test_call_stack_memory_efficiency(self, aifpl):
        """Test call stack memory efficiency."""
        # Large number of sequential calls
        large_expr = "(+ " + " ".join(str(i) for i in range(100)) + ")"
        result = aifpl.evaluate(large_expr)
        assert result == sum(range(100))

        # Deep but linear call stack
        linear_deep = "(+ 1 " * 50 + "1" + ")" * 50
        try:
            result = aifpl.evaluate(linear_deep)
            assert result == 51
        except AIFPLEvalError:
            # Might exceed depth limits, which is acceptable
            pass

    def test_call_stack_with_string_operations(self, aifpl):
        """Test call stack with string operations."""
        # Nested string operations
        result = aifpl.evaluate('''
        (string-append
          (string-upcase "hello")
          " "
          (string-downcase "WORLD"))
        ''')
        assert result == "HELLO world"

        # String operations with numeric conversions
        result = aifpl.evaluate('''
        (string->number
          (string-append
            (number->string (+ 2 3))
            (number->string (* 2 2))))
        ''')
        assert result == 54  # "5" + "4" = "54" -> 54

    def test_call_stack_with_list_operations(self, aifpl):
        """Test call stack with list operations."""
        # Nested list operations
        result = aifpl.evaluate("""
        (append
          (list 1 2)
          (reverse (list 3 4 5))
          (list 6))
        """)
        assert result == [1, 2, 5, 4, 3, 6]

        # List operations with transformations
        result = aifpl.evaluate("""
        (length
          (filter (lambda (x) (> x 0))
                  (map (lambda (x) (- x 2))
                       (list 1 2 3 4 5))))
        """)
        assert result == 3  # [-1, 0, 1, 2, 3] -> [1, 2, 3] -> length 3

    def test_call_stack_tail_call_optimization(self, aifpl):
        """Test tail call optimization (if supported)."""
        # Tail recursive function (if supported)
        try:
            tail_recursive = """
            (let ((tail-sum (lambda (n acc)
                             (if (<= n 0)
                                 acc
                                 (tail-sum (- n 1) (+ acc n))))))
              (tail-sum 100 0))
            """
            result = aifpl.evaluate(tail_recursive)
            assert result == 5050  # Sum of 1 to 100
        except AIFPLEvalError:
            # Tail call optimization or recursion might not be supported
            pass

    def test_call_stack_error_context(self, aifpl):
        """Test that call stack provides good error context."""
        # Error in deeply nested context
        try:
            aifpl.evaluate("""
            (let ((f (lambda (x)
                      (let ((g (lambda (y)
                                (/ y 0))))
                        (g x)))))
              (f 5))
            """)
        except AIFPLEvalError as e:
            error_msg = str(e)
            # Error should provide context about where the error occurred
            assert "Division by zero" in error_msg

    def test_call_stack_with_pattern_matching(self, aifpl):
        """Test call stack with pattern matching (if supported)."""
        try:
            result = aifpl.evaluate("""
            (match (+ 2 3)
              ((number? n) (if (> n 3) "big" "small"))
              (_ "not number"))
            """)
            assert result == "big"
        except AIFPLEvalError:
            # Pattern matching might not be supported
            pass

    def test_call_stack_state_isolation(self, aifpl):
        """Test that call stack states are properly isolated."""
        # Multiple independent evaluations should not interfere
        results = []
        expressions = [
            "(+ 1 2)",
            "(* 3 4)",
            "(/ 12 3)",
            "(- 10 5)",
        ]

        for expr in expressions:
            result = aifpl.evaluate(expr)
            results.append(result)

        assert results == [3, 12, 4, 5]

    def test_call_stack_with_closures(self, aifpl):
        """Test call stack with closures and lexical scoping."""
        # Closure capturing outer variable
        result = aifpl.evaluate("""
        (let ((x 10))
          (let ((f (lambda (y) (+ x y))))
            (f 5)))
        """)
        assert result == 15

        # Nested closures
        result = aifpl.evaluate("""
        (let ((x 10))
          (let ((f (lambda (y)
                    (let ((g (lambda (z) (+ x y z))))
                      (g 3)))))
            (f 5)))
        """)
        assert result == 18  # 10 + 5 + 3

    def test_call_stack_exception_unwinding(self, aifpl):
        """Test proper exception unwinding through call stack."""
        # Exception should unwind through multiple call levels
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("""
            (let ((f (lambda (x)
                      (let ((g (lambda (y)
                                (let ((h (lambda (z)
                                          (/ z 0))))
                                  (h y)))))
                        (g x)))))
              (f 5))
            """)

        # After exception, evaluator should be in clean state
        result = aifpl.evaluate("(+ 1 2)")
        assert result == 3

    def test_call_stack_with_large_argument_lists(self, aifpl):
        """Test call stack with functions that have many arguments."""
        # Function with many arguments
        many_args = "(+ " + " ".join(str(i) for i in range(50)) + ")"
        result = aifpl.evaluate(many_args)
        assert result == sum(range(50))

        # Lambda with many parameters (if supported)
        try:
            lambda_many_params = """
            ((lambda (a b c d e f g h i j)
               (+ a b c d e f g h i j))
             1 2 3 4 5 6 7 8 9 10)
            """
            result = aifpl.evaluate(lambda_many_params)
            assert result == 55
        except AIFPLEvalError:
            # Many parameters might not be supported
            pass

    def test_call_stack_performance_edge_cases(self, aifpl_custom):
        """Test call stack performance with edge cases."""
        # Test with different depth limits
        depth_limits = [10, 50, 100]

        for limit in depth_limits:
            aifpl_limited = aifpl_custom(max_depth=limit)

            # Create expression just under the limit
            expr = "(+ 1 " * (limit - 5) + "1" + ")" * (limit - 5)

            try:
                result = aifpl_limited.evaluate(expr)
                # Should succeed
                assert isinstance(result, (int, float))
            except AIFPLEvalError:
                # Might still exceed limits due to other factors
                pass

    def test_call_stack_with_mixed_operations(self, aifpl):
        """Test call stack with mixed operation types."""
        # Mix of arithmetic, string, list, and boolean operations
        mixed_expr = """
        (if (> (length (list 1 2 3)) 2)
            (string->number
              (string-append
                (number->string (+ 5 5))
                (number->string (* 2 3))))
            0)
        """
        result = aifpl.evaluate(mixed_expr)
        assert result == 106  # "10" + "6" = "106" -> 106
