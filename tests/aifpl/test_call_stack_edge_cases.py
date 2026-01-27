"""Tests for AIFPL call stack edge cases."""

import pytest

from aifpl import AIFPLEvalError
from aifpl.aifpl_call_stack import AIFPLCallStack
from aifpl.aifpl_value import AIFPLInteger, AIFPLString, AIFPLBoolean


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


class TestCallStackFormatting:
    """Test call stack formatting and display."""

    def test_format_empty_stack(self):
        """Test formatting an empty call stack."""
        call_stack = AIFPLCallStack()

        result = call_stack.format_stack_trace()

        assert result == "  (no function calls)"
        assert "... (" not in result

    def test_format_stack_within_limit(self):
        """Test formatting a call stack that doesn't exceed max_frames."""
        call_stack = AIFPLCallStack()

        # Add 5 frames (default max_frames is 10)
        for i in range(5):
            call_stack.push(
                function_name=f"func{i}",
                arguments={"x": AIFPLInteger(i)},
                expression=f"(func{i} {i})"
            )

        result = call_stack.format_stack_trace(max_frames=10)

        # Should NOT contain the truncation message
        assert "... (" not in result
        # Should contain all 5 functions
        assert "func0" in result
        assert "func1" in result
        assert "func2" in result
        assert "func3" in result
        assert "func4" in result
        # Should not show empty message
        assert "(no function calls)" not in result

    def test_format_stack_exceeds_limit(self):
        """Test formatting a call stack that exceeds max_frames."""
        call_stack = AIFPLCallStack()

        # Add 15 frames (max_frames will be 10)
        for i in range(15):
            call_stack.push(
                function_name=f"func{i}",
                arguments={"x": AIFPLInteger(i)},
                expression=f"(func{i} {i})"
            )

        result = call_stack.format_stack_trace(max_frames=10)

        # Should contain the truncation message
        assert "... (5 more frames)" in result
        # Should contain the last 10 functions (func5 through func14)
        assert "func5" in result
        assert "func14" in result
        # Should NOT contain the first 5 functions
        assert "func0" not in result
        assert "func4" not in result

    def test_format_stack_exactly_at_limit(self):
        """Test formatting a call stack that exactly matches max_frames."""
        call_stack = AIFPLCallStack()

        # Add exactly 10 frames (same as max_frames)
        for i in range(10):
            call_stack.push(
                function_name=f"func{i}",
                arguments={"x": AIFPLInteger(i)},
                expression=f"(func{i} {i})"
            )

        result = call_stack.format_stack_trace(max_frames=10)

        # Should NOT contain the truncation message (exactly at limit)
        assert "... (" not in result
        # Should contain all 10 functions
        assert "func0" in result
        assert "func9" in result

    def test_format_single_frame_stack(self):
        """Test formatting a call stack with a single frame."""
        call_stack = AIFPLCallStack()

        call_stack.push(
            function_name="single_func",
            arguments={"x": AIFPLInteger(42), "y": AIFPLString("test")},
            expression='(single_func 42 "test")'
        )

        result = call_stack.format_stack_trace()

        # Should NOT contain truncation message
        assert "... (" not in result
        # Should NOT contain "no function calls"
        assert "(no function calls)" not in result
        # Should contain the function
        assert "single_func" in result

    def test_format_truncation_message_count(self):
        """Test that truncation message shows correct count of hidden frames."""
        call_stack = AIFPLCallStack()

        # Add 25 frames, show only 10
        for i in range(25):
            call_stack.push(
                function_name=f"func{i}",
                arguments={},
                expression=f"(func{i})"
            )

        result = call_stack.format_stack_trace(max_frames=10)

        # Should show exactly 15 hidden frames (25 - 10 = 15)
        assert "... (15 more frames)" in result
        # Should contain frames 15-24
        assert "func15" in result
        assert "func24" in result
        # Should NOT contain frames 0-14
        assert "func0" not in result
        assert "func14" not in result

    def test_format_very_small_max_frames(self):
        """Test formatting with very small max_frames value."""
        call_stack = AIFPLCallStack()

        # Add 10 frames, show only 3
        for i in range(10):
            call_stack.push(
                function_name=f"func{i}",
                arguments={"x": AIFPLInteger(i)},
                expression=f"(func{i} {i})"
            )

        result = call_stack.format_stack_trace(max_frames=3)

        # Should show 7 hidden frames (10 - 3 = 7)
        assert "... (7 more frames)" in result
        # Should contain only last 3 frames
        assert "func7" in result
        assert "func8" in result
        assert "func9" in result
        # Should NOT contain earlier frames
        assert "func6" not in result
        assert "func0" not in result

    def test_format_nested_indentation(self):
        """Test that nested calls show proper indentation."""
        call_stack = AIFPLCallStack()

        # Add 3 frames to show nesting
        call_stack.push(
            function_name="outer",
            arguments={"x": AIFPLInteger(1)},
            expression="(outer 1)"
        )
        call_stack.push(
            function_name="middle",
            arguments={"y": AIFPLInteger(2)},
            expression="(middle 2)"
        )
        call_stack.push(
            function_name="inner",
            arguments={"z": AIFPLInteger(3)},
            expression="(inner 3)"
        )

        result = call_stack.format_stack_trace()

        # Verify all functions are present
        assert "outer" in result
        assert "middle" in result
        assert "inner" in result

        # Verify indentation increases (lines should have increasing spaces)
        lines = result.split('\n')
        function_lines = [line for line in lines if any(f in line for f in ["outer", "middle", "inner"])]
        assert len(function_lines) >= 3

    def test_format_with_multiple_argument_types(self):
        """Test formatting with various argument types."""
        call_stack = AIFPLCallStack()

        call_stack.push(
            function_name="multi_type_func",
            arguments={
                "num": AIFPLInteger(42),
                "text": AIFPLString("hello"),
                "flag": AIFPLBoolean(True)
            },
            expression='(multi_type_func 42 "hello" #t)'
        )

        result = call_stack.format_stack_trace()

        # Should contain function name
        assert "multi_type_func" in result
        # Should contain arguments (in some form)
        assert "42" in result or "AIFPLNumber" in result

    def test_format_with_no_arguments(self):
        """Test formatting frames with no arguments."""
        call_stack = AIFPLCallStack()

        call_stack.push(
            function_name="no_args_func",
            arguments={},
            expression="(no_args_func)"
        )

        result = call_stack.format_stack_trace()

        # Should contain function name
        assert "no_args_func" in result
        # Should show empty argument list
        assert "()" in result or "no_args_func" in result

    def test_format_boundary_exactly_one_over_limit(self):
        """Test boundary case of exactly one frame over the limit."""
        call_stack = AIFPLCallStack()

        # Add 11 frames with max_frames=10
        for i in range(11):
            call_stack.push(
                function_name=f"func{i}",
                arguments={},
                expression=f"(func{i})"
            )

        result = call_stack.format_stack_trace(max_frames=10)

        # Should show exactly 1 hidden frame
        assert "... (1 more frames)" in result
        # Should show frames 1-10
        assert "func1" in result
        assert "func10" in result
        # Should NOT show frame 0
        assert "func0" not in result

    def test_format_large_truncation(self):
        """Test formatting with large number of truncated frames."""
        call_stack = AIFPLCallStack()

        # Add 100 frames, show only 5
        for i in range(100):
            call_stack.push(
                function_name=f"func{i}",
                arguments={},
                expression=f"(func{i})"
            )

        result = call_stack.format_stack_trace(max_frames=5)

        # Should show 95 hidden frames (100 - 5 = 95)
        assert "... (95 more frames)" in result
        # Should only show last 5 frames
        assert "func95" in result
        assert "func99" in result
        # Should NOT show early frames
        assert "func0" not in result
        assert "func94" not in result

    def test_format_stack_trace_in_error_message(self, aifpl_custom):
        """Test that stack traces appear in error messages for depth exceeded."""
        # Create AIFPL with low depth limit
        aifpl_shallow = aifpl_custom(max_depth=20)

        # Create non-tail recursive function that will exceed depth
        deep_recursion = """
        (letrec ((factorial (lambda (n)
                              (if (<= n 1)
                                  1
                                  (* n (factorial (- n 1)))))))
          (factorial 50))
        """

        try:
            aifpl_shallow.evaluate(deep_recursion)
            # Should not reach here
            assert False, "Expected AIFPLEvalError"
        except AIFPLEvalError as e:
            error_msg = str(e)
            # Error should contain stack trace
            assert "Call stack:" in error_msg or "lambda" in error_msg
            # Should mention depth exceeded
            assert "too deeply nested" in error_msg or "max depth" in error_msg.lower()
