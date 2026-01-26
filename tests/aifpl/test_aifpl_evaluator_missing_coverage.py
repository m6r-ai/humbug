"""Tests for previously missing coverage areas in AIFPL evaluator."""

import pytest
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import AIFPLString


class TestEvaluatorMissingCoverage:
    """Test cases for previously uncovered edge cases in the evaluator."""

    # ========== Deep Recursion/Stack Overflow Tests ==========

    def test_deep_recursion_evaluate_method(self, aifpl_custom):
        """Test max depth exceeded in main evaluate() method."""
        aifpl_shallow = aifpl_custom(max_depth=5)
        # Create deeply nested expression that exceeds max_depth
        deep_expr = "1"
        for i in range(10):  # Exceed max_depth of 5
            deep_expr = f"(+ {deep_expr} {i})"

        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl_shallow.evaluate(deep_expr)
        assert "too deeply nested" in str(exc_info.value)
        assert "max depth: 5" in str(exc_info.value)

    def test_deep_recursion_tail_call_detection(self, aifpl_custom):
        """Test max depth exceeded in tail call detection."""
        aifpl_shallow = aifpl_custom(max_depth=3)
        # Create recursive function that triggers tail call detection depth limit
        recursive_code = """
        (let ((deep-func (lambda (n)
                           (if (= n 0)
                               0
                               (deep-func (- n 1))))))
          (deep-func 10))
        """
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl_shallow.evaluate(recursive_code)
        assert "too deeply nested" in str(exc_info.value)

    # ========== Quote Form Validation Tests ==========

    def test_quote_no_arguments(self, aifpl):
        """Test quote expression with no arguments."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(quote)")
        assert "wrong number of arguments" in str(exc_info.value)
        assert "Got 0 arguments" in str(exc_info.value)

    def test_quote_multiple_arguments(self, aifpl):
        """Test quote expression with multiple arguments."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(quote a b c)")
        assert "wrong number of arguments" in str(exc_info.value)
        assert "Got 3 arguments" in str(exc_info.value)

    # ========== Lambda Form Edge Cases ==========

    def test_lambda_invalid_single_parameter_number(self, aifpl):
        """Test lambda with invalid single parameter (number)."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(lambda 123 (+ 1 2))")
        assert "must be a list" in str(exc_info.value)

    def test_lambda_invalid_single_parameter_string(self, aifpl):
        """Test lambda with invalid single parameter (string)."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(lambda "param" (+ 1 2))')
        assert "must be a list" in str(exc_info.value)

    # ========== Let Form Validation Tests ==========

    def test_let_non_list_binding_structure(self, aifpl):
        """Test let with non-list binding structure."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(let 123 (+ 1 2))")
        assert "binding list must be a list" in str(exc_info.value)

    def test_let_non_list_individual_binding(self, aifpl):
        """Test let with non-list individual binding."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(let (x (y 2)) (+ x y))")
        assert "binding 1 must be a list" in str(exc_info.value)

    # ========== Range Function Edge Cases ==========

    def test_range_two_arg_invalid_start_type(self, aifpl):
        """Test 2-argument range with invalid start type."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(range "start" 10)')
        assert "must be a number" in str(exc_info.value)

    def test_range_two_arg_invalid_end_type(self, aifpl):
        """Test 2-argument range with invalid end type."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(range 1 "end")')
        assert "must be a number" in str(exc_info.value)

    def test_range_three_arg_invalid_start_type(self, aifpl):
        """Test 3-argument range with invalid start type."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(range "start" 10 2)')
        assert "must be a number" in str(exc_info.value)

    def test_range_three_arg_invalid_end_type(self, aifpl):
        """Test 3-argument range with invalid end type."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(range 1 "end" 2)')
        assert "must be a number" in str(exc_info.value)

    def test_range_three_arg_invalid_step_type(self, aifpl):
        """Test 3-argument range with invalid step type."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(range 1 10 "step")')
        assert "must be a number" in str(exc_info.value)

    # ========== Integer Validation Tests ==========

    def test_range_float_arguments(self, aifpl):
        """Test range with float arguments (should require integers)."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(range 1.5 5)")
        assert "integer" in str(exc_info.value).lower()

    def test_range_complex_arguments(self, aifpl):
        """Test range with complex number arguments."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("(range (complex 1 2) 5)")
        assert "integer" in str(exc_info.value).lower()

    # ========== Builtin Function Display Tests ==========

    def test_builtin_function_formatting(self, aifpl):
        """Test formatting of builtin function references."""
        # Get reference to builtin function without calling it
        result = aifpl.evaluate_and_format("+")
        assert result == "<builtin +>"

    def test_builtin_function_formatting_various(self, aifpl):
        """Test formatting of various builtin functions."""
        functions_to_test = ["*", "list"]
        for func_name in functions_to_test:
            result = aifpl.evaluate_and_format(func_name)
            assert result == f"<builtin {func_name}>"

    # ========== String Escaping Edge Cases ==========

    def test_string_formatting_backslash(self):
        """Test string formatting with backslash characters."""
        evaluator = AIFPLEvaluator()
        result = evaluator.format_result(AIFPLString("test\\path"))
        # Should escape backslashes: single \ becomes \\
        assert "\\\\" in result  # Should contain escaped backslash

    def test_string_formatting_carriage_return(self):
        """Test string formatting with carriage return characters."""
        evaluator = AIFPLEvaluator()
        result = evaluator.format_result(AIFPLString("line1\rline2"))
        assert "\\r" in result  # Should escape carriage return

    def test_string_formatting_control_characters(self):
        """Test string formatting with control characters."""
        evaluator = AIFPLEvaluator()
        # Test with ASCII control character (ASCII 1)
        result = evaluator.format_result(AIFPLString("test\x01end"))
        assert "\\u0001" in result  # Should escape as unicode

    def test_string_formatting_multiple_control_chars(self):
        """Test string formatting with various control characters."""
        evaluator = AIFPLEvaluator()

        # Test various control characters
        test_cases = [
            ("\x00", "\\u0000"),  # Null
            ("\x07", "\\u0007"),  # Bell
            ("\x1f", "\\u001f"),  # Unit separator
        ]

        for char, expected in test_cases:
            result = evaluator.format_result(AIFPLString(f"test{char}end"))
            assert expected in result

    # ========== Call Chain Management Tests ==========

    def test_recursive_lambda_call_chain_cleanup(self, aifpl):
        """Test that recursive lambda call chain is properly cleaned up."""
        recursive_code = """
        (let ((factorial (lambda (n)
                           (if (<= n 1)
                               1
                               (* n (factorial (- n 1)))))))
          (factorial 5))
        """
        result = aifpl.evaluate(recursive_code)
        assert result == 120

    def test_mutual_recursion_call_chain_cleanup(self, aifpl):
        """Test call chain cleanup with mutual recursion."""
        mutual_recursion_code = """
        (let ((is-even (lambda (n)
                         (if (= n 0)
                             #t
                             (is-odd (- n 1)))))
              (is-odd (lambda (n)
                        (if (= n 0)
                            #f
                            (is-even (- n 1))))))
          (is-even 4))
        """
        result = aifpl.evaluate(mutual_recursion_code)
        assert result is True

    # ========== Tail Call Detection Edge Cases ==========

    def test_tail_position_empty_list(self, aifpl):
        """Test empty list in tail position."""
        # Create lambda that returns empty list in tail position
        result = aifpl.evaluate("((lambda () ()))")
        # Should return empty list
        assert result == []

    def test_tail_position_quote(self, aifpl):
        """Test quote form in tail position."""
        # Create lambda that returns quoted value in tail position
        result = aifpl.evaluate_and_format("((lambda () (quote hello)))")
        # Should return the quoted symbol
        assert result == "hello"

    def test_tail_position_if_branches(self, aifpl):
        """Test if form branches in tail position."""
        # Test then branch in tail position
        result1 = aifpl.evaluate("((lambda (x) (if #t x 999)) 42)")
        assert result1 == 42

        # Test else branch in tail position
        result2 = aifpl.evaluate("((lambda (x) (if #f 999 x)) 42)")
        assert result2 == 42

    # ========== Error Context and Propagation Tests ==========

    def test_undefined_variable_in_tail_context(self, aifpl):
        """Test undefined variable error in tail call context."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("((lambda () undefined_var))")
        assert "Undefined variable" in str(exc_info.value)

    def test_symbol_lookup_error_context(self, aifpl):
        """Test that symbol lookup errors include context."""
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate("nonexistent_symbol")

        error_msg = str(exc_info.value)
        assert "Undefined variable" in error_msg
        assert "Available variables" in error_msg

    # ========== Environment Creation Edge Case ==========

    def test_global_environment_creation(self, aifpl):
        """Test global environment creation when none provided."""
        # This should create global environment with constants and builtins
        result = aifpl.evaluate("pi")
        assert abs(result - 3.14159) < 0.001

    def test_builtin_functions_in_global_env(self, aifpl):
        """Test that builtin functions are available in global environment."""
        # Test that builtin functions are accessible
        result = aifpl.evaluate("(+ 1 2)")
        assert result == 3

    # ========== Partial Coverage Branch Tests ==========

    def test_lambda_parameter_list_variations(self, aifpl):
        """Test various lambda parameter list formats."""
        # Empty parameter list
        result1 = aifpl.evaluate("((lambda () 42))")
        assert result1 == 42

        # Single parameter in list
        result2 = aifpl.evaluate("((lambda (x) x) 42)")
        assert result2 == 42

        # Multiple parameters
        result3 = aifpl.evaluate("((lambda (x y) (+ x y)) 1 2)")
        assert result3 == 3

    def test_list_type_checking_branches(self, aifpl):
        """Test list type checking in various contexts."""
        # Test with actual list
        result1 = aifpl.evaluate("(first (list 1 2 3))")
        assert result1 == 1

        # Test with empty list
        result2 = aifpl.evaluate("(length ())")
        assert result2 == 0

    def test_function_type_checking_branches(self, aifpl):
        """Test function type checking in various contexts."""
        # Test calling builtin function
        result1 = aifpl.evaluate("(+ 1 2)")
        assert result1 == 3

        # Test calling lambda function
        result2 = aifpl.evaluate("((lambda (x) (* x 2)) 5)")
        assert result2 == 10

    # ========== Complex Integration Tests ==========

    def test_deeply_nested_tail_calls(self, aifpl_custom):
        """Test deeply nested expressions that use tail call optimization."""
        aifpl_deep = aifpl_custom(max_depth=50)  # Allow deeper nesting

        # Create a tail-recursive countdown function
        countdown_code = """
        (let ((countdown (lambda (n acc)
                          (if (= n 0)
                              acc
                              (countdown (- n 1) (+ acc n))))))
          (countdown 10 0))
        """
        result = aifpl_deep.evaluate(countdown_code)
        # Sum of 1+2+3+...+10 = 55
        assert result == 55

    def test_error_propagation_through_tail_calls(self, aifpl):
        """Test error propagation through tail call optimization."""
        # Create recursive function that will eventually error
        error_code = """
        (let ((error-func (lambda (n)
                           (if (= n 0)
                               (/ 1 0)
                               (error-func (- n 1))))))
          (error-func 3))
        """
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate(error_code)
        assert "Division by zero" in str(exc_info.value)

    def test_special_form_detection_branches(self, aifpl):
        """Test special form detection in various contexts."""
        # Test that special forms work correctly
        result1 = aifpl.evaluate("(and #t #t)")
        assert result1 is True

        result2 = aifpl.evaluate("(or #f #t)")
        assert result2 is True

        result3 = aifpl.evaluate("(map (lambda (x) (* x 2)) (list 1 2 3))")
        expected = [2, 4, 6]
        assert result3 == expected
