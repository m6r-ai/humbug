"""
Tests for previously missing coverage areas in Menai evaluator.

NOTE: Tests that rely on max_depth behavior are skipped since the bytecode VM
uses tail-call optimization and doesn't have the same depth limits as the old
tree-walking interpreter had.
"""

import pytest
from menai.menai_error import MenaiEvalError


class TestEvaluatorMissingCoverage:
    """Test cases for previously uncovered edge cases in the evaluator."""

    # ========== Quote Form Validation Tests ==========

    def test_quote_no_arguments(self, menai):
        """Test quote expression with no arguments."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(quote)")
        assert "wrong number of arguments" in str(exc_info.value)
        assert "Got 0 arguments" in str(exc_info.value)

    def test_quote_multiple_arguments(self, menai):
        """Test quote expression with multiple arguments."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(quote a b c)")
        assert "wrong number of arguments" in str(exc_info.value)
        assert "Got 3 arguments" in str(exc_info.value)

    # ========== Lambda Form Edge Cases ==========

    def test_lambda_invalid_single_parameter_number(self, menai):
        """Test lambda with invalid single parameter (number)."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(lambda 123 (integer+ 1 2))")
        assert "must be a list" in str(exc_info.value)

    def test_lambda_invalid_single_parameter_string(self, menai):
        """Test lambda with invalid single parameter (string)."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(lambda "param" (integer+ 1 2))')
        assert "must be a list" in str(exc_info.value)

    # ========== Let Form Validation Tests ==========

    def test_let_non_list_binding_structure(self, menai):
        """Test let with non-list binding structure."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(let 123 (integer+ 1 2))")
        assert "binding list must be a list" in str(exc_info.value)

    def test_let_non_list_individual_binding(self, menai):
        """Test let with non-list individual binding."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(let (x (y 2)) (integer+ x y))")
        assert "binding 1 must be a list" in str(exc_info.value)

    # ========== Range Function Edge Cases ==========

    def test_range_two_arg_invalid_start_type(self, menai):
        """Test 2-argument range with invalid start type."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(range "start" 10)')
        assert "requires integer argument" in str(exc_info.value)

    def test_range_two_arg_invalid_end_type(self, menai):
        """Test 2-argument range with invalid end type."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(range 1 "end")')
        assert "requires integer argument" in str(exc_info.value)

    def test_range_three_arg_invalid_start_type(self, menai):
        """Test 3-argument range with invalid start type."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(range "start" 10 2)')
        assert "requires integer argument" in str(exc_info.value)

    def test_range_three_arg_invalid_end_type(self, menai):
        """Test 3-argument range with invalid end type."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(range 1 "end" 2)')
        assert "requires integer argument" in str(exc_info.value)

    def test_range_three_arg_invalid_step_type(self, menai):
        """Test 3-argument range with invalid step type."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate('(range 1 10 "step")')
        assert "requires integer argument" in str(exc_info.value)

    # ========== Integer Validation Tests ==========

    def test_range_float_arguments(self, menai):
        """Test range with float arguments (should require integers)."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(range 1.5 5)")
        assert "integer" in str(exc_info.value).lower()

    def test_range_complex_arguments(self, menai):
        """Test range with complex number arguments."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("(range (integer->complex 1 2) 5)")
        assert "integer" in str(exc_info.value).lower()

    # ========== Builtin Function Display Tests ==========

    def test_builtin_function_formatting(self, menai):
        """Test formatting of builtin function references."""
        result = menai.evaluate_and_format("float-sqrt")
        assert result == "<lambda (arg0)>"

    def test_builtin_function_formatting_various(self, menai):
        """Test formatting of various builtin functions."""
        # Fixed-arity stubs describe as <lambda (arg0, ...)>
        cases = [
            ("float-sqrt", "<lambda (arg0)>"),  # unary fixed-arity — bytecode stub
            ("list", "<lambda (args)>"),        # variadic prelude lambda — rest param named 'args'
        ]
        for func_name, expected in cases:
            result = menai.evaluate_and_format(func_name)
            assert result == expected, f"Expected {expected!r}, got {result!r}"

    # ========== Call Chain Management Tests ==========

    def test_recursive_lambda_call_chain_cleanup(self, menai):
        """Test that recursive lambda call chain is properly cleaned up."""
        recursive_code = """
        (letrec ((factorial (lambda (n)
                              (if (integer<=? n 1)
                                  1
                                  (integer* n (factorial (integer- n 1)))))))
          (factorial 5))
        """
        result = menai.evaluate(recursive_code)
        assert result == 120

    def test_mutual_recursion_call_chain_cleanup(self, menai):
        """Test call chain cleanup with mutual recursion."""
        mutual_recursion_code = """
        (letrec ((is-even (lambda (n)
                            (if (integer=? n 0)
                                #t
                                (is-odd (integer- n 1)))))
                 (is-odd (lambda (n)
                           (if (integer=? n 0)
                               #f
                               (is-even (integer- n 1))))))
          (is-even 4))
        """
        result = menai.evaluate(mutual_recursion_code)
        assert result is True

    # ========== Tail Call Detection Edge Cases ==========

    def test_tail_position_empty_list(self, menai):
        """Test empty list in tail position."""
        # Create lambda that returns empty list in tail position
        result = menai.evaluate("((lambda () ()))")
        # Should return empty list
        assert result == []

    def test_tail_position_quote(self, menai):
        """Test quote form in tail position."""
        # Create lambda that returns quoted value in tail position
        result = menai.evaluate_and_format("((lambda () (quote hello)))")
        # Should return the quoted symbol
        assert result == "hello"

    def test_tail_position_if_branches(self, menai):
        """Test if form branches in tail position."""
        # Test then branch in tail position
        result1 = menai.evaluate("((lambda (x) (if #t x 999)) 42)")
        assert result1 == 42

        # Test else branch in tail position
        result2 = menai.evaluate("((lambda (x) (if #f 999 x)) 42)")
        assert result2 == 42

    # ========== Error Context and Propagation Tests ==========

    def test_undefined_variable_in_tail_context(self, menai):
        """Test undefined variable error in tail call context."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("((lambda () undefined_var))")
        assert "Undefined variable" in str(exc_info.value)

    def test_symbol_lookup_error_context(self, menai):
        """Test that symbol lookup errors include context."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate("nonexistent_symbol")

        error_msg = str(exc_info.value)
        assert "Undefined variable" in error_msg
        assert "Available variables" in error_msg

    # ========== Environment Creation Edge Case ==========

    def test_global_environment_creation(self, menai):
        """Test global environment creation when none provided."""
        # This should create global environment with constants and builtins
        result = menai.evaluate("pi")
        assert abs(result - 3.14159) < 0.001

    def test_builtin_functions_in_global_env(self, menai):
        """Test that builtin functions are available in global environment."""
        # Test that builtin functions are accessible
        result = menai.evaluate("(integer+ 1 2)")
        assert result == 3

    # ========== Partial Coverage Branch Tests ==========

    def test_lambda_parameter_list_variations(self, menai):
        """Test various lambda parameter list formats."""
        # Empty parameter list
        result1 = menai.evaluate("((lambda () 42))")
        assert result1 == 42

        # Single parameter in list
        result2 = menai.evaluate("((lambda (x) x) 42)")
        assert result2 == 42

        # Multiple parameters
        result3 = menai.evaluate("((lambda (x y) (integer+ x y)) 1 2)")
        assert result3 == 3

    def test_list_type_checking_branches(self, menai):
        """Test list type checking in various contexts."""
        # Test with actual list
        result1 = menai.evaluate("(list-first (list 1 2 3))")
        assert result1 == 1

        # Test with empty list
        result2 = menai.evaluate("(list-length ())")
        assert result2 == 0

    def test_function_type_checking_branches(self, menai):
        """Test function type checking in various contexts."""
        # Test calling builtin function
        result1 = menai.evaluate("(integer+ 1 2)")
        assert result1 == 3

        # Test calling lambda function
        result2 = menai.evaluate("((lambda (x) (integer* x 2)) 5)")
        assert result2 == 10

    # ========== Complex Integration Tests ==========

    def test_deeply_nested_tail_calls(self, menai):
        """Test deeply nested expressions that use tail call optimization."""
        # Create a tail-recursive countdown function
        countdown_code = """
        (letrec ((countdown (lambda (n acc)
                             (if (integer=? n 0)
                                 acc
                                 (countdown (integer- n 1) (integer+ acc n))))))
          (countdown 10 0))
        """
        result = menai.evaluate(countdown_code)
        # Sum of 1+2+3+...+10 = 55
        assert result == 55

    def test_error_propagation_through_tail_calls(self, menai):
        """Test error propagation through tail call optimization."""
        # Create recursive function that will eventually error
        error_code = """
        (letrec ((error-func (lambda (n)
                              (if (integer=? n 0)
                                  (integer/ 1 0)
                                  (error-func (integer- n 1))))))
          (error-func 3))
        """
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate(error_code)
        assert "Division by zero" in str(exc_info.value)

    def test_special_form_detection_branches(self, menai):
        """Test special form detection in various contexts."""
        # Test that special forms work correctly
        result1 = menai.evaluate("(and #t #t)")
        assert result1 is True

        result2 = menai.evaluate("(or #f #t)")
        assert result2 is True

        result3 = menai.evaluate("(map-list (lambda (x) (integer* x 2)) (list 1 2 3))")
        expected = [2, 4, 6]
        assert result3 == expected
