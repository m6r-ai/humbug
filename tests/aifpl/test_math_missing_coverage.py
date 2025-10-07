"""Tests to address missing coverage in aifpl_math.py."""

import pytest
import cmath

from aifpl import AIFPLEvalError


class TestMathMissingCoverage:
    """Test cases specifically designed to address missing coverage in aifpl_math.py."""

    # ========== Comparison Operations Error Handling ==========

    def test_inequality_operators_reject_complex_numbers(self, aifpl):
        """Test that <, >, <=, >= reject complex number arguments."""
        complex_values = [
            "(complex 1 2)",
            "j",
            "(+ 3 j)",
            "(complex -1 5)"
        ]

        comparison_ops = ["<", ">", "<=", ">="]

        for op in comparison_ops:
            for complex_val in complex_values:
                with pytest.raises(AIFPLEvalError, match=f"Function '{op}' does not support complex numbers"):
                    aifpl.evaluate(f"({op} 1 {complex_val})")

                with pytest.raises(AIFPLEvalError, match=f"Function '{op}' does not support complex numbers"):
                    aifpl.evaluate(f"({op} {complex_val} 2)")

    def test_not_equal_all_arguments_equal_edge_case(self, aifpl):
        """Test != operator when all arguments are actually equal (returns False)."""
        # This tests line 190 which was missing coverage
        result = aifpl.evaluate("(!= 5 5 5 5)")
        assert result is False

        result = aifpl.evaluate('(!= "hello" "hello")')
        assert result is False

        result = aifpl.evaluate("(!= #t #t #t)")
        assert result is False

    # ========== Bitwise Operations Error Handling ==========

    def test_bitwise_operations_insufficient_arguments(self, aifpl):
        """Test bitwise operations with insufficient arguments."""
        # bit-or requires at least 2 arguments
        with pytest.raises(AIFPLEvalError, match="bit-or requires at least 2 arguments, got 1"):
            aifpl.evaluate("(bit-or 5)")

        with pytest.raises(AIFPLEvalError, match="bit-or requires at least 2 arguments, got 0"):
            aifpl.evaluate("(bit-or)")

        # bit-and requires at least 2 arguments
        with pytest.raises(AIFPLEvalError, match="bit-and requires at least 2 arguments, got 1"):
            aifpl.evaluate("(bit-and 7)")

        with pytest.raises(AIFPLEvalError, match="bit-and requires at least 2 arguments, got 0"):
            aifpl.evaluate("(bit-and)")

        # bit-xor requires at least 2 arguments
        with pytest.raises(AIFPLEvalError, match="bit-xor requires at least 2 arguments, got 1"):
            aifpl.evaluate("(bit-xor 3)")

        with pytest.raises(AIFPLEvalError, match="bit-xor requires at least 2 arguments, got 0"):
            aifpl.evaluate("(bit-xor)")

    def test_bitwise_operations_wrong_argument_count(self, aifpl):
        """Test bitwise operations with wrong argument count."""
        # bit-not takes exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="bit-not takes exactly 1 argument, got 2"):
            aifpl.evaluate("(bit-not 5 3)")

        with pytest.raises(AIFPLEvalError, match="bit-not takes exactly 1 argument, got 0"):
            aifpl.evaluate("(bit-not)")

        # bit-shift-left takes exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="bit-shift-left takes exactly 2 arguments, got 1"):
            aifpl.evaluate("(bit-shift-left 5)")

        with pytest.raises(AIFPLEvalError, match="bit-shift-left takes exactly 2 arguments, got 3"):
            aifpl.evaluate("(bit-shift-left 5 2 1)")

        # bit-shift-right takes exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="bit-shift-right takes exactly 2 arguments, got 1"):
            aifpl.evaluate("(bit-shift-right 8)")

        with pytest.raises(AIFPLEvalError, match="bit-shift-right takes exactly 2 arguments, got 3"):
            aifpl.evaluate("(bit-shift-right 8 2 1)")

    # ========== Mathematical Functions Error Handling ==========

    def test_trigonometric_functions_wrong_argument_count(self, aifpl):
        """Test trigonometric functions with wrong argument count."""
        trig_functions = ["sin", "cos", "tan"]

        for func in trig_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 0"):
                aifpl.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 2"):
                aifpl.evaluate(f"({func} 1 2)")

    def test_trigonometric_functions_with_complex_numbers(self, aifpl):
        """Test trigonometric functions with complex arguments (should work)."""
        # This tests the complex number branches that were missing coverage
        result = aifpl.evaluate("(sin (complex 1 2))")
        expected = cmath.sin(1+2j)
        assert abs(result - expected) < 1e-10

        result = aifpl.evaluate("(cos (complex 1 2))")
        expected = cmath.cos(1+2j)
        assert abs(result - expected) < 1e-10

        # tan with complex numbers - this tests line 403
        result = aifpl.evaluate("(tan (complex 0.5 0.5))")
        expected = cmath.tan(0.5+0.5j)
        assert abs(result - expected) < 1e-10

    def test_logarithmic_functions_wrong_argument_count(self, aifpl):
        """Test logarithmic functions with wrong argument count."""
        log_functions = ["log", "log10", "exp"]

        for func in log_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 0"):
                aifpl.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 2"):
                aifpl.evaluate(f"({func} 1 2)")

    def test_logarithmic_functions_with_complex_numbers(self, aifpl):
        """Test logarithmic functions with complex arguments."""
        # log10 with complex numbers - this tests line 429
        result = aifpl.evaluate("(log10 (complex -1 0))")
        expected = cmath.log10(-1+0j)
        assert abs(result - expected) < 1e-10

    def test_other_math_functions_wrong_argument_count(self, aifpl):
        """Test other mathematical functions with wrong argument count."""
        single_arg_functions = ["sqrt", "abs", "round", "floor", "ceil"]

        for func in single_arg_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 0"):
                aifpl.evaluate(f"({func})")

            # Too many arguments (except abs which already has good coverage)
            if func != "abs":
                with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 2"):
                    aifpl.evaluate(f"({func} 1 2)")

        # pow function takes exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="pow takes exactly 2 arguments, got 1"):
            aifpl.evaluate("(pow 2)")

        with pytest.raises(AIFPLEvalError, match="pow takes exactly 2 arguments, got 3"):
            aifpl.evaluate("(pow 2 3 4)")

    def test_rounding_functions_with_complex_numbers(self, aifpl):
        """Test rounding functions with complex numbers (should fail)."""
        rounding_functions = ["round", "floor", "ceil"]

        # Test with complex numbers that have non-zero imaginary parts
        for func in rounding_functions:
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' does not support complex numbers"):
                aifpl.evaluate(f"({func} (complex 3.5 2.1))")

        # Test the edge case where complex number has very small imaginary part
        # This tests lines 476, 495, 514 which handle the tolerance check
        for func in rounding_functions:
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' does not support complex numbers"):
                aifpl.evaluate(f"({func} (complex 3.5 1e-5))")

    # ========== Base Conversion Error Handling ==========

    def test_base_conversion_functions_wrong_argument_count(self, aifpl):
        """Test base conversion functions with wrong argument count."""
        base_functions = ["bin", "hex", "oct"]

        for func in base_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 0"):
                aifpl.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(AIFPLEvalError, match=f"{func} takes exactly 1 argument, got 2"):
                aifpl.evaluate(f"({func} 15 16)")

    # ========== Complex Number Functions Error Handling ==========

    def test_complex_number_functions_wrong_argument_count(self, aifpl):
        """Test complex number functions with wrong argument count."""
        # real function
        with pytest.raises(AIFPLEvalError, match="real takes exactly 1 argument, got 0"):
            aifpl.evaluate("(real)")

        with pytest.raises(AIFPLEvalError, match="real takes exactly 1 argument, got 2"):
            aifpl.evaluate("(real 1 2)")

        # imag function
        with pytest.raises(AIFPLEvalError, match="imag takes exactly 1 argument, got 0"):
            aifpl.evaluate("(imag)")

        with pytest.raises(AIFPLEvalError, match="imag takes exactly 1 argument, got 2"):
            aifpl.evaluate("(imag 1 2)")

        # complex function
        with pytest.raises(AIFPLEvalError, match="complex takes exactly 2 arguments, got 1"):
            aifpl.evaluate("(complex 5)")

        with pytest.raises(AIFPLEvalError, match="complex takes exactly 2 arguments, got 3"):
            aifpl.evaluate("(complex 1 2 3)")

    def test_complex_function_with_complex_arguments(self, aifpl):
        """Test complex function with complex number arguments (should fail)."""
        # This tests line 632
        with pytest.raises(AIFPLEvalError, match="complex arguments must be real numbers"):
            aifpl.evaluate("(complex (complex 1 2) 3)")

        with pytest.raises(AIFPLEvalError, match="complex arguments must be real numbers"):
            aifpl.evaluate("(complex 1 (complex 2 3))")

    def test_real_imag_functions_with_complex_return_paths(self, aifpl):
        """Test real/imag functions with complex numbers to hit return paths."""
        # Test real function with complex number (tests line 599 path)
        # First create a complex number with non-integer real part
        result = aifpl.evaluate("(real (complex 3.7 4.2))")
        assert result == 3.7

        # Test imag function with complex number (tests line 618 path)
        result = aifpl.evaluate("(imag (complex 3.7 4.2))")
        assert result == 4.2

    # ========== Type Checking Helper Methods ==========

    def test_ensure_real_number_with_non_numeric_input(self, aifpl):
        """Test _ensure_real_number with non-numeric input."""
        # This tests line 664 in _ensure_real_number
        # We can't directly test the helper method, but we can test functions that use it
        # min/max functions use _ensure_real_number
        with pytest.raises(AIFPLEvalError, match="Function 'min' requires numeric arguments"):
            aifpl.evaluate('(min "hello" 2)')

        with pytest.raises(AIFPLEvalError, match="Function 'max' requires numeric arguments"):
            aifpl.evaluate('(max #t 5)')

    def test_ensure_real_number_with_complex_input(self, aifpl):
        """Test _ensure_real_number with complex input."""
        # This tests line 667 in _ensure_real_number
        # min/max functions use _ensure_real_number and should reject complex numbers
        with pytest.raises(AIFPLEvalError, match="Function 'min' does not support complex numbers"):
            aifpl.evaluate("(min (complex 1 2) 5)")

        with pytest.raises(AIFPLEvalError, match="Function 'max' does not support complex numbers"):
            aifpl.evaluate("(max j 3)")

    # ========== Additional Edge Cases ==========

    def test_comparison_operators_error_handling(self, aifpl):
        """Test comparison operators with insufficient arguments."""
        comparison_ops = ["=", "!=", "<", ">", "<=", ">="]

        for op in comparison_ops:
            # All comparison operators require at least 2 arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires at least 2 arguments, got 1"):
                aifpl.evaluate(f"({op} 5)")

            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires at least 2 arguments, got 0"):
                aifpl.evaluate(f"({op})")

    def test_comparison_operators_with_non_numeric_arguments(self, aifpl):
        """Test comparison operators with non-numeric arguments."""
        numeric_comparison_ops = ["<", ">", "<=", ">="]

        for op in numeric_comparison_ops:
            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires numeric arguments"):
                aifpl.evaluate(f'({op} "hello" 5)')

            with pytest.raises(AIFPLEvalError, match=f"Function '{op}' requires numeric arguments"):
                aifpl.evaluate(f'({op} 5 "world")')

    def test_boolean_not_function_error_handling(self, aifpl):
        """Test not function with wrong argument count and type."""
        # Wrong argument count
        with pytest.raises(AIFPLEvalError, match="not takes exactly 1 argument, got 0"):
            aifpl.evaluate("(not)")

        with pytest.raises(AIFPLEvalError, match="not takes exactly 1 argument, got 2"):
            aifpl.evaluate("(not #t #f)")

        # Wrong argument type
        with pytest.raises(AIFPLEvalError, match="Function 'not' requires boolean arguments"):
            aifpl.evaluate("(not 5)")

        with pytest.raises(AIFPLEvalError, match="Function 'not' requires boolean arguments"):
            aifpl.evaluate('(not "hello")')

    def test_arithmetic_operations_with_non_numeric_arguments(self, aifpl):
        """Test arithmetic operations with non-numeric arguments."""
        # Test individual cases to avoid the loop issue
        with pytest.raises(AIFPLEvalError, match="Function '\\+' requires numeric arguments"):
            aifpl.evaluate('(+ "hello" 5)')

        with pytest.raises(AIFPLEvalError, match="Function '-' requires numeric arguments"):
            aifpl.evaluate('(- "hello" 5)')

        with pytest.raises(AIFPLEvalError, match="Function '\\*' requires numeric arguments"):
            aifpl.evaluate('(* "hello" 5)')

        with pytest.raises(AIFPLEvalError, match="Function '/' requires numeric arguments"):
            aifpl.evaluate('(/ "hello" 5)')

        with pytest.raises(AIFPLEvalError, match="Function '//' requires numeric arguments"):
            aifpl.evaluate('(// "hello" 5)')

        with pytest.raises(AIFPLEvalError, match="Function '%' requires numeric arguments"):
            aifpl.evaluate('(% "hello" 5)')

        with pytest.raises(AIFPLEvalError, match="Function '\\*\\*' requires numeric arguments"):
            aifpl.evaluate('(** "hello" 5)')

    def test_floor_division_and_modulo_argument_validation(self, aifpl):
        """Test floor division and modulo argument count validation."""
        # Floor division requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="Floor division takes exactly 2 arguments, got 1"):
            aifpl.evaluate("(// 5)")

        with pytest.raises(AIFPLEvalError, match="Floor division takes exactly 2 arguments, got 3"):
            aifpl.evaluate("(// 10 3 2)")

        # Modulo requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="Modulo takes exactly 2 arguments, got 1"):
            aifpl.evaluate("(% 5)")

        with pytest.raises(AIFPLEvalError, match="Modulo takes exactly 2 arguments, got 3"):
            aifpl.evaluate("(% 10 3 2)")

    def test_exponentiation_argument_validation(self, aifpl):
        """Test exponentiation argument count validation."""
        # ** requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="Power takes exactly 2 arguments, got 1"):
            aifpl.evaluate("(** 2)")

        with pytest.raises(AIFPLEvalError, match="Power takes exactly 2 arguments, got 3"):
            aifpl.evaluate("(** 2 3 4)")
