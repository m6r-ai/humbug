"""Tests to address missing coverage in aifpl_math.py."""

import re
import pytest
import cmath

from aifpl import AIFPLEvalError


class TestMathMissingCoverage:
    """Test cases specifically designed to address missing coverage in aifpl_math.py."""

    # ========== Comparison Operations Error Handling ==========

    def test_inequality_operators_reject_complex_numbers(self, aifpl):
        """Test that integer typed comparison operators reject complex number arguments."""
        complex_values = [
            "(complex 1 2)",
            "1j",
            "(complex 3 1)",
            "(complex -1 5)"
        ]

        comparison_ops = ["integer<?", "integer>?", "integer<=?", "integer>=?"]

        for op in comparison_ops:
            for complex_val in complex_values:
                with pytest.raises(AIFPLEvalError, match=f"requires integer arguments"):
                    aifpl.evaluate(f"({op} 1 {complex_val})")

                with pytest.raises(AIFPLEvalError, match=f"requires integer arguments"):
                    aifpl.evaluate(f"({op} {complex_val} 2)")

    def test_not_equal_all_arguments_equal_edge_case(self, aifpl):
        """Test != operator when all arguments are actually equal (returns False)."""
        # This tests line 190 which was missing coverage
        result = aifpl.evaluate("(integer!=? 5 5 5 5)")
        assert result is False

        result = aifpl.evaluate('(string!=? "hello" "hello")')
        assert result is False

        result = aifpl.evaluate("(boolean!=? #t #t #t)")
        assert result is False

    # ========== Bitwise Operations Error Handling ==========

    def test_bitwise_operations_insufficient_arguments(self, aifpl):
        """Test bitwise operations with insufficient arguments."""
        # bit-or requires at least 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'bit-or' has wrong number of arguments"):
            aifpl.evaluate("(bit-or 5)")

        with pytest.raises(AIFPLEvalError, match="Function 'bit-or' has wrong number of arguments"):
            aifpl.evaluate("(bit-or)")

        # bit-and requires at least 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'bit-and' has wrong number of arguments"):
            aifpl.evaluate("(bit-and 7)")

        with pytest.raises(AIFPLEvalError, match="Function 'bit-and' has wrong number of arguments"):
            aifpl.evaluate("(bit-and)")

        # bit-xor requires at least 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'bit-xor' has wrong number of arguments"):
            aifpl.evaluate("(bit-xor 3)")

        with pytest.raises(AIFPLEvalError, match="Function 'bit-xor' has wrong number of arguments"):
            aifpl.evaluate("(bit-xor)")

    def test_bitwise_operations_wrong_argument_count(self, aifpl):
        """Test bitwise operations with wrong argument count."""
        # bit-not requires exactly 1 argument
        with pytest.raises(AIFPLEvalError, match="Function 'bit-not' has wrong number of arguments"):
            aifpl.evaluate("(bit-not 5 3)")

        with pytest.raises(AIFPLEvalError, match="Function 'bit-not' has wrong number of arguments"):
            aifpl.evaluate("(bit-not)")

        # bit-shift-left requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'bit-shift-left' has wrong number of arguments"):
            aifpl.evaluate("(bit-shift-left 5)")

        with pytest.raises(AIFPLEvalError, match="Function 'bit-shift-left' has wrong number of arguments"):
            aifpl.evaluate("(bit-shift-left 5 2 1)")

        # bit-shift-right requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'bit-shift-right' has wrong number of arguments"):
            aifpl.evaluate("(bit-shift-right 8)")

        with pytest.raises(AIFPLEvalError, match="Function 'bit-shift-right' has wrong number of arguments"):
            aifpl.evaluate("(bit-shift-right 8 2 1)")

    # ========== Mathematical Functions Error Handling ==========

    def test_trigonometric_functions_wrong_argument_count(self, aifpl):
        """Test trigonometric functions with wrong argument count."""
        trig_functions = ["float-sin", "float-cos", "float-tan"]

        for func in trig_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                aifpl.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                aifpl.evaluate(f"({func} 1 2)")

    def test_trigonometric_functions_with_complex_numbers(self, aifpl):
        """Test trigonometric functions with complex arguments (should work)."""
        # This tests the complex number branches that were missing coverage
        result = aifpl.evaluate("(complex-sin (complex 1 2))")
        expected = cmath.sin(1+2j)
        assert abs(result - expected) < 1e-10

        result = aifpl.evaluate("(complex-cos (complex 1 2))")
        expected = cmath.cos(1+2j)
        assert abs(result - expected) < 1e-10

        # tan with complex numbers - this tests line 403
        result = aifpl.evaluate("(complex-tan (complex 0.5 0.5))")
        expected = cmath.tan(0.5+0.5j)
        assert abs(result - expected) < 1e-10

    def test_logarithmic_functions_wrong_argument_count(self, aifpl):
        """Test logarithmic functions with wrong argument count."""
        log_functions = ["float-log", "float-log10", "float-exp"]

        for func in log_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                aifpl.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                aifpl.evaluate(f"({func} 1.0 2.0)")

    def test_logarithmic_functions_with_complex_numbers(self, aifpl):
        """Test logarithmic functions with complex arguments."""
        # log10 with complex numbers - this tests line 429
        result = aifpl.evaluate("(complex-log10 (complex -1 0))")
        expected = cmath.log10(-1+0j)
        assert abs(result - expected) < 1e-10

    def test_other_math_functions_wrong_argument_count(self, aifpl):
        """Test other mathematical functions with wrong argument count."""
        single_arg_functions = ["float-sqrt", "float-abs", "float-round", "float-floor", "float-ceil"]

        for func in single_arg_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                aifpl.evaluate(f"({func})")

            # Too many arguments (except abs which already has good coverage)
            if func != "float-abs":
                with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                    aifpl.evaluate(f"({func} 1.0 2.0)")

        # float-expt function requires at least 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'float-expt' has wrong number of arguments"):
            aifpl.evaluate("(float-expt 2.0)")

    def test_rounding_functions_with_complex_numbers(self, aifpl):
        """Test rounding functions with complex numbers (should fail)."""
        rounding_functions = ["float-round", "float-floor", "float-ceil"]

        # Test with complex numbers that have non-zero imaginary parts
        for func in rounding_functions:
            with pytest.raises(AIFPLEvalError, match=f"requires float arguments"):
                aifpl.evaluate(f"({func} (complex 3.5 2.1))")

        # Test the edge case where complex number has very small imaginary part
        # This tests lines 476, 495, 514 which handle the tolerance check
        for func in rounding_functions:
            with pytest.raises(AIFPLEvalError, match=f"requires float arguments"):
                aifpl.evaluate(f"({func} (complex 3.5 1e-5))")

    # ========== Base Conversion Error Handling ==========

    def test_base_conversion_functions_wrong_argument_count(self, aifpl):
        """Test base conversion functions with wrong argument count."""
        base_functions = ["bin", "hex", "oct"]

        for func in base_functions:
            # No arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                aifpl.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(AIFPLEvalError, match=f"Function '{func}' has wrong number of arguments"):
                aifpl.evaluate(f"({func} 15 16)")

    # ========== Complex Number Functions Error Handling ==========

    def test_complex_number_functions_wrong_argument_count(self, aifpl):
        """Test complex number functions with wrong argument count."""
        # real function
        with pytest.raises(AIFPLEvalError, match="Function 'real' has wrong number of arguments"):
            aifpl.evaluate("(real)")

        with pytest.raises(AIFPLEvalError, match="Function 'real' has wrong number of arguments"):
            aifpl.evaluate("(real 1 2)")

        # imag function
        with pytest.raises(AIFPLEvalError, match="Function 'imag' has wrong number of arguments"):
            aifpl.evaluate("(imag)")

        with pytest.raises(AIFPLEvalError, match="Function 'imag' has wrong number of arguments"):
            aifpl.evaluate("(imag 1 2)")

        # complex function
        with pytest.raises(AIFPLEvalError, match="Function 'complex' has wrong number of arguments"):
            aifpl.evaluate("(complex 5)")

        with pytest.raises(AIFPLEvalError, match="Function 'complex' has wrong number of arguments"):
            aifpl.evaluate("(complex 1 2 3)")

    def test_complex_function_with_complex_arguments(self, aifpl):
        """Test complex function with complex number arguments (should fail)."""
        # This tests line 632
        with pytest.raises(AIFPLEvalError, match="requires float argument"):
            aifpl.evaluate("(complex (complex 1 2) 3)")

        with pytest.raises(AIFPLEvalError, match="requires float argument"):
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
        with pytest.raises(AIFPLEvalError, match="Function 'integer-min' requires integer arguments"):
            aifpl.evaluate('(integer-min "hello" 2)')

        with pytest.raises(AIFPLEvalError, match="Function 'integer-max' requires integer arguments"):
            aifpl.evaluate('(integer-max #t 5)')

        with pytest.raises(AIFPLEvalError, match="Function 'float-min' requires float arguments"):
            aifpl.evaluate('(float-min "hello" 2.0)')

        with pytest.raises(AIFPLEvalError, match="Function 'float-max' requires float arguments"):
            aifpl.evaluate('(float-max #t 5.0)')

    def test_ensure_float_with_complex_input(self, aifpl):
        """Test _ensure_float with complex input."""
        # This tests line 667 in _ensure_real_number
        # min/max functions use _ensure_real_number and should reject complex numbers
        with pytest.raises(AIFPLEvalError, match="requires float arguments"):
            aifpl.evaluate("(float-min (complex 1 2) 5.0)")

        with pytest.raises(AIFPLEvalError, match="requires float arguments"):
            aifpl.evaluate("(float-max 1j 3)")

    # ========== Additional Edge Cases ==========

    def test_comparison_operators_error_handling(self, aifpl):
        """Test typed comparison operators with insufficient arguments."""
        comparison_ops = ["integer=?", "integer!=?"]

        for op in comparison_ops:
            with pytest.raises(AIFPLEvalError, match=f"Function '{re.escape(op)}' has wrong number of arguments"):
                aifpl.evaluate(f"({op} 5)")

            with pytest.raises(AIFPLEvalError, match=f"Function '{re.escape(op)}' has wrong number of arguments"):
                aifpl.evaluate(f"({op})")

    def test_boolean_not_function_error_handling(self, aifpl):
        """Test not function with wrong argument count and type."""
        # Wrong argument count
        with pytest.raises(AIFPLEvalError, match="Function 'not' has wrong number of arguments"):
            aifpl.evaluate("(not)")

        with pytest.raises(AIFPLEvalError, match="Function 'not' has wrong number of arguments"):
            aifpl.evaluate("(not #t #f)")

        # Wrong argument type
        with pytest.raises(AIFPLEvalError, match="Function 'not' requires boolean arguments"):
            aifpl.evaluate("(not 5)")

        with pytest.raises(AIFPLEvalError, match="Function 'not' requires boolean arguments"):
            aifpl.evaluate('(not "hello")')

    def test_floor_division_and_modulo_argument_validation(self, aifpl):
        """Test floor division and modulo argument count validation."""
        # Floor division requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'float//' has wrong number of arguments"):
            aifpl.evaluate("(float// 5.0)")

        with pytest.raises(AIFPLEvalError, match="Function 'float//' has wrong number of arguments"):
            aifpl.evaluate("(float// 10.0 3.0 2.0)")

        # Modulo requires exactly 2 arguments
        with pytest.raises(AIFPLEvalError, match="Function 'integer%' has wrong number of arguments"):
            aifpl.evaluate("(integer% 5)")

        with pytest.raises(AIFPLEvalError, match="Function 'integer%' has wrong number of arguments"):
            aifpl.evaluate("(integer% 10 3 2)")
