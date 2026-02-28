"""Tests to address missing coverage in menai_math.py."""

import re
import pytest
import cmath

from menai import MenaiEvalError


class TestMathMissingCoverage:
    """Test cases specifically designed to address missing coverage in menai_math.py."""

    # ========== Comparison Operations Error Handling ==========

    def test_inequality_operators_reject_complex_numbers(self, menai):
        """Test that integer typed comparison operators reject complex number arguments."""
        complex_values = [
            "(float->complex 1.0 2.0)",
            "1j",
            "(float->complex 3.0 1.0)",
            "(integer->complex -1 5)"
        ]

        comparison_ops = ["integer<?", "integer>?", "integer<=?", "integer>=?"]

        for op in comparison_ops:
            for complex_val in complex_values:
                with pytest.raises(MenaiEvalError, match=f"requires integer arguments"):
                    menai.evaluate(f"({op} 1 {complex_val})")

                with pytest.raises(MenaiEvalError, match=f"requires integer arguments"):
                    menai.evaluate(f"({op} {complex_val} 2)")

    def test_not_equal_all_arguments_equal_edge_case(self, menai):
        """Test != operator when all arguments are actually equal (returns False)."""
        # This tests line 190 which was missing coverage
        result = menai.evaluate("(integer!=? 5 5 5 5)")
        assert result is False

        result = menai.evaluate('(string!=? "hello" "hello")')
        assert result is False

        result = menai.evaluate("(boolean!=? #t #t #t)")
        assert result is False

    # ========== Bitwise Operations Error Handling ==========

    def test_bitwise_operations_insufficient_arguments(self, menai):
        """Test bitwise operations with insufficient arguments."""
        # integer-bit-or requires at least 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-or' has wrong number of arguments"):
            menai.evaluate("(integer-bit-or 5)")

        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-or' has wrong number of arguments"):
            menai.evaluate("(integer-bit-or)")

        # integer-bit-and requires at least 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-and' has wrong number of arguments"):
            menai.evaluate("(integer-bit-and 7)")

        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-and' has wrong number of arguments"):
            menai.evaluate("(integer-bit-and)")

        # integer-bit-xor requires at least 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-xor' has wrong number of arguments"):
            menai.evaluate("(integer-bit-xor 3)")

        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-xor' has wrong number of arguments"):
            menai.evaluate("(integer-bit-xor)")

    def test_bitwise_operations_wrong_argument_count(self, menai):
        """Test bitwise operations with wrong argument count."""
        # integer-bit-not requires exactly 1 argument
        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-not' has wrong number of arguments"):
            menai.evaluate("(integer-bit-not 5 3)")

        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-not' has wrong number of arguments"):
            menai.evaluate("(integer-bit-not)")

        # integer-bit-shift-left requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-shift-left' has wrong number of arguments"):
            menai.evaluate("(integer-bit-shift-left 5)")

        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-shift-left' has wrong number of arguments"):
            menai.evaluate("(integer-bit-shift-left 5 2 1)")

        # integer-bit-shift-right requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-shift-right' has wrong number of arguments"):
            menai.evaluate("(integer-bit-shift-right 8)")

        with pytest.raises(MenaiEvalError, match="Function 'integer-bit-shift-right' has wrong number of arguments"):
            menai.evaluate("(integer-bit-shift-right 8 2 1)")

    # ========== Mathematical Functions Error Handling ==========

    def test_trigonometric_functions_wrong_argument_count(self, menai):
        """Test trigonometric functions with wrong argument count."""
        trig_functions = ["float-sin", "float-cos", "float-tan"]

        for func in trig_functions:
            # No arguments
            with pytest.raises(MenaiEvalError, match=f"Function '{func}' has wrong number of arguments"):
                menai.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(MenaiEvalError, match=f"Function '{func}' has wrong number of arguments"):
                menai.evaluate(f"({func} 1 2)")

    def test_trigonometric_functions_with_complex_numbers(self, menai):
        """Test trigonometric functions with complex arguments (should work)."""
        # This tests the complex number branches that were missing coverage
        result = menai.evaluate("(complex-sin (integer->complex 1 2))")
        expected = cmath.sin(1+2j)
        assert abs(result - expected) < 1e-10

        result = menai.evaluate("(complex-cos (integer->complex 1 2))")
        expected = cmath.cos(1+2j)
        assert abs(result - expected) < 1e-10

        # tan with complex numbers - this tests line 403
        result = menai.evaluate("(complex-tan (float->complex 0.5 0.5))")
        expected = cmath.tan(0.5+0.5j)
        assert abs(result - expected) < 1e-10

    def test_logarithmic_functions_wrong_argument_count(self, menai):
        """Test logarithmic functions with wrong argument count."""
        log_functions = ["float-log", "float-log10", "float-exp"]

        for func in log_functions:
            # No arguments
            with pytest.raises(MenaiEvalError, match=f"Function '{func}' has wrong number of arguments"):
                menai.evaluate(f"({func})")

            # Too many arguments
            with pytest.raises(MenaiEvalError, match=f"Function '{func}' has wrong number of arguments"):
                menai.evaluate(f"({func} 1.0 2.0)")

    def test_logarithmic_functions_with_complex_numbers(self, menai):
        """Test logarithmic functions with complex arguments."""
        # log10 with complex numbers - this tests line 429
        result = menai.evaluate("(complex-log10 (integer->complex -1 0))")
        expected = cmath.log10(-1+0j)
        assert abs(result - expected) < 1e-10

    def test_other_math_functions_wrong_argument_count(self, menai):
        """Test other mathematical functions with wrong argument count."""
        single_arg_functions = ["float-sqrt", "float-abs", "float-round", "float-floor", "float-ceil"]

        for func in single_arg_functions:
            # No arguments
            with pytest.raises(MenaiEvalError, match=f"Function '{func}' has wrong number of arguments"):
                menai.evaluate(f"({func})")

            # Too many arguments (except abs which already has good coverage)
            if func != "float-abs":
                with pytest.raises(MenaiEvalError, match=f"Function '{func}' has wrong number of arguments"):
                    menai.evaluate(f"({func} 1.0 2.0)")

        # float-expn function requires at least 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'float-expn' has wrong number of arguments"):
            menai.evaluate("(float-expn 2.0)")

    def test_rounding_functions_with_complex_numbers(self, menai):
        """Test rounding functions with complex numbers (should fail)."""
        rounding_functions = ["float-round", "float-floor", "float-ceil"]

        # Test with complex numbers that have non-zero imaginary parts
        for func in rounding_functions:
            with pytest.raises(MenaiEvalError, match=f"requires float arguments"):
                menai.evaluate(f"({func} (float->complex 3.5 2.1))")

        # Test the edge case where complex number has very small imaginary part
        # This tests lines 476, 495, 514 which handle the tolerance check
        for func in rounding_functions:
            with pytest.raises(MenaiEvalError, match=f"requires float arguments"):
                menai.evaluate(f"({func} (float->complex 3.5 1e-5))")

    # ========== Complex Number Functions Error Handling ==========

    def test_complex_number_functions_wrong_argument_count(self, menai):
        """Test complex number functions with wrong argument count."""
        # real function
        with pytest.raises(MenaiEvalError, match="Function 'complex-real' has wrong number of arguments"):
            menai.evaluate("(complex-real)")

        with pytest.raises(MenaiEvalError, match="Function 'complex-real' has wrong number of arguments"):
            menai.evaluate("(complex-real 1 2)")

        # imag function
        with pytest.raises(MenaiEvalError, match="Function 'complex-imag' has wrong number of arguments"):
            menai.evaluate("(complex-imag)")

        with pytest.raises(MenaiEvalError, match="Function 'complex-imag' has wrong number of arguments"):
            menai.evaluate("(complex-imag 1 2)")

        # complex function
        with pytest.raises(MenaiEvalError, match="Function 'integer->complex' has wrong number of arguments"):
            menai.evaluate("(integer->complex 1 2 3)")

        with pytest.raises(MenaiEvalError, match="Function 'float->complex' has wrong number of arguments"):
            menai.evaluate("(float->complex 1.0 2.0 3.0)")

    def test_complex_function_with_complex_arguments(self, menai):
        """Test complex function with complex number arguments (should fail)."""
        # This tests line 632
        with pytest.raises(MenaiEvalError, match="requires float argument"):
            menai.evaluate("(float->complex (float->complex 1 2) 3)")

        with pytest.raises(MenaiEvalError, match="requires float argument"):
            menai.evaluate("(float->complex 1 (float->complex 2 3))")

    def test_real_imag_functions_with_complex_return_paths(self, menai):
        """Test real/imag functions with complex numbers to hit return paths."""
        # Test real function with complex number (tests line 599 path)
        # First create a complex number with non-integer real part
        result = menai.evaluate("(complex-real (float->complex 3.7 4.2))")
        assert result == 3.7

        # Test imag function with complex number (tests line 618 path)
        result = menai.evaluate("(complex-imag (float->complex 3.7 4.2))")
        assert result == 4.2

    # ========== Type Checking Helper Methods ==========

    def test_ensure_real_number_with_non_numeric_input(self, menai):
        """Test _ensure_real_number with non-numeric input."""
        # This tests line 664 in _ensure_real_number
        # We can't directly test the helper method, but we can test functions that use it
        # min/max functions use _ensure_real_number
        with pytest.raises(MenaiEvalError, match="Function 'integer-min' requires integer arguments"):
            menai.evaluate('(integer-min "hello" 2)')

        with pytest.raises(MenaiEvalError, match="Function 'integer-max' requires integer arguments"):
            menai.evaluate('(integer-max #t 5)')

        with pytest.raises(MenaiEvalError, match="Function 'float-min' requires float arguments"):
            menai.evaluate('(float-min "hello" 2.0)')

        with pytest.raises(MenaiEvalError, match="Function 'float-max' requires float arguments"):
            menai.evaluate('(float-max #t 5.0)')

    def test_ensure_float_with_complex_input(self, menai):
        """Test _ensure_float with complex input."""
        # This tests line 667 in _ensure_real_number
        # min/max functions use _ensure_real_number and should reject complex numbers
        with pytest.raises(MenaiEvalError, match="requires float arguments"):
            menai.evaluate("(float-min (float->complex 1 2) 5.0)")

        with pytest.raises(MenaiEvalError, match="requires float arguments"):
            menai.evaluate("(float-max 1j 3)")

    # ========== Additional Edge Cases ==========

    def test_comparison_operators_error_handling(self, menai):
        """Test typed comparison operators with insufficient arguments."""
        comparison_ops = ["integer=?", "integer!=?"]

        for op in comparison_ops:
            with pytest.raises(MenaiEvalError, match=f"Function '{re.escape(op)}' has wrong number of arguments"):
                menai.evaluate(f"({op} 5)")

            with pytest.raises(MenaiEvalError, match=f"Function '{re.escape(op)}' has wrong number of arguments"):
                menai.evaluate(f"({op})")

    def test_boolean_not_function_error_handling(self, menai):
        """Test not function with wrong argument count and type."""
        # Wrong argument count
        with pytest.raises(MenaiEvalError, match="Function 'boolean-not' has wrong number of arguments"):
            menai.evaluate("(boolean-not)")

        with pytest.raises(MenaiEvalError, match="Function 'boolean-not' has wrong number of arguments"):
            menai.evaluate("(boolean-not #t #f)")

        # Wrong argument type
        with pytest.raises(MenaiEvalError, match="Function 'boolean-not' requires boolean arguments"):
            menai.evaluate("(boolean-not 5)")

        with pytest.raises(MenaiEvalError, match="Function 'boolean-not' requires boolean arguments"):
            menai.evaluate('(boolean-not "hello")')

    def test_floor_division_and_modulo_argument_validation(self, menai):
        """Test floor division and modulo argument count validation."""
        # Floor division requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'float//' has wrong number of arguments"):
            menai.evaluate("(float// 5.0)")

        with pytest.raises(MenaiEvalError, match="Function 'float//' has wrong number of arguments"):
            menai.evaluate("(float// 10.0 3.0 2.0)")

        # Modulo requires exactly 2 arguments
        with pytest.raises(MenaiEvalError, match="Function 'integer%' has wrong number of arguments"):
            menai.evaluate("(integer% 5)")

        with pytest.raises(MenaiEvalError, match="Function 'integer%' has wrong number of arguments"):
            menai.evaluate("(integer% 10 3 2)")
