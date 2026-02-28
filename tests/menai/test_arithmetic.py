"""Tests for arithmetic operations and mathematical functions."""

import pytest
import math
import cmath

from menai import MenaiEvalError


class TestArithmetic:
    """Test arithmetic operations and mathematical functions."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic integer addition
        ("(integer+ 1 2)", "3"),
        ("(integer+ 1 2 3)", "6"),
        ("(integer+ 1 2 3 4)", "10"),

        # Negative numbers
        ("(integer+ -1 2)", "1"),
        ("(integer+ 1 -2)", "-1"),
        ("(integer+ -1 -2)", "-3"),
    ])
    def test_integer_addition(self, menai, expression, expected):
        """Test integer addition operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic float addition
        ("(float+ 1.5 2.5)", "4.0"),
        ("(float+ 1.1 2.2)", "3.3000000000000003"),  # Floating point precision
        ("(float+ 1.0 2.0 3.0)", "6.0"),
    ])
    def test_float_addition(self, menai, expression, expected):
        """Test float addition operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Complex addition
        ("(complex+ (integer->complex 1 2) (integer->complex 3 4))", "4+6j"),
    ])
    def test_complex_addition(self, menai, expression, expected):
        """Test complex addition operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic integer subtraction
        ("(integer- 5 3)", "2"),
        ("(integer- 10 3 2)", "5"),  # Left associative: ((10 - 3) - 2)

        # Multiple arguments
        ("(integer- 10 1 2 3)", "4"),  # ((((10 - 1) - 2) - 3)
    ])
    def test_integer_subtraction(self, menai, expression, expected):
        """Test integer subtraction operation including unary minus."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic float subtraction
        ("(float- 5.5 2.5)", "3.0"),
        ("(float- 10.0 3.0 2.0)", "5.0"),
    ])
    def test_float_subtraction(self, menai, expression, expected):
        """Test float subtraction operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Complex subtraction
        ("(complex- (integer->complex 5 3) (integer->complex 2 1))", "3+2j"),
    ])
    def test_complex_subtraction(self, menai, expression, expected):
        """Test complex subtraction operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic integer multiplication
        ("(integer* 2 3)", "6"),
        ("(integer* 2 3 4)", "24"),

        # Zero multiplication
        ("(integer* 5 0)", "0"),
        ("(integer* 0 5)", "0"),

        # Negative numbers
        ("(integer* -2 3)", "-6"),
        ("(integer* 2 -3)", "-6"),
        ("(integer* -2 -3)", "6"),
    ])
    def test_integer_multiplication(self, menai, expression, expected):
        """Test integer multiplication operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic float multiplication
        ("(float* 2.5 4.0)", "10.0"),
        ("(float* 3.0 3.5)", "10.5"),
    ])
    def test_float_multiplication(self, menai, expression, expected):
        """Test float multiplication operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Complex multiplication
        ("(complex* (integer->complex 2 3) (integer->complex 1 4))", "-10+11j"),
    ])
    def test_complex_multiplication(self, menai, expression, expected):
        """Test complex multiplication operation."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Integer division (floor division semantics)
        ("(integer/ 6 2)", "3"),
        ("(integer/ 8 4)", "2"),
        ("(integer/ 7 2)", "3"),  # Floor division

        # Multiple arguments (left associative)
        ("(integer/ 24 2 3)", "4"),
        ("(integer/ 100 5 2)", "10"),
    ])
    def test_integer_division(self, menai, expression, expected):
        """Test integer division operation (floor division)."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Float division
        ("(float/ 5.0 2.0)", "2.5"),
        ("(float/ 1.0 3.0)", "0.3333333333333333"),
        ("(float/ 10.0 2.5)", "4.0"),

        # Multiple arguments (left associative)
        ("(float/ 24.0 2.0 3.0)", "4.0"),
    ])
    def test_float_division(self, menai, expression, expected):
        """Test float division operation."""
        assert menai.evaluate_and_format(expression) == expected

    def test_division_by_zero(self, menai):
        """Test that division by zero raises appropriate error."""
        with pytest.raises(MenaiEvalError, match="Division by zero"):
            menai.evaluate("(integer/ 1 0)")

        with pytest.raises(MenaiEvalError, match="Division by zero"):
            menai.evaluate("(float/ 1.0 0.0)")

    @pytest.mark.parametrize("expression,expected", [
        # Basic modulo (float->integer inputs only)
        ("(integer% 7 3)", "1"),
        ("(integer% 8 3)", "2"),
        ("(integer% 9 3)", "0"),

        # Negative numbers
        ("(integer% -7 3)", "2"),  # Python modulo behavior
        ("(integer% 7 -3)", "-2"),
        ("(integer% -7 -3)", "-1"),
    ])
    def test_modulo(self, menai, expression, expected):
        """Test modulo operation."""
        assert menai.evaluate_and_format(expression) == expected

    def test_modulo_by_zero(self, menai):
        """Test that modulo by zero raises error."""
        with pytest.raises(MenaiEvalError, match="Modulo by zero"):
            menai.evaluate("(integer% 1 0)")

    @pytest.mark.parametrize("expression,expected", [
        # Basic pow (float->integer inputs)
        ("(float-expn 2.0 3.0)", "8.0"),
        ("(float-expn 3.0 2.0)", "9.0"),
        ("(float-expn 5.0 0.0)", "1.0"),
        ("(float-expn 0.0 5.0)", "0.0"),
    ])
    def test_pow_function(self, menai, expression, expected):
        """Test pow function."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic integer exponentiation
        ("(integer-expn 2 10)", "1024"),
        ("(integer-expn 3 3)", "27"),
        ("(integer-expn 5 0)", "1"),
        ("(integer-expn 0 0)", "1"),
        ("(integer-expn 0 5)", "0"),
        ("(integer-expn 1 1000)", "1"),

        # Negative base
        ("(integer-expn -2 3)", "-8"),
        ("(integer-expn -2 4)", "16"),
        ("(integer-expn -1 0)", "1"),

        # Arbitrary precision (Python int ** int stays exact)
        ("(integer-expn 2 64)", "18446744073709551616"),
        ("(integer-expn 10 20)", "100000000000000000000"),
    ])
    def test_integer_expt(self, menai, expression, expected):
        """Test integer-expn function (exact arbitrary-precision integer exponentiation)."""
        assert menai.evaluate_and_format(expression) == expected

    def test_integer_expt_negative_exponent_error(self, menai):
        """Test that integer-expn raises on negative exponent (result would not be an integer)."""
        with pytest.raises(MenaiEvalError, match="non-negative exponent"):
            menai.evaluate("(integer-expn 2 -1)")

        with pytest.raises(MenaiEvalError, match="non-negative exponent"):
            menai.evaluate("(integer-expn 10 -3)")

    def test_integer_expt_type_errors(self, menai):
        """Test that integer-expn rejects non-integer arguments."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer-expn 2.0 3)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer-expn 2 3.0)")

    @pytest.mark.parametrize("expression,expected_approx", [
        # Trigonometric functions
        ("(float-sin 0.0)", 0.0),
        ("(float-sin (float* pi 0.5))", 1.0),
        ("(float-sin pi)", 0.0),
        ("(float-cos 0.0)", 1.0),
        ("(float-cos (float* pi 0.5))", 0.0),
        ("(float-cos pi)", -1.0),
        ("(float-tan 0.0)", 0.0),
        ("(float-tan (float* pi 0.25))", 1.0),
    ])
    def test_trigonometric_functions(self, menai, expression, expected_approx):
        """Test trigonometric functions."""
        result = menai.evaluate(expression)
        assert abs(result - expected_approx) < 1e-10

    def test_trigonometric_with_complex(self, menai):
        """Test trigonometric functions with complex arguments."""
        # sin(i) = i*sinh(1)
        result = menai.evaluate("(complex-sin 1j)")
        expected = 1j * math.sinh(1)
        assert abs(result - expected) < 1e-10

    @pytest.mark.parametrize("expression,expected_approx", [
        # Logarithmic functions
        ("(float-log e)", 1.0),
        ("(float-log 1.0)", 0.0),
        ("(float-exp 0.0)", 1.0),
        ("(float-exp 1.0)", math.e),
        ("(float-log10 10.0)", 1.0),
        ("(float-log10 100.0)", 2.0),
        ("(float-log10 1.0)", 0.0),
    ])
    def test_logarithmic_functions(self, menai, expression, expected_approx):
        """Test logarithmic and exponential functions."""
        result = menai.evaluate(expression)
        assert abs(result - expected_approx) < 1e-10

    def test_logarithmic_with_complex(self, menai):
        """Test logarithmic functions with complex arguments."""
        # log(-1) = i*pi
        result = menai.evaluate("(complex-log -1+0j)")
        expected = 1j * math.pi
        assert abs(result - expected) < 1e-10

    @pytest.mark.parametrize("expression,expected", [
        # Square root
        ("(float-sqrt 4.0)", "2.0"),  # sqrt returns float
        ("(float-sqrt 9.0)", "3.0"),  # sqrt returns float
        ("(float-sqrt 16.0)", "4.0"),  # sqrt returns float
        ("(float-sqrt 2.0)", str(math.sqrt(2))),

        # Square root of zero
        ("(float-sqrt 0.0)", "0.0"),  # sqrt returns float
    ])
    def test_sqrt_function(self, menai, expression, expected):
        """Test square root function."""
        result = menai.evaluate_and_format(expression)
        if expected == str(math.sqrt(2)):
            # Check approximately for irrational results
            actual = float(result)
            assert abs(actual - math.sqrt(2)) < 1e-10
        else:
            assert result == expected

    @pytest.mark.parametrize("expression,expected", [
        # Absolute value
        ("(float-abs 5.0)", "5.0"),
        ("(float-abs -5.0)", "5.0"),
        ("(float-abs 0.0)", "0.0"),
        ("(float-abs 3.14)", "3.14"),
        ("(float-abs -3.14)", "3.14"),

        # Complex absolute value (magnitude)
        ("(complex-abs (integer->complex 3 4))", "5.0"),  # |3+4i| = 5, abs of complex returns float
        ("(complex-abs 1j)", "1.0"),  # |i| = 1, abs of complex returns float
    ])
    def test_abs_function(self, menai, expression, expected):
        """Test absolute value function."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Round function
        ("(float-round 3.2)", "3.0"),
        ("(float-round 3.7)", "4.0"),
        ("(float-round 3.5)", "4.0"),  # Python rounds to even
        ("(float-round 2.5)", "2.0"),  # Python rounds to even
        ("(float-round -3.2)", "-3.0"),
        ("(float-round -3.7)", "-4.0"),

        # Floor function
        ("(float-floor 3.2)", "3.0"),
        ("(float-floor 3.7)", "3.0"),
        ("(float-floor -3.2)", "-4.0"),
        ("(float-floor -3.7)", "-4.0"),

        # Ceiling function
        ("(float-ceil 3.2)", "4.0"),
        ("(float-ceil 3.7)", "4.0"),
        ("(float-ceil -3.2)", "-3.0"),
        ("(float-ceil -3.7)", "-3.0"),
    ])
    def test_rounding_functions(self, menai, expression, expected):
        """Test rounding functions (round, floor, ceil)."""
        assert menai.evaluate_and_format(expression) == expected

    def test_rounding_functions_reject_complex(self, menai):
        """Test that rounding functions reject complex numbers."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(round (integer->complex 1 2))")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(floor 1j)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(ceil (integer->complex 3 4))")

    @pytest.mark.parametrize("expression,expected", [
        # Min function
        ("(integer-min 1)", "1"),
        ("(integer-min 3 1 4)", "1"),
        ("(integer-min 5 2 8 1 9)", "1"),
        ("(integer-min -3 -1 -5)", "-5"),
        ("(float-min 3.14 2.71)", "2.71"),

        # Max function
        ("(integer-max 1)", "1"),
        ("(integer-max 3 1 4)", "4"),
        ("(integer-max 5 2 8 1 9)", "9"),
        ("(integer-max -3 -1 -5)", "-1"),
        ("(float-max 3.14 2.71)", "3.14"),
    ])
    def test_min_max_functions(self, menai, expression, expected):
        """Test min and max functions."""
        assert menai.evaluate_and_format(expression) == expected

    def test_min_max_empty_args_error(self, menai):
        """Test that min/max with no arguments raises error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(min)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(max)")

    @pytest.mark.parametrize("expression,expected", [
        # Bitwise OR
        ("(integer-bit-or 5 3)", "7"),  # 101 | 011 = 111
        ("(integer-bit-or 1 2 4)", "7"),  # 001 | 010 | 100 = 111
        ("(integer-bit-or 0 0)", "0"),

        # Bitwise AND
        ("(integer-bit-and 5 3)", "1"),  # 101 & 011 = 001
        ("(integer-bit-and 7 3 1)", "1"),  # 111 & 011 & 001 = 001
        ("(integer-bit-and 5 2)", "0"),  # 101 & 010 = 000

        # Bitwise XOR
        ("(integer-bit-xor 5 3)", "6"),  # 101 ^ 011 = 110
        ("(integer-bit-xor 7 3 1)", "5"),  # ((111 ^ 011) ^ 001) = (100 ^ 001) = 101

        # Bitwise NOT
        ("(integer-bit-not 0)", "-1"),  # Two's complement
        ("(integer-bit-not -1)", "0"),
        ("(integer-bit-not 5)", "-6"),  # ~101 = ...11111010 = -6
    ])
    def test_bitwise_operations(self, menai, expression, expected):
        """Test bitwise operations."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Left shift
        ("(integer-bit-shift-left 1 3)", "8"),  # 1 << 3 = 8
        ("(integer-bit-shift-left 5 2)", "20"),  # 5 << 2 = 20
        ("(integer-bit-shift-left 0 5)", "0"),  # 0 << 5 = 0

        # Right shift
        ("(integer-bit-shift-right 8 3)", "1"),  # 8 >> 3 = 1
        ("(integer-bit-shift-right 20 2)", "5"),  # 20 >> 2 = 5
        ("(integer-bit-shift-right 0 5)", "0"),  # 0 >> 5 = 0
        ("(integer-bit-shift-right -8 2)", "-2"),  # Arithmetic right shift
    ])
    def test_bit_shift_operations(self, menai, expression, expected):
        """Test bit shift operations."""
        assert menai.evaluate_and_format(expression) == expected

    def test_bitwise_operations_require_integers(self, menai):
        """Test that bitwise operations require integer arguments."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer-bit-or 1.5 2)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer-bit-and 1 2.5)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer-bit-xor (integer->complex 1 2) 3)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer-bit-not 3.14)")

    @pytest.mark.parametrize("expression,expected", [
        # Complex number construction
        ("(float->complex 3.0 4.0)", "3+4j"),
        ("(float->complex 0.0 1.0)", "1j"),
        ("(float->complex 5.0)", "5+0j"),
        ("(float->complex -2.0 -3.0)", "-2-3j"),

        # Real part extraction
        ("(complex-real (integer->complex 3 4))", "3.0"),
        ("(complex-real 1j)", "0.0"),

        # Imaginary part extraction
        ("(complex-imag (integer->complex 3 4))", "4.0"),
        ("(complex-imag 1j)", "1.0"),
    ])
    def test_complex_number_functions(self, menai, expression, expected):
        """Test complex number construction and component extraction."""
        assert menai.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("number_format,expected", [
        # Hexadecimal literals
        ("#xFF", "255"),
        ("#x10", "16"),
        ("#xABC", "2748"),
        ("#xff", "255"),  # Lowercase

        # Binary literals
        ("#b1010", "10"),
        ("#b11111111", "255"),
        ("#B1010", "10"),  # Uppercase

        # Octal literals
        ("#o777", "511"),
        ("#o10", "8"),
        ("#O777", "511"),  # Uppercase

        # Scientific notation
        ("1e2", "100.0"),  # Scientific notation produces float
        ("1.5e2", "150.0"),  # Scientific notation produces float
        ("1E-2", "0.01"),
        ("2.5E+1", "25.0"),  # Scientific notation produces float
    ])
    def test_number_format_literals(self, menai, number_format, expected):
        """Test various number format literals."""
        assert menai.evaluate_and_format(number_format) == expected

    def test_arity_errors(self, menai):
        """Test that operators with fixed arity reject wrong argument counts."""
        # Binary operators
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(float// 1.0)")  # Floor division needs 2 args

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(float% 1)")  # Modulo needs 2 args

        # Unary operators
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(float-abs)")  # abs needs 1 arg

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer-abs 1 2)")  # abs takes only 1 arg

    def test_complex_trigonometric_edge_cases(self, menai):
        """Test trigonometric functions with pure imaginary numbers."""
        # Test tan with complex numbers to hit the complex branch
        result = menai.evaluate("(complex-tan 1j)")
        expected = cmath.tan(1j)
        assert abs(result - expected) < 1e-10

    def test_logarithm_negative_numbers_return_complex(self, menai):
        """Test that logarithms of negative numbers return complex results."""
        # Test log with negative real numbers
        result = menai.evaluate("(complex-log -2+0j)")
        expected = cmath.log(-2)
        assert abs(result - expected) < 1e-10

        # Test log10 with negative real numbers
        result = menai.evaluate("(complex-log10 -10+0j)")
        expected = cmath.log10(-10)
        assert abs(result - expected) < 1e-10

    def test_sqrt_negative_and_complex_numbers(self, menai):
        """Test sqrt with negative and complex numbers."""
        # Test sqrt with negative numbers
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(floor-sqrt -9.0)")

        # Test sqrt with complex numbers
        result = menai.evaluate("(complex-sqrt (integer->complex 0 4))")
        expected = cmath.sqrt(4j)
        assert abs(result - expected) < 1e-10

    def test_exponential_with_complex_numbers(self, menai):
        """Test exponential function with complex arguments."""
        # Test exp with complex numbers
        result = menai.evaluate("(complex-exp (integer->complex 1 2))")
        expected = cmath.exp(1+2j)
        assert abs(result - expected) < 1e-10

        # Test exp with pure imaginary
        result = menai.evaluate("(complex-exp 1j)")
        expected = cmath.exp(1j)
        assert abs(result - expected) < 1e-10

    def test_rounding_with_near_zero_complex_parts(self, menai):
        """Test rounding functions with complex numbers having tiny imaginary parts."""
        # Create a complex number with a very small but non-zero imaginary part
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(round (complex+ (float->complex 3.5 0.0) (complex* 1j (float->complex 1e-5 0.0))))")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(floor (complex+ (float->complex 2.7 0.0) (complex* 1j (float->complex 1e-8 0.0))))")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(ceil (complex+ (float->complex 4.1 0.0) (complex* 1j (float->complex 1e-6 0.0))))")

    def test_real_imag_with_integer_results(self, menai):
        """Test real/imag functions that return integers."""
        # Test cases where real/imag parts are whole numbers
        result = menai.evaluate("(complex-real (float->complex 5.0 3.0))")
        assert result == 5.0
        assert isinstance(result, float)

        result = menai.evaluate("(complex-imag (float->complex 2.0 7.0))")
        assert result == 7.0
        assert isinstance(result, float)
