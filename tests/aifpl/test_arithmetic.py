"""Tests for arithmetic operations and mathematical functions."""

import pytest
import math
import cmath

from aifpl import AIFPLEvalError


class TestArithmetic:
    """Test arithmetic operations and mathematical functions."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic addition
        ("(+ 1 2)", "3"),
        ("(+ 1 2 3)", "6"),
        ("(+ 1 2 3 4)", "10"),

        # Addition identity (empty)
        ("(+)", "0"),

        # Single argument
        ("(+ 5)", "5"),

        # Type promotion int -> float
        ("(+ 1 2.5)", "3.5"),
        ("(+ 1.5 2)", "3.5"),
        ("(+ 1.1 2.2)", "3.3000000000000003"),  # Floating point precision

        # Type promotion int/float -> complex
        ("(+ 1 j)", "1+1j"),
        ("(+ 2.5 j)", "2.5+1j"),
        ("(+ j 3)", "3+1j"),

        # Complex addition
        ("(+ (complex 1 2) (complex 3 4))", "4+6j"),

        # Negative numbers
        ("(+ -1 2)", "1"),
        ("(+ 1 -2)", "-1"),
        ("(+ -1 -2)", "-3"),
    ])
    def test_addition(self, aifpl, expression, expected):
        """Test addition operation with various argument types and counts."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic subtraction
        ("(- 5 3)", "2"),
        ("(- 10 3 2)", "5"),  # Left associative: ((10 - 3) - 2)

        # Unary minus
        ("(- 5)", "-5"),
        ("(- -3)", "3"),

        # Type promotion
        ("(- 5.5 2)", "3.5"),
        ("(- 5 2.5)", "2.5"),

        # Complex subtraction
        ("(- (complex 5 3) (complex 2 1))", "3+2j"),
        ("(- 5 j)", "5-1j"),

        # Multiple arguments
        ("(- 10 1 2 3)", "4"),  # ((((10 - 1) - 2) - 3)
    ])
    def test_subtraction(self, aifpl, expression, expected):
        """Test subtraction operation including unary minus."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic multiplication
        ("(* 2 3)", "6"),
        ("(* 2 3 4)", "24"),

        # Multiplication identity (empty)
        ("(*)", "1"),

        # Single argument
        ("(* 7)", "7"),

        # Type promotion
        ("(* 2 3.5)", "7.0"),  # int * float = float
        ("(* 2.5 4)", "10.0"),  # float * int = float

        # Complex multiplication
        ("(* 2 j)", "2j"),
        ("(* j j)", "-1+0j"),  # j*j = -1, simplifies to float when imag part is 0
        ("(* (complex 2 3) (complex 1 4))", "-10+11j"),

        # Zero multiplication
        ("(* 5 0)", "0"),
        ("(* 0 5)", "0"),

        # Negative numbers
        ("(* -2 3)", "-6"),
        ("(* 2 -3)", "-6"),
        ("(* -2 -3)", "6"),
    ])
    def test_multiplication(self, aifpl, expression, expected):
        """Test multiplication operation with various types."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic division
        ("(/ 6 2)", "3.0"),  # Division always returns float
        ("(/ 8 4)", "2.0"),  # Division always returns float
        ("(/ 7 2)", "3.5"),  # Integer division becomes float

        # Multiple arguments (left associative)
        ("(/ 24 2 3)", "4.0"),  # ((24 / 2) / 3) - division always returns float
        ("(/ 100 5 2)", "10.0"),  # ((100 / 5) / 2) - division always returns float

        # Type promotion
        ("(/ 5.0 2)", "2.5"),
        ("(/ 10 2.5)", "4.0"),  # Division always returns float

        # Complex division
        ("(/ (complex 4 2) (complex 1 1))", "3-1j"),
        ("(/ 6 j)", "-6j"),

        # Fraction results
        ("(/ 1 3)", "0.3333333333333333"),
    ])
    def test_division(self, aifpl, expression, expected):
        """Test division operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_division_by_zero(self, aifpl):
        """Test that division by zero raises appropriate error."""
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(/ 1 0)")

        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(/ 5 2 0)")

    @pytest.mark.parametrize("expression,expected", [
        # Floor division
        ("(// 7 2)", "3"),
        ("(// 8 3)", "2"),
        ("(// -7 2)", "-4"),  # Floor division rounds down
        ("(// 7 -2)", "-4"),
        ("(// -7 -2)", "3"),

        # Float inputs
        ("(// 7.5 2)", "3.0"),  # Floor division with float returns float
        ("(// 7 2.0)", "3.0"),  # Floor division with float returns float
    ])
    def test_floor_division(self, aifpl, expression, expected):
        """Test floor division operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_floor_division_by_zero(self, aifpl):
        """Test that floor division by zero raises error."""
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(// 1 0)")

    @pytest.mark.parametrize("expression,expected", [
        # Basic modulo
        ("(% 7 3)", "1"),
        ("(% 8 3)", "2"),
        ("(% 9 3)", "0"),

        # Negative numbers
        ("(% -7 3)", "2"),  # Python modulo behavior
        ("(% 7 -3)", "-2"),
        ("(% -7 -3)", "-1"),

        # Float inputs
        ("(% 7.5 3)", "1.5"),
        ("(% 7 3.0)", "1.0"),  # Modulo with float returns float
    ])
    def test_modulo(self, aifpl, expression, expected):
        """Test modulo operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_modulo_by_zero(self, aifpl):
        """Test that modulo by zero raises error."""
        with pytest.raises(AIFPLEvalError, match="Modulo by zero"):
            aifpl.evaluate("(% 1 0)")

    @pytest.mark.parametrize("expression,expected", [
        # Basic exponentiation
        ("(** 2 3)", "8"),
        ("(** 3 2)", "9"),
        ("(** 5 0)", "1"),
        ("(** 0 5)", "0"),

        # Negative exponents
        ("(** 2 -1)", "0.5"),
        ("(** 4 -2)", "0.0625"),

        # Float exponents
        ("(** 4 0.5)", "2.0"),  # Square root - returns float
        ("(** 8 0.3333333333333333)", "2"),  # Cube root (approximately)

        # Complex exponentiation
        ("(** j 2)", "-1+0j"),  # j^2 = -1, simplifies to float when imag part is 0
        ("(** (complex 1 1) 2)", "2j"),
    ])
    def test_exponentiation(self, aifpl, expression, expected):
        """Test exponentiation operation."""
        result = aifpl.evaluate_and_format(expression)
        if "0.3333333333333333" in expression:
            # For cube root, check approximately
            actual = float(result)
            assert abs(actual - 2.0) < 0.1
        else:
            assert result == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic pow (same as **)
        ("(pow 2 3)", "8"),
        ("(pow 3 2)", "9"),
        ("(pow 5 0)", "1"),
    ])
    def test_pow_function(self, aifpl, expression, expected):
        """Test pow function (alias for **)."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected_approx", [
        # Trigonometric functions
        ("(sin 0)", 0.0),
        ("(sin (* pi 0.5))", 1.0),
        ("(sin pi)", 0.0),
        ("(cos 0)", 1.0),
        ("(cos (* pi 0.5))", 0.0),
        ("(cos pi)", -1.0),
        ("(tan 0)", 0.0),
        ("(tan (* pi 0.25))", 1.0),
    ])
    def test_trigonometric_functions(self, aifpl, expression, expected_approx):
        """Test trigonometric functions."""
        result = aifpl.evaluate(expression)
        assert abs(result - expected_approx) < 1e-10

    def test_trigonometric_with_complex(self, aifpl):
        """Test trigonometric functions with complex arguments."""
        # sin(i) = i*sinh(1)
        result = aifpl.evaluate("(sin j)")
        expected = 1j * math.sinh(1)
        assert abs(result - expected) < 1e-10

    @pytest.mark.parametrize("expression,expected_approx", [
        # Logarithmic functions
        ("(log e)", 1.0),
        ("(log 1)", 0.0),
        ("(exp 0)", 1.0),
        ("(exp 1)", math.e),
        ("(log10 10)", 1.0),
        ("(log10 100)", 2.0),
        ("(log10 1)", 0.0),
    ])
    def test_logarithmic_functions(self, aifpl, expression, expected_approx):
        """Test logarithmic and exponential functions."""
        result = aifpl.evaluate(expression)
        assert abs(result - expected_approx) < 1e-10

    def test_logarithmic_with_complex(self, aifpl):
        """Test logarithmic functions with complex arguments."""
        # log(-1) = i*pi
        result = aifpl.evaluate("(log -1)")
        expected = 1j * math.pi
        assert abs(result - expected) < 1e-10

    @pytest.mark.parametrize("expression,expected", [
        # Square root
        ("(sqrt 4)", "2.0"),  # sqrt returns float
        ("(sqrt 9)", "3.0"),  # sqrt returns float
        ("(sqrt 16)", "4.0"),  # sqrt returns float
        ("(sqrt 2)", str(math.sqrt(2))),

        # Square root of zero
        ("(sqrt 0)", "0.0"),  # sqrt returns float
    ])
    def test_sqrt_function(self, aifpl, expression, expected):
        """Test square root function."""
        result = aifpl.evaluate_and_format(expression)
        if expected == str(math.sqrt(2)):
            # Check approximately for irrational results
            actual = float(result)
            assert abs(actual - math.sqrt(2)) < 1e-10
        else:
            assert result == expected

    def test_sqrt_negative_returns_complex(self, aifpl):
        """Test that sqrt of negative number returns complex result."""
        result = aifpl.evaluate("(sqrt -4)")
        assert isinstance(result, complex)
        assert result == 2j

    @pytest.mark.parametrize("expression,expected", [
        # Absolute value
        ("(abs 5)", "5"),
        ("(abs -5)", "5"),
        ("(abs 0)", "0"),
        ("(abs 3.14)", "3.14"),
        ("(abs -3.14)", "3.14"),

        # Complex absolute value (magnitude)
        ("(abs (complex 3 4))", "5.0"),  # |3+4i| = 5, abs of complex returns float
        ("(abs j)", "1.0"),  # |i| = 1, abs of complex returns float
    ])
    def test_abs_function(self, aifpl, expression, expected):
        """Test absolute value function."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Round function
        ("(round 3.2)", "3"),
        ("(round 3.7)", "4"),
        ("(round 3.5)", "4"),  # Python rounds to even
        ("(round 2.5)", "2"),  # Python rounds to even
        ("(round -3.2)", "-3"),
        ("(round -3.7)", "-4"),

        # Floor function
        ("(floor 3.2)", "3"),
        ("(floor 3.7)", "3"),
        ("(floor -3.2)", "-4"),
        ("(floor -3.7)", "-4"),

        # Ceiling function
        ("(ceil 3.2)", "4"),
        ("(ceil 3.7)", "4"),
        ("(ceil -3.2)", "-3"),
        ("(ceil -3.7)", "-3"),
    ])
    def test_rounding_functions(self, aifpl, expression, expected):
        """Test rounding functions (round, floor, ceil)."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_rounding_functions_reject_complex(self, aifpl):
        """Test that rounding functions reject complex numbers."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(round (complex 1 2))")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(floor j)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(ceil (complex 3 4))")

    @pytest.mark.parametrize("expression,expected", [
        # Min function
        ("(min 1)", "1"),
        ("(min 3 1 4)", "1"),
        ("(min 5 2 8 1 9)", "1"),
        ("(min -3 -1 -5)", "-5"),
        ("(min 3.14 2.71)", "2.71"),

        # Max function
        ("(max 1)", "1"),
        ("(max 3 1 4)", "4"),
        ("(max 5 2 8 1 9)", "9"),
        ("(max -3 -1 -5)", "-1"),
        ("(max 3.14 2.71)", "3.14"),
    ])
    def test_min_max_functions(self, aifpl, expression, expected):
        """Test min and max functions."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_min_max_empty_args_error(self, aifpl):
        """Test that min/max with no arguments raises error."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(min)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(max)")

    @pytest.mark.parametrize("expression,expected", [
        # Bitwise OR
        ("(bit-or 5 3)", "7"),  # 101 | 011 = 111
        ("(bit-or 1 2 4)", "7"),  # 001 | 010 | 100 = 111
        ("(bit-or 0 0)", "0"),

        # Bitwise AND
        ("(bit-and 5 3)", "1"),  # 101 & 011 = 001
        ("(bit-and 7 3 1)", "1"),  # 111 & 011 & 001 = 001
        ("(bit-and 5 2)", "0"),  # 101 & 010 = 000

        # Bitwise XOR
        ("(bit-xor 5 3)", "6"),  # 101 ^ 011 = 110
        ("(bit-xor 7 3 1)", "5"),  # ((111 ^ 011) ^ 001) = (100 ^ 001) = 101

        # Bitwise NOT
        ("(bit-not 0)", "-1"),  # Two's complement
        ("(bit-not -1)", "0"),
        ("(bit-not 5)", "-6"),  # ~101 = ...11111010 = -6
    ])
    def test_bitwise_operations(self, aifpl, expression, expected):
        """Test bitwise operations."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Left shift
        ("(bit-shift-left 1 3)", "8"),  # 1 << 3 = 8
        ("(bit-shift-left 5 2)", "20"),  # 5 << 2 = 20
        ("(bit-shift-left 0 5)", "0"),  # 0 << 5 = 0

        # Right shift
        ("(bit-shift-right 8 3)", "1"),  # 8 >> 3 = 1
        ("(bit-shift-right 20 2)", "5"),  # 20 >> 2 = 5
        ("(bit-shift-right 0 5)", "0"),  # 0 >> 5 = 0
        ("(bit-shift-right -8 2)", "-2"),  # Arithmetic right shift
    ])
    def test_bit_shift_operations(self, aifpl, expression, expected):
        """Test bit shift operations."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_bitwise_operations_require_integers(self, aifpl):
        """Test that bitwise operations require integer arguments."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bit-or 1.5 2)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bit-and 1 2.5)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bit-xor (complex 1 2) 3)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bit-not 3.14)")

    @pytest.mark.parametrize("expression,expected", [
        # Binary conversion
        ("(bin 0)", '"0b0"'),
        ("(bin 5)", '"0b101"'),
        ("(bin 255)", '"0b11111111"'),
        ("(bin -1)", '"-0b1"'),

        # Hexadecimal conversion
        ("(hex 0)", '"0x0"'),
        ("(hex 15)", '"0xf"'),
        ("(hex 255)", '"0xff"'),
        ("(hex -1)", '"-0x1"'),

        # Octal conversion
        ("(oct 0)", '"0o0"'),
        ("(oct 8)", '"0o10"'),
        ("(oct 64)", '"0o100"'),
        ("(oct -1)", '"-0o1"'),
    ])
    def test_base_conversion_functions(self, aifpl, expression, expected):
        """Test base conversion functions (bin, hex, oct)."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_base_conversion_requires_integers(self, aifpl):
        """Test that base conversion functions require integer arguments."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bin 3.14)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(hex 2.5)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(oct (complex 1 2))")

    @pytest.mark.parametrize("expression,expected", [
        # Complex number construction
        ("(complex 3 4)", "3+4j"),
        ("(complex 0 1)", "1j"),
        ("(complex 5 0)", "5+0j"),
        ("(complex -2 -3)", "-2-3j"),

        # Real part extraction
        ("(real (complex 3 4))", "3"),
        ("(real 42)", "42"),
        ("(real 3.14)", "3.14"),
        ("(real j)", "0"),

        # Imaginary part extraction
        ("(imag (complex 3 4))", "4"),
        ("(imag 42)", "0"),
        ("(imag 3.14)", "0"),
        ("(imag j)", "1"),
    ])
    def test_complex_number_functions(self, aifpl, expression, expected):
        """Test complex number construction and component extraction."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("number_format,expected", [
        # Hexadecimal literals
        ("0xFF", "255"),
        ("0x10", "16"),
        ("0xABC", "2748"),
        ("0xff", "255"),  # Lowercase

        # Binary literals
        ("0b1010", "10"),
        ("0b11111111", "255"),
        ("0B1010", "10"),  # Uppercase

        # Octal literals
        ("0o777", "511"),
        ("0o10", "8"),
        ("0O777", "511"),  # Uppercase

        # Scientific notation
        ("1e2", "100.0"),  # Scientific notation produces float
        ("1.5e2", "150.0"),  # Scientific notation produces float
        ("1E-2", "0.01"),
        ("2.5E+1", "25.0"),  # Scientific notation produces float
    ])
    def test_number_format_literals(self, aifpl, number_format, expected):
        """Test various number format literals."""
        assert aifpl.evaluate_and_format(number_format) == expected

    def test_arithmetic_type_errors(self, aifpl):
        """Test that arithmetic operations reject non-numeric types."""
        # String arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(+ 1 "hello")')

        # Boolean arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(* 2 #t)")

        # List arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(- 5 (list 1 2))")

    def test_arity_errors(self, aifpl):
        """Test that operators with fixed arity reject wrong argument counts."""
        # Binary operators
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(// 1)")  # Floor division needs 2 args

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(% 1)")  # Modulo needs 2 args

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(** 1)")  # Exponentiation needs 2 args

        # Unary operators
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(abs)")  # abs needs 1 arg

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(abs 1 2)")  # abs takes only 1 arg

    def test_division_minimum_args(self, aifpl):
        """Test that division requires at least 2 arguments."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(/)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(/ 5)")

    def test_subtraction_minimum_args(self, aifpl):
        """Test that subtraction requires at least 1 argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(-)")

    # ========== Additional Coverage Tests ==========

    def test_division_argument_position_specific_errors(self, aifpl):
        """Test division by zero with specific argument positions."""
        # Test division by zero at different argument positions
        with pytest.raises(AIFPLEvalError, match="Division by zero at argument 2"):
            aifpl.evaluate("(/ 10 0)")

        with pytest.raises(AIFPLEvalError, match="Division by zero at argument 3"):
            aifpl.evaluate("(/ 10 2 0)")

        with pytest.raises(AIFPLEvalError, match="Division by zero at argument 4"):
            aifpl.evaluate("(/ 24 2 3 0)")

    def test_complex_trigonometric_edge_cases(self, aifpl):
        """Test trigonometric functions with pure imaginary numbers."""
        # Test tan with complex numbers to hit the complex branch
        result = aifpl.evaluate("(tan j)")
        expected = cmath.tan(1j)
        assert abs(result - expected) < 1e-10

    def test_logarithm_negative_numbers_return_complex(self, aifpl):
        """Test that logarithms of negative numbers return complex results."""
        # Test log with negative real numbers
        result = aifpl.evaluate("(log -2)")
        expected = cmath.log(-2)
        assert abs(result - expected) < 1e-10

        # Test log10 with negative real numbers
        result = aifpl.evaluate("(log10 -10)")
        expected = cmath.log10(-10)
        assert abs(result - expected) < 1e-10

    def test_sqrt_negative_and_complex_numbers(self, aifpl):
        """Test sqrt with negative and complex numbers."""
        # Test sqrt with negative numbers (returns complex)
        result = aifpl.evaluate("(sqrt -9)")
        expected = cmath.sqrt(-9)
        assert abs(result - expected) < 1e-10

        # Test sqrt with complex numbers
        result = aifpl.evaluate("(sqrt (complex 0 4))")
        expected = cmath.sqrt(4j)
        assert abs(result - expected) < 1e-10

    def test_exponential_with_complex_numbers(self, aifpl):
        """Test exponential function with complex arguments."""
        # Test exp with complex numbers
        result = aifpl.evaluate("(exp (complex 1 2))")
        expected = cmath.exp(1+2j)
        assert abs(result - expected) < 1e-10

        # Test exp with pure imaginary
        result = aifpl.evaluate("(exp j)")
        expected = cmath.exp(1j)
        assert abs(result - expected) < 1e-10

    def test_rounding_with_near_zero_complex_parts(self, aifpl):
        """Test rounding functions with complex numbers having tiny imaginary parts."""
        # This should test the tolerance checking in rounding functions
        # Create a complex number with a very small but non-zero imaginary part
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(round (+ 3.5 (* j 1e-5)))")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(floor (+ 2.7 (* j 1e-8)))")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(ceil (+ 4.1 (* j 1e-6)))")

    def test_real_imag_with_integer_results(self, aifpl):
        """Test real/imag functions that return integers."""
        # Test cases where real/imag parts are whole numbers
        result = aifpl.evaluate("(real (complex 5.0 3.0))")
        assert result == 5
        assert isinstance(result, int)

        result = aifpl.evaluate("(imag (complex 2.0 7.0))")
        assert result == 7
        assert isinstance(result, int)
