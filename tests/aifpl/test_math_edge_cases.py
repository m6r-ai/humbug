"""Tests for AIFPL mathematical operations edge cases."""

import math
import cmath
import pytest

from aifpl import AIFPLEvalError


class TestAIFPLMathEdgeCases:
    """Test mathematical operation edge cases and boundary conditions."""

    def test_division_edge_cases(self, aifpl):
        """Test division edge cases beyond basic division by zero."""
        # Division by very small numbers
        result = aifpl.evaluate("(/ 1 0.001)")
        assert abs(result - 1000.0) < 1e-10

        result = aifpl.evaluate("(/ 1 1e-10)")
        assert abs(result - 1e10) < 1e-5

        # Division resulting in very small numbers
        result = aifpl.evaluate("(/ 1e-10 1)")
        assert abs(result - 1e-10) < 1e-15

        result = aifpl.evaluate("(/ 1 1000000)")
        assert abs(result - 1e-6) < 1e-12

        # Integer vs float division
        result = aifpl.evaluate("(/ 5 2)")
        assert result == 2.5

        result = aifpl.evaluate("(// 5 2)")
        assert result == 2

        result = aifpl.evaluate("(% 5 2)")
        assert result == 1

        # Negative number division
        result = aifpl.evaluate("(/ -10 3)")
        assert abs(result - (-10/3)) < 1e-10

        result = aifpl.evaluate("(// -10 3)")
        assert result == -4  # Floor division

        result = aifpl.evaluate("(% -10 3)")
        assert result == 2  # Python modulo behavior

    def test_division_by_zero_comprehensive(self, aifpl):
        """Test comprehensive division by zero scenarios."""
        # Basic division by zero
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(/ 1 0)")

        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(/ 5 2 0)")

        # Floor division by zero
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(// 1 0)")

        # Modulo by zero
        with pytest.raises(AIFPLEvalError, match="Modulo by zero"):
            aifpl.evaluate("(% 1 0)")

        # Division by zero in complex expressions
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(+ 1 (/ 2 0))")

        # Division by expression that evaluates to zero
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(/ 5 (- 3 3))")

    def test_power_and_exponentiation_edge_cases(self, aifpl):
        """Test power and exponentiation edge cases."""
        # Special power cases
        assert aifpl.evaluate("(** 0 0)") == 1  # 0^0 = 1 by convention
        assert aifpl.evaluate("(** 1 1000)") == 1  # 1^anything = 1
        assert aifpl.evaluate("(** 2 0)") == 1  # anything^0 = 1
        assert aifpl.evaluate("(** -1 2)") == 1  # (-1)^even = 1
        assert aifpl.evaluate("(** -1 3)") == -1  # (-1)^odd = -1

        # Negative exponents
        result = aifpl.evaluate("(** 2 -1)")
        assert abs(result - 0.5) < 1e-10

        result = aifpl.evaluate("(** 4 -2)")
        assert abs(result - 0.0625) < 1e-10

        # Fractional exponents (roots)
        result = aifpl.evaluate("(** 4 0.5)")
        assert abs(result - 2.0) < 1e-10

        result = aifpl.evaluate("(** 8 0.3333333333333333)")
        assert abs(result - 2.0) < 0.1  # Cube root approximation

        # Large exponents
        result = aifpl.evaluate("(** 10 10)")
        assert result == 10000000000

        # Complex exponentiation
        result = aifpl.evaluate("(** j 2)")
        assert abs(result - (-1)) < 1e-10

        result = aifpl.evaluate("(** (complex 1 1) 2)")
        expected = (1+1j)**2
        assert abs(result - expected) < 1e-10

    def test_root_operations_edge_cases(self, aifpl):
        """Test root operations edge cases."""
        # Basic square roots
        assert aifpl.evaluate("(sqrt 0)") == 0
        assert aifpl.evaluate("(sqrt 1)") == 1
        assert aifpl.evaluate("(sqrt 4)") == 2
        assert aifpl.evaluate("(sqrt 9)") == 3

        # Irrational roots
        result = aifpl.evaluate("(sqrt 2)")
        assert abs(result - math.sqrt(2)) < 1e-10

        result = aifpl.evaluate("(sqrt 3)")
        assert abs(result - math.sqrt(3)) < 1e-10

        # Square root of negative numbers (should return complex)
        result = aifpl.evaluate("(sqrt -4)")
        assert isinstance(result, complex)
        assert abs(result - 2j) < 1e-10

        result = aifpl.evaluate("(sqrt -1)")
        assert isinstance(result, complex)
        assert abs(result - 1j) < 1e-10

        # Square root of complex numbers
        result = aifpl.evaluate("(sqrt (complex 3 4))")
        expected = cmath.sqrt(3+4j)
        assert abs(result - expected) < 1e-10

    def test_trigonometric_edge_cases(self, aifpl):
        """Test trigonometric function edge cases."""
        # Special angle values
        result = aifpl.evaluate("(sin 0)")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(sin (* pi 0.5))")
        assert abs(result - 1) < 1e-10

        result = aifpl.evaluate("(sin pi)")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(cos 0)")
        assert abs(result - 1) < 1e-10

        result = aifpl.evaluate("(cos (* pi 0.5))")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(cos pi)")
        assert abs(result - (-1)) < 1e-10

        result = aifpl.evaluate("(tan 0)")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(tan (* pi 0.25))")
        assert abs(result - 1) < 1e-10

        # Negative angles
        result = aifpl.evaluate("(sin (* -1 pi))")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(cos (* -1 pi))")
        assert abs(result - (-1)) < 1e-10

        # Very small angles (sin(x) ≈ x for small x)
        result = aifpl.evaluate("(sin 0.001)")
        assert abs(result - 0.001) < 1e-6

        # Large angles (periodicity)
        result = aifpl.evaluate("(sin (* 2 pi))")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(cos (* 2 pi))")
        assert abs(result - 1) < 1e-10

    def test_trigonometric_with_complex_numbers(self, aifpl):
        """Test trigonometric functions with complex arguments."""
        # sin(i) = i*sinh(1)
        result = aifpl.evaluate("(sin j)")
        expected = 1j * math.sinh(1)
        assert abs(result - expected) < 1e-10

        # cos(i) = cosh(1)
        result = aifpl.evaluate("(cos j)")
        expected = math.cosh(1)
        assert abs(result - expected) < 1e-10

        # Complex trigonometric identities
        # sin^2 + cos^2 = 1 should hold for complex numbers too
        z = 1 + 2j
        result_sin = aifpl.evaluate("(sin (complex 1 2))")
        result_cos = aifpl.evaluate("(cos (complex 1 2))")
        identity_result = result_sin**2 + result_cos**2
        assert abs(identity_result - 1) < 1e-10

    def test_logarithmic_edge_cases(self, aifpl):
        """Test logarithmic function edge cases."""
        # Natural logarithm edge cases
        result = aifpl.evaluate("(log 1)")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(log e)")
        assert abs(result - 1) < 1e-10

        # Base-10 logarithm edge cases
        result = aifpl.evaluate("(log10 1)")
        assert abs(result - 0) < 1e-10

        result = aifpl.evaluate("(log10 10)")
        assert abs(result - 1) < 1e-10

        result = aifpl.evaluate("(log10 100)")
        assert abs(result - 2) < 1e-10

        result = aifpl.evaluate("(log10 1000)")
        assert abs(result - 3) < 1e-10

        # Logarithm of numbers less than 1
        result = aifpl.evaluate("(log 0.5)")
        assert abs(result - math.log(0.5)) < 1e-10

        result = aifpl.evaluate("(log10 0.1)")
        assert abs(result - (-1)) < 1e-10

        # Logarithm of very small positive numbers
        result = aifpl.evaluate("(log 1e-10)")
        assert abs(result - math.log(1e-10)) < 1e-10

    def test_logarithmic_with_complex_numbers(self, aifpl):
        """Test logarithmic functions with complex arguments."""
        # log(-1) = i*pi
        result = aifpl.evaluate("(log -1)")
        expected = 1j * math.pi
        assert abs(result - expected) < 1e-10

        # log(i) = i*pi/2
        result = aifpl.evaluate("(log j)")
        expected = 1j * math.pi / 2
        assert abs(result - expected) < 1e-10

        # log of complex number
        result = aifpl.evaluate("(log (complex 1 1))")
        expected = cmath.log(1+1j)
        assert abs(result - expected) < 1e-10

    def test_logarithmic_domain_errors(self, aifpl):
        """Test logarithmic function domain errors."""
        # Logarithm of zero should raise error or return -inf
        try:
            result = aifpl.evaluate("(log 0)")
            # If it doesn't raise an error, it should be -inf
            assert math.isinf(result) and result < 0
        except AIFPLEvalError:
            # Error is also acceptable
            pass

        try:
            result = aifpl.evaluate("(log10 0)")
            assert math.isinf(result) and result < 0
        except AIFPLEvalError:
            pass

        # Logarithm of negative real numbers (should return complex or error)
        try:
            result = aifpl.evaluate("(log -2)")
            # Should either be complex or raise error
            if not isinstance(result, complex):
                pytest.fail("log of negative number should return complex")
        except AIFPLEvalError:
            # Error is also acceptable for real-only implementations
            pass

    def test_exponential_edge_cases(self, aifpl):
        """Test exponential function edge cases."""
        # Basic exponential cases
        result = aifpl.evaluate("(exp 0)")
        assert abs(result - 1) < 1e-10

        result = aifpl.evaluate("(exp 1)")
        assert abs(result - math.e) < 1e-10

        # Negative exponents
        result = aifpl.evaluate("(exp -1)")
        assert abs(result - (1/math.e)) < 1e-10

        # Large exponents
        result = aifpl.evaluate("(exp 10)")
        assert abs(result - math.exp(10)) < 1e-5

        # Very small exponents
        result = aifpl.evaluate("(exp -10)")
        assert abs(result - math.exp(-10)) < 1e-15

        # exp(i*pi) = -1 (Euler's identity)
        result = aifpl.evaluate("(exp (* j pi))")
        assert abs(result - (-1)) < 1e-10

        # exp(i*pi/2) = i
        result = aifpl.evaluate("(exp (* j pi 0.5))")
        assert abs(result - 1j) < 1e-10

    def test_absolute_value_edge_cases(self, aifpl):
        """Test absolute value edge cases."""
        # Basic absolute values
        assert aifpl.evaluate("(abs 5)") == 5
        assert aifpl.evaluate("(abs -5)") == 5
        assert aifpl.evaluate("(abs 0)") == 0
        assert aifpl.evaluate("(abs 3.14)") == 3.14
        assert aifpl.evaluate("(abs -3.14)") == 3.14

        # Complex absolute value (magnitude)
        result = aifpl.evaluate("(abs (complex 3 4))")
        assert abs(result - 5) < 1e-10  # |3+4i| = 5

        result = aifpl.evaluate("(abs j)")
        assert abs(result - 1) < 1e-10  # |i| = 1

        result = aifpl.evaluate("(abs (complex -3 -4))")
        assert abs(result - 5) < 1e-10  # |-3-4i| = 5

        # Very small numbers
        result = aifpl.evaluate("(abs -1e-100)")
        assert result == 1e-100

        # Very large numbers
        result = aifpl.evaluate("(abs -1e100)")
        assert result == 1e100

    def test_rounding_functions_edge_cases(self, aifpl):
        """Test rounding function edge cases."""
        # Round function edge cases
        assert aifpl.evaluate("(round 3.2)") == 3
        assert aifpl.evaluate("(round 3.7)") == 4
        assert aifpl.evaluate("(round 3.5)") == 4  # Python rounds to even
        assert aifpl.evaluate("(round 2.5)") == 2  # Python rounds to even
        assert aifpl.evaluate("(round -3.2)") == -3
        assert aifpl.evaluate("(round -3.7)") == -4
        assert aifpl.evaluate("(round -3.5)") == -4  # Python rounds to even
        assert aifpl.evaluate("(round -2.5)") == -2  # Python rounds to even

        # Floor function edge cases
        assert aifpl.evaluate("(floor 3.0)") == 3
        assert aifpl.evaluate("(floor 3.2)") == 3
        assert aifpl.evaluate("(floor 3.7)") == 3
        assert aifpl.evaluate("(floor -3.2)") == -4
        assert aifpl.evaluate("(floor -3.7)") == -4
        assert aifpl.evaluate("(floor 0.0)") == 0
        assert aifpl.evaluate("(floor -0.1)") == -1

        # Ceiling function edge cases
        assert aifpl.evaluate("(ceil 3.0)") == 3
        assert aifpl.evaluate("(ceil 3.2)") == 4
        assert aifpl.evaluate("(ceil 3.7)") == 4
        assert aifpl.evaluate("(ceil -3.2)") == -3
        assert aifpl.evaluate("(ceil -3.7)") == -3
        assert aifpl.evaluate("(ceil 0.0)") == 0
        assert aifpl.evaluate("(ceil 0.1)") == 1

        # Very small numbers
        assert aifpl.evaluate("(round 1e-10)") == 0
        assert aifpl.evaluate("(floor 1e-10)") == 0
        assert aifpl.evaluate("(ceil 1e-10)") == 1
        assert aifpl.evaluate("(ceil -1e-10)") == 0
        assert aifpl.evaluate("(floor -1e-10)") == -1

    def test_rounding_functions_reject_complex(self, aifpl):
        """Test that rounding functions reject complex numbers."""
        complex_values = [
            "(complex 1 2)",
            "j",
            "(complex 3 4)",
            "(+ 5 j)",
        ]

        for value in complex_values:
            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(round {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(floor {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(ceil {value})")

    def test_min_max_edge_cases(self, aifpl):
        """Test min/max function edge cases."""
        # Single argument
        assert aifpl.evaluate("(min 42)") == 42
        assert aifpl.evaluate("(max 42)") == 42

        # Multiple arguments
        assert aifpl.evaluate("(min 3 1 4 1 5)") == 1
        assert aifpl.evaluate("(max 3 1 4 1 5)") == 5

        # Negative numbers
        assert aifpl.evaluate("(min -3 -1 -5)") == -5
        assert aifpl.evaluate("(max -3 -1 -5)") == -1

        # Mixed positive/negative
        assert aifpl.evaluate("(min -2 0 3)") == -2
        assert aifpl.evaluate("(max -2 0 3)") == 3

        # Floating point numbers
        result = aifpl.evaluate("(min 3.14 2.71 3.16)")
        assert abs(result - 2.71) < 1e-10

        result = aifpl.evaluate("(max 3.14 2.71 3.16)")
        assert abs(result - 3.16) < 1e-10

        # Very small differences
        result = aifpl.evaluate("(min 1.0000001 1.0000002)")
        assert abs(result - 1.0000001) < 1e-10

        # Very large numbers
        result = aifpl.evaluate("(min 1e100 2e100)")
        assert result == 1e100

        result = aifpl.evaluate("(max 1e100 2e100)")
        assert result == 2e100

    def test_min_max_empty_args_error(self, aifpl):
        """Test that min/max with no arguments raises error."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(min)")

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(max)")

    def test_bitwise_operations_edge_cases(self, aifpl):
        """Test bitwise operations edge cases."""
        # Basic bitwise operations
        assert aifpl.evaluate("(bit-or 5 3)") == 7  # 101 | 011 = 111
        assert aifpl.evaluate("(bit-and 5 3)") == 1  # 101 & 011 = 001
        assert aifpl.evaluate("(bit-xor 5 3)") == 6  # 101 ^ 011 = 110

        # Bitwise NOT edge cases
        assert aifpl.evaluate("(bit-not 0)") == -1  # Two's complement
        assert aifpl.evaluate("(bit-not -1)") == 0
        assert aifpl.evaluate("(bit-not 5)") == -6  # ~101 = ...11111010 = -6

        # Operations with zero
        assert aifpl.evaluate("(bit-or 0 0)") == 0
        assert aifpl.evaluate("(bit-and 0 0)") == 0
        assert aifpl.evaluate("(bit-xor 0 0)") == 0
        assert aifpl.evaluate("(bit-or 5 0)") == 5
        assert aifpl.evaluate("(bit-and 5 0)") == 0
        assert aifpl.evaluate("(bit-xor 5 0)") == 5

        # Operations with all bits set
        assert aifpl.evaluate("(bit-and 255 255)") == 255
        assert aifpl.evaluate("(bit-or 255 255)") == 255
        assert aifpl.evaluate("(bit-xor 255 255)") == 0

        # Multiple arguments
        assert aifpl.evaluate("(bit-or 1 2 4)") == 7  # 001 | 010 | 100 = 111
        assert aifpl.evaluate("(bit-and 7 3 1)") == 1  # 111 & 011 & 001 = 001
        assert aifpl.evaluate("(bit-xor 7 3 1)") == 5  # ((111 ^ 011) ^ 001) = (100 ^ 001) = 101

        # Negative numbers (two's complement)
        assert aifpl.evaluate("(bit-and -1 5)") == 5  # -1 has all bits set
        assert aifpl.evaluate("(bit-or -1 5)") == -1  # -1 has all bits set

    def test_bit_shift_operations_edge_cases(self, aifpl):
        """Test bit shift operations edge cases."""
        # Left shift operations
        assert aifpl.evaluate("(bit-shift-left 1 0)") == 1  # No shift
        assert aifpl.evaluate("(bit-shift-left 1 1)") == 2  # 1 << 1 = 2
        assert aifpl.evaluate("(bit-shift-left 1 3)") == 8  # 1 << 3 = 8
        assert aifpl.evaluate("(bit-shift-left 5 2)") == 20  # 5 << 2 = 20
        assert aifpl.evaluate("(bit-shift-left 0 5)") == 0  # 0 << 5 = 0

        # Right shift operations
        assert aifpl.evaluate("(bit-shift-right 8 0)") == 8  # No shift
        assert aifpl.evaluate("(bit-shift-right 8 1)") == 4  # 8 >> 1 = 4
        assert aifpl.evaluate("(bit-shift-right 8 3)") == 1  # 8 >> 3 = 1
        assert aifpl.evaluate("(bit-shift-right 20 2)") == 5  # 20 >> 2 = 5
        assert aifpl.evaluate("(bit-shift-right 0 5)") == 0  # 0 >> 5 = 0

        # Arithmetic right shift with negative numbers
        assert aifpl.evaluate("(bit-shift-right -8 2)") == -2  # Arithmetic right shift
        assert aifpl.evaluate("(bit-shift-right -1 1)") == -1  # -1 >> 1 = -1

        # Large shifts
        assert aifpl.evaluate("(bit-shift-left 1 10)") == 1024  # 1 << 10 = 1024
        assert aifpl.evaluate("(bit-shift-right 1024 10)") == 1  # 1024 >> 10 = 1

    def test_bitwise_operations_require_integers(self, aifpl):
        """Test that bitwise operations require integer arguments."""
        non_integer_values = ["1.5", "2.5", "(complex 1 2)", "3.14"]

        for value in non_integer_values:
            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(bit-or {value} 2)")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(bit-and 1 {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(bit-xor {value} 3)")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(bit-not {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(bit-shift-left {value} 2)")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(bit-shift-right 8 {value})")

    def test_base_conversion_edge_cases(self, aifpl):
        """Test base conversion edge cases."""
        # Binary conversion edge cases
        assert aifpl.evaluate('(bin 0)') == '0b0'
        assert aifpl.evaluate('(bin 1)') == '0b1'
        assert aifpl.evaluate('(bin 5)') == '0b101'
        assert aifpl.evaluate('(bin 255)') == '0b11111111'
        assert aifpl.evaluate('(bin -1)') == '-0b1'
        assert aifpl.evaluate('(bin -5)') == '-0b101'

        # Hexadecimal conversion edge cases
        assert aifpl.evaluate('(hex 0)') == '0x0'
        assert aifpl.evaluate('(hex 10)') == '0xa'
        assert aifpl.evaluate('(hex 15)') == '0xf'
        assert aifpl.evaluate('(hex 16)') == '0x10'
        assert aifpl.evaluate('(hex 255)') == '0xff'
        assert aifpl.evaluate('(hex -1)') == '-0x1'
        assert aifpl.evaluate('(hex -15)') == '-0xf'

        # Octal conversion edge cases
        assert aifpl.evaluate('(oct 0)') == '0o0'
        assert aifpl.evaluate('(oct 7)') == '0o7'
        assert aifpl.evaluate('(oct 8)') == '0o10'
        assert aifpl.evaluate('(oct 64)') == '0o100'
        assert aifpl.evaluate('(oct -1)') == '-0o1'
        assert aifpl.evaluate('(oct -8)') == '-0o10'

        # Large numbers
        assert aifpl.evaluate('(bin 1024)') == '0b10000000000'
        assert aifpl.evaluate('(hex 4096)') == '0x1000'
        assert aifpl.evaluate('(oct 512)') == '0o1000'

    def test_base_conversion_requires_integers(self, aifpl):
        """Test that base conversion functions require integer arguments."""
        non_integer_values = ["3.14", "2.5", "(complex 1 2)", "1.0"]

        for value in non_integer_values:
            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(bin {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(hex {value})")

            with pytest.raises(AIFPLEvalError):
                aifpl.evaluate(f"(oct {value})")

    def test_complex_number_operations_edge_cases(self, aifpl):
        """Test complex number operations edge cases."""
        # Complex number construction edge cases
        result = aifpl.evaluate("(complex 0 0)")
        assert result == 0+0j

        # Pure real complex (should simplify)
        result = aifpl.evaluate("(complex 5 0)")
        assert result == 5
        assert isinstance(result, (int, float))

        # Pure imaginary complex
        result = aifpl.evaluate("(complex 0 3)")
        assert result == 3j

        # Real/imaginary part extraction edge cases
        assert aifpl.evaluate("(real (complex 3 4))") == 3
        assert aifpl.evaluate("(imag (complex 3 4))") == 4
        assert aifpl.evaluate("(real 42)") == 42
        assert aifpl.evaluate("(imag 42)") == 0
        assert aifpl.evaluate("(real 3.14)") == 3.14
        assert aifpl.evaluate("(imag 3.14)") == 0
        assert aifpl.evaluate("(real j)") == 0
        assert aifpl.evaluate("(imag j)") == 1

        # Complex arithmetic edge cases
        result = aifpl.evaluate("(+ (complex 1 2) (complex 3 4))")
        assert result == 4+6j

        result = aifpl.evaluate("(* (complex 1 2) (complex 3 4))")
        assert result == (1+2j)*(3+4j)

        result = aifpl.evaluate("(/ (complex 4 2) (complex 1 1))")
        expected = (4+2j)/(1+1j)
        assert abs(result - expected) < 1e-10

    def test_mathematical_constants_edge_cases(self, aifpl):
        """Test mathematical constants edge cases."""
        # Pi constant
        pi_value = aifpl.evaluate("pi")
        assert abs(pi_value - math.pi) < 1e-10

        # E constant
        e_value = aifpl.evaluate("e")
        assert abs(e_value - math.e) < 1e-10

        # Imaginary unit
        j_value = aifpl.evaluate("j")
        assert j_value == 1j

        # Use constants in expressions
        result = aifpl.evaluate("(* 2 pi)")
        assert abs(result - (2 * math.pi)) < 1e-10

        result = aifpl.evaluate("(** e 2)")
        assert abs(result - (math.e ** 2)) < 1e-10

        result = aifpl.evaluate("(* j j)")
        assert abs(result - (-1)) < 1e-10

    def test_arithmetic_type_coercion_edge_cases(self, aifpl):
        """Test arithmetic type coercion edge cases."""
        # Integer to float coercion
        result = aifpl.evaluate("(+ 1 2.5)")
        assert result == 3.5
        assert isinstance(result, float)

        result = aifpl.evaluate("(* 2 3.0)")
        assert result == 6
        assert isinstance(result, int)

        # Float to complex coercion
        result = aifpl.evaluate("(+ 2.5 j)")
        assert result == 2.5+1j
        assert isinstance(result, complex)

        # Integer to complex coercion
        result = aifpl.evaluate("(+ 1 j)")
        assert result == 1+1j
        assert isinstance(result, complex)

        # Mixed operations
        result = aifpl.evaluate("(+ 1 2.5 j)")
        assert result == 3.5+1j
        assert isinstance(result, complex)

    def test_infinity_and_nan_edge_cases(self, aifpl):
        """Test handling of infinity and NaN values."""
        # Test operations that might produce infinity
        try:
            # Very large exponentiation
            result = aifpl.evaluate("(** 10 1000)")
            # This might be infinity or a very large number
            if math.isinf(result):
                assert result > 0  # Should be positive infinity
        except (OverflowError, AIFPLEvalError):
            # Overflow errors are also acceptable
            pass

        # Test operations with very large numbers
        try:
            result = aifpl.evaluate("(* 1e100 1e100)")
            if math.isinf(result):
                assert result > 0
            else:
                assert result == 1e200
        except (OverflowError, AIFPLEvalError):
            pass

        # Test operations that might produce NaN
        try:
            # 0/0 might produce NaN instead of error
            result = aifpl.evaluate("(/ 0.0 0.0)")
            if not isinstance(result, Exception) and not math.isnan(result):
                # If it doesn't produce NaN or error, it should at least be handled
                pass
        except AIFPLEvalError:
            # Division by zero error is expected and acceptable
            pass

    def test_mathematical_precision_limits(self, aifpl):
        """Test mathematical precision limits."""
        # Very small number operations
        result = aifpl.evaluate("(+ 1e-100 1e-100)")
        assert result == 2e-100

        # Operations near machine epsilon
        result = aifpl.evaluate("(+ 1.0 1e-15)")
        assert result == 1.0 + 1e-15

        # Very large number operations
        result = aifpl.evaluate("(+ 1e100 1e100)")
        assert result == 2e100

        # Precision loss in floating point
        result = aifpl.evaluate("(+ 1e20 1)")
        # This might lose precision due to floating point limitations
        # The test just ensures it doesn't crash
        assert isinstance(result, (int, float))

    def test_mathematical_identities(self, aifpl):
        """Test that mathematical identities hold."""
        # Additive identity
        assert aifpl.evaluate("(+ 5 0)") == 5
        assert aifpl.evaluate("(+ 0 5)") == 5

        # Multiplicative identity
        assert aifpl.evaluate("(* 5 1)") == 5
        assert aifpl.evaluate("(* 1 5)") == 5

        # Multiplicative zero
        assert aifpl.evaluate("(* 5 0)") == 0
        assert aifpl.evaluate("(* 0 5)") == 0

        # Exponentiation identities
        assert aifpl.evaluate("(** 5 1)") == 5
        assert aifpl.evaluate("(** 1 5)") == 1
        assert aifpl.evaluate("(** 5 0)") == 1

        # Logarithm identities (approximately)
        result = aifpl.evaluate("(log (exp 5))")
        assert abs(result - 5) < 1e-10

        result = aifpl.evaluate("(exp (log 5))")
        assert abs(result - 5) < 1e-10
