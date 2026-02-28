"""Tests for float-log2, float-logn, and complex-logn builtins."""

import math
import cmath
import pytest

from menai import MenaiEvalError


class TestFloatLog2:
    """Tests for float-log2 (log base 2, correctly rounded via math.log2)."""

    def test_log2_power_of_two(self, menai):
        """log2 of exact powers of two should be exact integers."""
        assert menai.evaluate("(float-log2 1.0)") == 0.0
        assert menai.evaluate("(float-log2 2.0)") == 1.0
        assert menai.evaluate("(float-log2 4.0)") == 2.0
        assert menai.evaluate("(float-log2 8.0)") == 3.0
        assert menai.evaluate("(float-log2 1024.0)") == 10.0
        assert menai.evaluate("(float-log2 0.5)") == -1.0
        assert menai.evaluate("(float-log2 0.25)") == -2.0

    def test_log2_general_values(self, menai):
        """log2 of non-power-of-two values."""
        result = menai.evaluate("(float-log2 3.0)")
        assert abs(result - math.log2(3.0)) < 1e-14

        result = menai.evaluate("(float-log2 10.0)")
        assert abs(result - math.log2(10.0)) < 1e-14

        result = menai.evaluate("(float-log2 100.0)")
        assert abs(result - math.log2(100.0)) < 1e-14

    def test_log2_zero_returns_neg_inf(self, menai):
        """log2(0) should return -inf."""
        result = menai.evaluate("(float-log2 0.0)")
        assert math.isinf(result) and result < 0

    def test_log2_negative_raises_error(self, menai):
        """log2 of a negative number should raise an error."""
        with pytest.raises(MenaiEvalError, match="non-negative"):
            menai.evaluate("(float-log2 -1.0)")

        with pytest.raises(MenaiEvalError, match="non-negative"):
            menai.evaluate("(float-log2 -0.5)")

    def test_log2_type_error(self, menai):
        """log2 requires a float argument."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(float-log2 8)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(float-log2 \"8.0\")")

    def test_log2_precision_vs_log(self, menai):
        """float-log2 uses math.log2 directly and is correctly rounded."""
        # math.log2(3) gives a correctly-rounded result that differs from
        # math.log(3)/math.log(2) by a small ULP error.
        result_log2 = menai.evaluate("(float-log2 3.0)")
        assert result_log2 == math.log2(3.0)

    def test_log2_constant_folding(self, menai):
        """Constant folding should evaluate float-log2 at compile time."""
        # This exercises the constant folder path
        result = menai.evaluate("(float-log2 1024.0)")
        assert result == 10.0

    def test_log2_first_class(self, menai):
        """float-log2 can be used as a first-class function."""
        result = menai.evaluate("(list-map float-log2 (list 1.0 2.0 4.0 8.0))")
        assert result == [0.0, 1.0, 2.0, 3.0]

    def test_log2_identity(self, menai):
        """2^(log2(x)) == x for positive floats."""
        result = menai.evaluate("(float-expn 2.0 (float-log2 7.0))")
        assert abs(result - 7.0) < 1e-13


class TestFloatLogn:
    """Tests for float-logn (log base n, general case)."""

    def test_logn_base2(self, menai):
        """float-logn with base 2 should match log2 for exact cases."""
        assert menai.evaluate("(float-logn 1.0 2.0)") == 0.0
        assert menai.evaluate("(float-logn 8.0 2.0)") == 3.0
        assert menai.evaluate("(float-logn 1024.0 2.0)") == 10.0

    def test_logn_base10(self, menai):
        """float-logn with base 10 should match log10 for exact cases."""
        assert abs(menai.evaluate("(float-logn 1.0 10.0)") - 0.0) < 1e-14
        assert abs(menai.evaluate("(float-logn 10.0 10.0)") - 1.0) < 1e-14
        assert abs(menai.evaluate("(float-logn 100.0 10.0)") - 2.0) < 1e-14
        # float-logn is the general case and may accumulate small rounding errors
        assert abs(menai.evaluate("(float-logn 1000.0 10.0)") - 3.0) < 1e-12

    def test_logn_base_e(self, menai):
        """float-logn with base e should match natural log."""
        result = menai.evaluate("(float-logn e e)")
        assert abs(result - 1.0) < 1e-14

        result = menai.evaluate("(float-logn 1.0 e)")
        assert abs(result - 0.0) < 1e-14

    def test_logn_arbitrary_base(self, menai):
        """float-logn with arbitrary base."""
        result = menai.evaluate("(float-logn 27.0 3.0)")
        assert abs(result - 3.0) < 1e-13

        result = menai.evaluate("(float-logn 625.0 5.0)")
        assert abs(result - 4.0) < 1e-13

    def test_logn_zero_x_returns_neg_inf(self, menai):
        """float-logn(0, base) should return -inf."""
        result = menai.evaluate("(float-logn 0.0 2.0)")
        assert math.isinf(result) and result < 0

    def test_logn_negative_x_raises_error(self, menai):
        """float-logn of negative x should raise an error."""
        with pytest.raises(MenaiEvalError, match="non-negative"):
            menai.evaluate("(float-logn -1.0 2.0)")

    def test_logn_zero_base_raises_error(self, menai):
        """float-logn with base 0 should raise an error."""
        with pytest.raises(MenaiEvalError, match="positive base"):
            menai.evaluate("(float-logn 8.0 0.0)")

    def test_logn_negative_base_raises_error(self, menai):
        """float-logn with negative base should raise an error."""
        with pytest.raises(MenaiEvalError, match="positive base"):
            menai.evaluate("(float-logn 8.0 -2.0)")

    def test_logn_base_one_raises_error(self, menai):
        """float-logn with base 1 should raise an error (log base 1 is undefined)."""
        with pytest.raises(MenaiEvalError, match="positive base"):
            menai.evaluate("(float-logn 8.0 1.0)")

    def test_logn_type_errors(self, menai):
        """float-logn requires two float arguments."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(float-logn 8 2.0)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(float-logn 8.0 2)")

    def test_logn_constant_folding(self, menai):
        """Constant folding should evaluate float-logn at compile time."""
        result = menai.evaluate("(float-logn 27.0 3.0)")
        assert abs(result - 3.0) < 1e-13

    def test_logn_first_class(self, menai):
        """float-logn can be used as a first-class function."""
        result = menai.evaluate(
            "(list-map (lambda (x) (float-logn x 10.0)) (list 1.0 10.0 100.0))"
        )
        assert len(result) == 3
        assert abs(result[0] - 0.0) < 1e-13
        assert abs(result[1] - 1.0) < 1e-13
        assert abs(result[2] - 2.0) < 1e-13

    def test_logn_identity(self, menai):
        """base^(logn(x, base)) == x for positive x and valid base."""
        result = menai.evaluate("(float-expn 3.0 (float-logn 81.0 3.0))")
        assert abs(result - 81.0) < 1e-11


class TestComplexLogn:
    """Tests for complex-logn (log base n for complex numbers)."""

    def test_complex_logn_base10(self, menai):
        """complex-logn with base 10 should match complex-log10."""
        result_logn = menai.evaluate("(complex-logn 100+0j 10+0j)")
        result_log10 = menai.evaluate("(complex-log10 100+0j)")
        assert abs(result_logn - result_log10) < 1e-14

    def test_complex_logn_base_e(self, menai):
        """complex-logn with base e should match complex-log."""
        import math
        e_complex = complex(math.e, 0)
        result_logn = menai.evaluate(
            "(complex-logn (float->complex 1.0 1.0) (float->complex e 0.0))"
        )
        result_log = menai.evaluate("(complex-log (float->complex 1.0 1.0))")
        assert abs(result_logn - result_log) < 1e-14

    def test_complex_logn_negative_real(self, menai):
        """complex-logn of -1 in base 2 should give i*pi/ln(2)."""
        result = menai.evaluate("(complex-logn -1+0j 2+0j)")
        expected = cmath.log(-1+0j, 2+0j)
        assert abs(result - expected) < 1e-14

    def test_complex_logn_pure_imaginary(self, menai):
        """complex-logn of i in base 10."""
        result = menai.evaluate("(complex-logn 1j 10+0j)")
        expected = cmath.log(1j, 10+0j)
        assert abs(result - expected) < 1e-14

    def test_complex_logn_zero_base_raises_error(self, menai):
        """complex-logn with zero base should raise an error."""
        with pytest.raises(MenaiEvalError, match="non-zero base"):
            menai.evaluate("(complex-logn 1+0j 0+0j)")

    def test_complex_logn_type_errors(self, menai):
        """complex-logn requires two complex arguments."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(complex-logn 8.0 2+0j)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(complex-logn 8+0j 2.0)")

    def test_complex_logn_constant_folding(self, menai):
        """Constant folding should evaluate complex-logn at compile time."""
        result = menai.evaluate("(complex-logn 100+0j 10+0j)")
        expected = cmath.log(100+0j, 10+0j)
        assert abs(result - expected) < 1e-14

    def test_complex_logn_first_class(self, menai):
        """complex-logn can be used as a first-class function."""
        result = menai.evaluate(
            "(list-map (lambda (z) (complex-logn z 10+0j)) (list 1+0j 10+0j 100+0j))"
        )
        assert len(result) == 3
        assert abs(result[0] - cmath.log(1+0j, 10+0j)) < 1e-14
        assert abs(result[1] - cmath.log(10+0j, 10+0j)) < 1e-14
        assert abs(result[2] - cmath.log(100+0j, 10+0j)) < 1e-14
