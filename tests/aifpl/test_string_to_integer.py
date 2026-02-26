"""Tests for string->integer conversion function."""

import pytest

from aifpl import AIFPL, AIFPLEvalError


class TestStringToInteger:
    """Tests for the string->integer built-in function."""

    # --- Base 10 (default) ---

    @pytest.mark.parametrize("expression,expected", [
        # Explicit radix 10
        ('(string->integer "42" 10)', '42'),
        ('(string->integer "0" 10)', '0'),
        ('(string->integer "-5" 10)', '-5'),
        ('(string->integer "1000000" 10)', '1000000'),
        # Default radix (no second argument)
        ('(string->integer "42")', '42'),
        ('(string->integer "0")', '0'),
        ('(string->integer "-99")', '-99'),
    ])
    def test_base_10(self, aifpl, expression, expected):
        """Test parsing decimal strings."""
        assert aifpl.evaluate_and_format(expression) == expected

    # --- Base 16 ---

    @pytest.mark.parametrize("expression,expected", [
        ('(string->integer "ff" 16)', '255'),
        ('(string->integer "FF" 16)', '255'),
        ('(string->integer "Ff" 16)', '255'),      # mixed case
        ('(string->integer "0" 16)', '0'),
        ('(string->integer "1" 16)', '1'),
        ('(string->integer "a" 16)', '10'),
        ('(string->integer "10" 16)', '16'),
        ('(string->integer "100" 16)', '256'),
        ('(string->integer "-ff" 16)', '-255'),
        ('(string->integer "-FF" 16)', '-255'),
    ])
    def test_base_16(self, aifpl, expression, expected):
        """Test parsing hexadecimal strings."""
        assert aifpl.evaluate_and_format(expression) == expected

    # --- Base 2 ---

    @pytest.mark.parametrize("expression,expected", [
        ('(string->integer "1010" 2)', '10'),
        ('(string->integer "0" 2)', '0'),
        ('(string->integer "1" 2)', '1'),
        ('(string->integer "11111111" 2)', '255'),
        ('(string->integer "10000000" 2)', '128'),
        ('(string->integer "-1010" 2)', '-10'),
    ])
    def test_base_2(self, aifpl, expression, expected):
        """Test parsing binary strings."""
        assert aifpl.evaluate_and_format(expression) == expected

    # --- Base 8 ---

    @pytest.mark.parametrize("expression,expected", [
        ('(string->integer "377" 8)', '255'),
        ('(string->integer "0" 8)', '0'),
        ('(string->integer "1" 8)', '1'),
        ('(string->integer "10" 8)', '8'),
        ('(string->integer "777" 8)', '511'),
        ('(string->integer "-377" 8)', '-255'),
    ])
    def test_base_8(self, aifpl, expression, expected):
        """Test parsing octal strings."""
        assert aifpl.evaluate_and_format(expression) == expected

    # --- Round-trip with integer->string ---

    @pytest.mark.parametrize("n,radix", [
        (255, 16),
        (255, 2),
        (255, 8),
        (255, 10),
        (0, 10),
        (1, 2),
        (1024, 16),
        (511, 8),
    ])
    def test_round_trip(self, aifpl, n, radix):
        """string->integer(integer->string(n, r), r) == n for all supported radices."""
        expr = f'(string->integer (integer->string {n} {radix}) {radix})'
        assert aifpl.evaluate_and_format(expr) == str(n)

    # --- Returns #f on parse failure ---

    @pytest.mark.parametrize("expression", [
        '(string->integer "hello" 10)',
        '(string->integer "" 10)',
        '(string->integer "ff" 10)',       # hex digits not valid in base 10
        '(string->integer "2" 2)',         # '2' not valid in base 2
        '(string->integer "8" 8)',         # '8' not valid in base 8
        '(string->integer "xyz" 16)',      # 'x', 'y', 'z' not valid hex digits
        '(string->integer "3.14" 10)',     # float string not valid
        '(string->integer "1+2j" 10)',     # complex string not valid
        '(string->integer "#xff" 16)',     # prefixed literal not valid (no prefix stripping)
        '(string->integer "hello")',       # default radix, unparseable
    ])
    def test_returns_false_on_failure(self, aifpl, expression):
        """Test that unparseable strings return #f rather than raising an error."""
        assert aifpl.evaluate_and_format(expression) == '#f'

    @pytest.mark.parametrize("expression,expected", [
        # Python's int() strips surrounding whitespace, so these succeed
        ('(string->integer " 42" 10)', '42'),
        ('(string->integer "42 " 10)', '42'),
        ('(string->integer "  -5  " 10)', '-5'),
    ])
    def test_whitespace_is_stripped(self, aifpl, expression, expected):
        """Test that surrounding whitespace is accepted (Python int() behaviour)."""
        assert aifpl.evaluate_and_format(expression) == expected

    # --- Invalid radix raises error ---

    @pytest.mark.parametrize("radix", [0, 1, 3, 4, 5, 6, 7, 9, 11, 15, 17, 32, -1])
    def test_invalid_radix_raises_error(self, aifpl, radix):
        """Test that an unsupported radix raises a runtime error."""
        with pytest.raises(AIFPLEvalError, match="radix must be 2, 8, 10, or 16"):
            aifpl.evaluate(f'(string->integer "42" {radix})')

    # --- Type errors ---

    def test_non_string_first_arg_raises_error(self, aifpl):
        """Test that a non-string first argument raises a type error."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer 42 10)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer #t 10)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer (list "4" "2") 10)')

    def test_non_integer_radix_raises_error(self, aifpl):
        """Test that a non-integer radix raises a type error."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer "42" 10.0)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer "42" "10")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer "42" #t)')

    # --- Arity errors ---

    def test_arity_errors(self, aifpl):
        """Test that wrong argument counts raise errors."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->integer "42" 10 16)')

    # --- Result is an integer type ---

    def test_result_is_integer(self, aifpl):
        """Test that successful parses return an integer value."""
        assert aifpl.evaluate_and_format('(integer? (string->integer "42" 10))') == '#t'
        assert aifpl.evaluate_and_format('(integer? (string->integer "ff" 16))') == '#t'
        assert aifpl.evaluate_and_format('(integer? (string->integer "1010" 2))') == '#t'

    def test_failure_result_is_boolean(self, aifpl):
        """Test that parse failures return a boolean (#f), not some other type."""
        assert aifpl.evaluate_and_format('(boolean? (string->integer "hello" 10))') == '#t'

    # --- First-class use ---

    def test_used_as_first_class_function(self, aifpl):
        """Test that string->integer can be passed as a first-class value."""
        expr = '(list-map (lambda (s) (string->integer s 16)) (list "ff" "10" "a"))'
        assert aifpl.evaluate_and_format(expr) == '(255 16 10)'

    def test_used_with_list_filter(self, aifpl):
        """Test string->integer used with list-filter to discard unparseable values."""
        expr = '''
        (list-filter
          (lambda (x) (integer? x))
          (list-map
            (lambda (s) (string->integer s 10))
            (list "1" "two" "3" "four" "5")))
        '''
        assert aifpl.evaluate_and_format(expr) == '(1 3 5)'
