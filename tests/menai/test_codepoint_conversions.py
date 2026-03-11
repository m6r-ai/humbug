"""Tests for string->integer-codepoint and integer-codepoint->string."""

import pytest

from menai import MenaiEvalError


class TestStringToIntegerCodepoint:
    """Tests for string->integer-codepoint."""

    @pytest.mark.parametrize("expression,expected", [
        # ASCII range
        ('(string->integer-codepoint "A")',    '65'),
        ('(string->integer-codepoint "a")',    '97'),
        ('(string->integer-codepoint "Z")',    '90'),
        ('(string->integer-codepoint "0")',    '48'),
        ('(string->integer-codepoint " ")',    '32'),
        ('(string->integer-codepoint "!")',    '33'),
        # Boundary: NUL (codepoint 0)
        ('(string->integer-codepoint "\x00")', '0'),
        # Boundary: highest ASCII
        ('(string->integer-codepoint "\x7f")', '127'),
        # Non-ASCII BMP
        ('(string->integer-codepoint "\u00e9")', '233'),    # é
        ('(string->integer-codepoint "\u03c0")', '960'),    # π
        ('(string->integer-codepoint "\u4e2d")', '20013'),  # 中
        ('(string->integer-codepoint "\uffff")', '65535'),  # highest BMP
        # Supplementary plane (above U+FFFF)
        ('(string->integer-codepoint "\U0001F600")', '128512'),  # 😀
        ('(string->integer-codepoint "\U0010FFFF")', '1114111'), # highest valid codepoint
    ])
    def test_valid_characters(self, menai, expression, expected):
        """Test conversion of valid single-character strings."""
        assert menai.evaluate_and_format(expression) == expected

    def test_result_is_integer(self, menai):
        """Result is an integer value."""
        assert menai.evaluate_and_format('(integer? (string->integer-codepoint "A"))') == '#t'

    # --- Error: wrong string length ---

    def test_empty_string_raises_error(self, menai):
        """Empty string raises an error."""
        with pytest.raises(MenaiEvalError, match="single-character string"):
            menai.evaluate('(string->integer-codepoint "")')

    @pytest.mark.parametrize("expression", [
        '(string->integer-codepoint "AB")',
        '(string->integer-codepoint "hello")',
        '(string->integer-codepoint "  ")',
    ])
    def test_multi_character_string_raises_error(self, menai, expression):
        """Multi-character string raises an error."""
        with pytest.raises(MenaiEvalError, match="single-character string"):
            menai.evaluate(expression)

    # --- Error: wrong type ---

    @pytest.mark.parametrize("expression", [
        '(string->integer-codepoint 65)',
        '(string->integer-codepoint 65.0)',
        '(string->integer-codepoint #t)',
        '(string->integer-codepoint #none)',
        '(string->integer-codepoint (list "A"))',
    ])
    def test_non_string_raises_error(self, menai, expression):
        """Non-string argument raises a type error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(expression)

    # --- Arity errors ---

    def test_no_args_raises_error(self, menai):
        """Zero arguments raises an arity error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string->integer-codepoint)')

    def test_too_many_args_raises_error(self, menai):
        """Two arguments raises an arity error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string->integer-codepoint "A" "B")')

    # --- First-class use ---

    def test_used_as_first_class_function(self, menai):
        """string->integer-codepoint can be passed as a first-class value."""
        result = menai.evaluate_and_format(
            '(map-list string->integer-codepoint (list "A" "B" "C"))'
        )
        assert result == '(65 66 67)'


class TestIntegerCodepointToString:
    """Tests for integer-codepoint->string."""

    @pytest.mark.parametrize("expression,expected", [
        # ASCII range
        ('(integer-codepoint->string 65)',  '"A"'),
        ('(integer-codepoint->string 97)',  '"a"'),
        ('(integer-codepoint->string 90)',  '"Z"'),
        ('(integer-codepoint->string 48)',  '"0"'),
        ('(integer-codepoint->string 32)',  '" "'),
        ('(integer-codepoint->string 33)',  '"!"'),
        # Boundary: NUL (codepoint 0)
        ('(integer-codepoint->string 0)',   '"\\u0000"'),
        # Boundary: highest ASCII
        ('(integer-codepoint->string 127)', '"\x7f"'),
        # Non-ASCII BMP
        ('(integer-codepoint->string 233)',   '"\u00e9"'),   # é
        ('(integer-codepoint->string 960)',   '"\u03c0"'),   # π
        ('(integer-codepoint->string 20013)', '"\u4e2d"'),   # 中
        ('(integer-codepoint->string 65535)', '"\uffff"'),   # highest BMP
        # Supplementary plane
        ('(integer-codepoint->string 128512)', '"\U0001F600"'),  # 😀
        ('(integer-codepoint->string 1114111)', '"\U0010FFFF"'), # highest valid codepoint
    ])
    def test_valid_codepoints(self, menai, expression, expected):
        """Test conversion of valid Unicode scalar values."""
        assert menai.evaluate_and_format(expression) == expected

    def test_result_is_string(self, menai):
        """Result is a string value."""
        assert menai.evaluate_and_format('(string? (integer-codepoint->string 65))') == '#t'

    def test_result_is_single_character(self, menai):
        """Result always has length 1."""
        assert menai.evaluate_and_format('(string-length (integer-codepoint->string 65))') == '1'
        assert menai.evaluate_and_format('(string-length (integer-codepoint->string 128512))') == '1'

    # --- Error: out-of-range codepoints ---

    def test_negative_raises_error(self, menai):
        """Negative value raises an error."""
        with pytest.raises(MenaiEvalError, match="valid Unicode scalar value"):
            menai.evaluate('(integer-codepoint->string -1)')

    def test_above_max_raises_error(self, menai):
        """Value above 0x10FFFF raises an error."""
        with pytest.raises(MenaiEvalError, match="valid Unicode scalar value"):
            menai.evaluate('(integer-codepoint->string 1114112)')

    # --- Error: surrogate codepoints ---

    @pytest.mark.parametrize("codepoint", [
        0xD800,   # first surrogate
        0xD900,   # mid surrogate range
        0xDFFF,   # last surrogate
    ])
    def test_surrogate_raises_error(self, menai, codepoint):
        """Surrogate codepoints raise an error."""
        with pytest.raises(MenaiEvalError, match="valid Unicode scalar value"):
            menai.evaluate(f'(integer-codepoint->string {codepoint})')

    # --- Error: wrong type ---

    @pytest.mark.parametrize("expression", [
        '(integer-codepoint->string "A")',
        '(integer-codepoint->string 65.0)',
        '(integer-codepoint->string #t)',
        '(integer-codepoint->string #none)',
        '(integer-codepoint->string (list 65))',
    ])
    def test_non_integer_raises_error(self, menai, expression):
        """Non-integer argument raises a type error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(expression)

    # --- Arity errors ---

    def test_no_args_raises_error(self, menai):
        """Zero arguments raises an arity error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer-codepoint->string)')

    def test_too_many_args_raises_error(self, menai):
        """Two arguments raises an arity error."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer-codepoint->string 65 66)')

    # --- First-class use ---

    def test_used_as_first_class_function(self, menai):
        """integer-codepoint->string can be passed as a first-class value."""
        result = menai.evaluate_and_format(
            '(map-list integer-codepoint->string (list 65 66 67))'
        )
        assert result == '("A" "B" "C")'


class TestCodepointRoundTrip:
    """Round-trip tests between the two functions."""

    @pytest.mark.parametrize("char", [
        '"A"', '"z"', '"0"', '"\u03c0"', '"\u4e2d"', '"\U0001F600"',
    ])
    def test_string_to_codepoint_to_string(self, menai, char):
        """integer-codepoint->string(string->integer-codepoint(c)) == c for all valid chars."""
        expr = f'(integer-codepoint->string (string->integer-codepoint {char}))'
        assert menai.evaluate_and_format(expr) == char

    @pytest.mark.parametrize("codepoint", [65, 97, 233, 960, 20013, 65535, 128512, 1114111])
    def test_codepoint_to_string_to_codepoint(self, menai, codepoint):
        """string->integer-codepoint(integer-codepoint->string(n)) == n for all valid codepoints."""
        expr = f'(string->integer-codepoint (integer-codepoint->string {codepoint}))'
        assert menai.evaluate_and_format(expr) == str(codepoint)


class TestCodepointConstantFolding:
    """Verify that both functions are folded at compile time when given constant arguments."""

    def test_string_to_codepoint_folded(self, menai):
        """string->integer-codepoint with a literal is constant-folded."""
        assert menai.evaluate('(string->integer-codepoint "A")') == 65

    def test_codepoint_to_string_folded(self, menai):
        """integer-codepoint->string with a literal is constant-folded."""
        assert menai.evaluate('(integer-codepoint->string 65)') == 'A'

    def test_nested_fold(self, menai):
        """Nested constant expression folds completely at compile time."""
        assert menai.evaluate(
            '(string->integer-codepoint (integer-codepoint->string 65))'
        ) == 65

    def test_invalid_length_not_folded(self, menai):
        """Invalid-length string is not folded — error is raised at runtime."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string->integer-codepoint "AB")')

    def test_invalid_codepoint_not_folded(self, menai):
        """Out-of-range codepoint is not folded — error is raised at runtime."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer-codepoint->string -1)')

    def test_surrogate_not_folded(self, menai):
        """Surrogate codepoint is not folded — error is raised at runtime."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer-codepoint->string 55296)')
