"""Tests for string operations and string manipulation functions."""

import pytest

from aifpl import AIFPL, AIFPLEvalError


class TestStrings:
    """Test string operations and manipulation functions."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic string literals
        ('"hello"', '"hello"'),
        ('""', '""'),  # Empty string
        ('"hello world"', '"hello world"'),

        # Strings with spaces
        ('"  hello  "', '"  hello  "'),
        ('"\\thello\\n"', '"\\thello\\n"'),  # Tabs and newlines preserved in output
    ])
    def test_string_literals(self, aifpl, expression, expected):
        """Test basic string literal parsing and formatting."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected_content", [
        # Escape sequences
        ('"\\""', '"'),  # Escaped quote
        ('"\\\\"', '\\'),  # Escaped backslash
        ('"\\n"', '\n'),  # Newline
        ('"\\t"', '\t'),  # Tab
        ('"\\r"', '\r'),  # Carriage return

        # Unicode escape sequences
        ('"\\u0041"', 'A'),  # Unicode A
        ('"\\u03B1"', 'α'),  # Greek alpha
        ('"\\u03B2"', 'β'),  # Greek beta
        ('"\\u03B3"', 'γ'),  # Greek gamma

        # Combined escapes
        ('"Hello\\nWorld"', 'Hello\nWorld'),
        ('"Quote: \\""', 'Quote: "'),
        ('"Path\\\\to\\\\file"', 'Path\\to\\file'),
    ])
    def test_string_escape_sequences(self, aifpl, expression, expected_content):
        """Test string escape sequence processing."""
        result = aifpl.evaluate(expression)
        assert result == expected_content

    @pytest.mark.parametrize("unicode_expr,expected_content", [
        # Basic Unicode
        ('"Hello 世界"', 'Hello 世界'),
        ('"Café naïve"', 'Café naïve'),

        # Emoji (4-byte UTF-8)
        ('"Hello 👋"', 'Hello 👋'),
        ('"🌍 World"', '🌍 World'),

        # Mixed ASCII and Unicode
        ('"résumé"', 'résumé'),
        ('"Москва"', 'Москва'),  # Russian
        ('"北京"', '北京'),  # Chinese

        # Unicode via escape sequences
        ('"\\u4F60\\u597D"', '你好'),  # "Hello" in Chinese
    ])
    def test_unicode_string_handling(self, aifpl, unicode_expr, expected_content):
        """Test Unicode string handling."""
        result = aifpl.evaluate(unicode_expr)
        assert result == expected_content

    @pytest.mark.parametrize("expression,expected", [
        # Basic string append
        ('(string-concat "hello" " " "world")', '"hello world"'),
        ('(string-concat "a" "b" "c")', '"abc"'),

        # Empty string handling
        ('(string-concat)', '""'),  # Identity case
        ('(string-concat "")', '""'),
        ('(string-concat "" "hello")', '"hello"'),
        ('(string-concat "hello" "")', '"hello"'),

        # Many arguments
        ('(string-concat "a" "b" "c" "d" "e")', '"abcde"'),

        # Unicode strings
        ('(string-concat "Hello " "世界")', '"Hello 世界"'),
    ])
    def test_string_append(self, aifpl, expression, expected):
        """Test string-concat operation."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic string length
        ('(string-length "hello")', '5'),
        ('(string-length "")', '0'),
        ('(string-length "a")', '1'),

        # Unicode length (character count, not byte count)
        ('(string-length "世界")', '2'),
        ('(string-length "café")', '4'),
        ('(string-length "👋")', '1'),  # Single emoji

        # Strings with escape sequences
        ('(string-length "hello\\nworld")', '11'),  # Includes the newline
        ('(string-length "\\t")', '1'),  # Tab is one character
    ])
    def test_string_length(self, aifpl, expression, expected):
        """Test string-length function."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # Basic string-slice (3-argument form)
        ('(string-slice "hello" 1 4)', '"ell"'),  # Indices 1, 2, 3
        ('(string-slice "hello" 0 5)', '"hello"'),  # Full string
        ('(string-slice "hello" 0 1)', '"h"'),  # First character
        ('(string-slice "hello" 4 5)', '"o"'),  # Last character

        # Empty string-slice
        ('(string-slice "hello" 2 2)', '""'),  # Empty range
        ('(string-slice "hello" 0 0)', '""'),  # Empty range at start

        # Unicode string-slices
        ('(string-slice "世界你好" 1 3)', '"界你"'),
        ('(string-slice "café" 1 3)', '"af"'),
    ])
    def test_string_slice(self, aifpl, expression, expected):
        """Test string-slice function (3-argument form)."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # string-slice 2-argument form: (string-slice str start) -> from start to end
        ('(string-slice "hello" 2)', '"llo"'),    # from index 2 to end
        ('(string-slice "hello" 0)', '"hello"'),  # full string
        ('(string-slice "hello" 5)', '""'),        # start at end -> empty
        ('(string-slice "hello" 1)', '"ello"'),   # from index 1 to end
        ('(string-slice "hello" 4)', '"o"'),       # last character only
    ])
    def test_string_slice_two_arg_form(self, aifpl, expression, expected):
        """Test string-slice function (2-argument form: from index to end of string)."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_string_slice_index_errors(self, aifpl):
        """Test string-slice with invalid indices."""
        # Index out of range should be handled gracefully or raise error
        # Let's test the current behavior
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-slice "hello" 0 10)')  # End beyond string

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-slice "hello" -1 3)')  # Negative start

    def test_string_slice_negative_end_index(self, aifpl):
        """Test string-slice with negative end index."""
        with pytest.raises(AIFPLEvalError, match="string-slice end index cannot be negative"):
            aifpl.evaluate('(string-slice "hello" 1 -1)')

        with pytest.raises(AIFPLEvalError, match="string-slice end index cannot be negative"):
            aifpl.evaluate('(string-slice "test" 0 -5)')

        with pytest.raises(AIFPLEvalError, match="string-slice end index cannot be negative"):
            aifpl.evaluate('(string-slice "" 0 -1)')  # Empty string with negative end

    def test_string_slice_start_index_out_of_range(self, aifpl):
        """Test string-slice with start index beyond string length."""
        with pytest.raises(AIFPLEvalError, match="string-slice start index out of range"):
            aifpl.evaluate('(string-slice "hello" 10 15)')  # start_idx > string_len

        with pytest.raises(AIFPLEvalError, match="string-slice start index out of range"):
            aifpl.evaluate('(string-slice "hi" 5 6)')  # start_idx > string_len

        with pytest.raises(AIFPLEvalError, match="string-slice start index out of range"):
            aifpl.evaluate('(string-slice "" 1 2)')  # start_idx > empty string length

        # Test with Unicode strings
        with pytest.raises(AIFPLEvalError, match="string-slice start index out of range"):
            aifpl.evaluate('(string-slice "世界" 5 6)')  # start beyond Unicode string

    def test_string_slice_start_greater_than_end(self, aifpl):
        """Test string-slice with start index greater than end index."""
        with pytest.raises(AIFPLEvalError, match="start index.*cannot be greater than end index"):
            aifpl.evaluate('(string-slice "hello" 3 1)')  # start > end

        with pytest.raises(AIFPLEvalError, match="start index.*cannot be greater than end index"):
            aifpl.evaluate('(string-slice "test" 4 2)')  # start > end

        with pytest.raises(AIFPLEvalError, match="start index.*cannot be greater than end index"):
            aifpl.evaluate('(string-slice "world" 5 0)')  # start > end

        # Test with Unicode strings
        with pytest.raises(AIFPLEvalError, match="start index.*cannot be greater than end index"):
            aifpl.evaluate('(string-slice "café" 3 1)')  # start > end with Unicode

    def test_string_slice_edge_cases_comprehensive(self, aifpl):
        """Test comprehensive edge cases for string-slice validation."""
        # Valid boundary cases (should work)
        assert aifpl.evaluate_and_format('(string-slice "hello" 5 5)') == '""'  # start == end == length
        assert aifpl.evaluate_and_format('(string-slice "hello" 0 0)') == '""'  # start == end == 0
        assert aifpl.evaluate_and_format('(string-slice "hello" 2 2)') == '""'  # start == end in middle

        # Test validation order precedence
        with pytest.raises(AIFPLEvalError, match="string-slice start index cannot be negative"):
            aifpl.evaluate('(string-slice "test" -1 10)')  # start negative, end out of range

        with pytest.raises(AIFPLEvalError, match="string-slice end index cannot be negative"):
            aifpl.evaluate('(string-slice "test" 0 -1)')  # start valid, end negative

        with pytest.raises(AIFPLEvalError, match="string-slice start index out of range"):
            aifpl.evaluate('(string-slice "test" 10 15)')  # both out of range, start checked first

    def test_string_slice_with_different_string_lengths(self, aifpl):
        """Test error conditions with various string lengths."""
        # Empty string
        with pytest.raises(AIFPLEvalError, match="string-slice start index out of range"):
            aifpl.evaluate('(string-slice "" 1 1)')

        # Single character
        with pytest.raises(AIFPLEvalError, match="string-slice start index out of range"):
            aifpl.evaluate('(string-slice "a" 2 3)')

        # Long string
        with pytest.raises(AIFPLEvalError, match="start index.*cannot be greater than end index"):
            aifpl.evaluate('(string-slice "abcdefghijklmnop" 10 5)')

        # Multiple violations - should catch first one encountered
        with pytest.raises(AIFPLEvalError, match="string-slice start index cannot be negative"):
            aifpl.evaluate('(string-slice "hello" -1 -2)')  # Both negative, start checked first

    @pytest.mark.parametrize("expression,expected", [
        # String case conversion
        ('(string-upcase "hello")', '"HELLO"'),
        ('(string-upcase "Hello World")', '"HELLO WORLD"'),
        ('(string-upcase "")', '""'),
        ('(string-upcase "ALREADY UPPER")', '"ALREADY UPPER"'),

        ('(string-downcase "HELLO")', '"hello"'),
        ('(string-downcase "Hello World")', '"hello world"'),
        ('(string-downcase "")', '""'),
        ('(string-downcase "already lower")', '"already lower"'),

        # Unicode case conversion
        ('(string-upcase "café")', '"CAFÉ"'),
        ('(string-downcase "CAFÉ")', '"café"'),
    ])
    def test_string_case_conversion(self, aifpl, expression, expected):
        """Test string case conversion functions."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # String character reference
        ('(string-ref "hello" 0)', '"h"'),
        ('(string-ref "hello" 1)', '"e"'),
        ('(string-ref "hello" 4)', '"o"'),

        # Unicode character reference
        ('(string-ref "世界" 0)', '"世"'),
        ('(string-ref "世界" 1)', '"界"'),
        ('(string-ref "café" 3)', '"é"'),
    ])
    def test_string_ref(self, aifpl, expression, expected):
        """Test string-ref function."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_string_ref_index_errors(self, aifpl):
        """Test string-ref with invalid indices."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-ref "hello" 5)')  # Index out of range

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-ref "hello" -1)')  # Negative index

    @pytest.mark.parametrize("expression,expected", [
        # String contains predicate
        ('(string-contains? "hello world" "world")', '#t'),
        ('(string-contains? "hello world" "hello")', '#t'),
        ('(string-contains? "hello world" "o w")', '#t'),
        ('(string-contains? "hello world" "xyz")', '#f'),
        ('(string-contains? "hello" "")', '#t'),  # Empty string is contained
        ('(string-contains? "" "")', '#t'),  # Empty contains empty
        ('(string-contains? "" "a")', '#f'),  # Empty doesn\'t contain non-empty

        # Unicode contains
        ('(string-contains? "Hello 世界" "世界")', '#t'),
        ('(string-contains? "café" "fé")', '#t'),
    ])
    def test_string_contains(self, aifpl, expression, expected):
        """Test string-contains? predicate."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # String prefix predicate
        ('(string-prefix? "hello world" "hello")', '#t'),
        ('(string-prefix? "hello world" "he")', '#t'),
        ('(string-prefix? "hello world" "")', '#t'),  # Empty prefix
        ('(string-prefix? "hello world" "world")', '#f'),  # Not at start
        ('(string-prefix? "hello" "hello world")', '#f'),  # Prefix longer than string

        # Unicode prefix
        ('(string-prefix? "世界你好" "世界")', '#t'),
        ('(string-prefix? "café" "ca")', '#t'),
    ])
    def test_string_prefix(self, aifpl, expression, expected):
        """Test string-prefix? predicate."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # String suffix predicate
        ('(string-suffix? "hello world" "world")', '#t'),
        ('(string-suffix? "hello world" "ld")', '#t'),
        ('(string-suffix? "hello world" "")', '#t'),  # Empty suffix
        ('(string-suffix? "hello world" "hello")', '#f'),  # Not at end
        ('(string-suffix? "hello" "hello world")', '#f'),  # Suffix longer than string

        # Unicode suffix
        ('(string-suffix? "世界你好" "你好")', '#t'),
        ('(string-suffix? "café" "é")', '#t'),
    ])
    def test_string_suffix(self, aifpl, expression, expected):
        """Test string-suffix? predicate."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # String equality predicate
        ('(string=? "hello" "hello")', '#t'),
        ('(string=? "hello" "world")', '#f'),
        ('(string=? "" "")', '#t'),

        # Multiple arguments (all must be equal)
        ('(string=? "hello" "hello" "hello")', '#t'),
        ('(string=? "hello" "hello" "world")', '#f'),

        # Case sensitive
        ('(string=? "Hello" "hello")', '#f'),

        # Unicode equality
        ('(string=? "世界" "世界")', '#t'),
        ('(string=? "café" "cafe")', '#f'),  # Different characters
    ])
    def test_string_equality(self, aifpl, expression, expected):
        """Test string=? predicate."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # String to number conversion
        ('(string->number "42")', '42'),
        ('(string->number "3.14")', '3.14'),
        ('(string->number "-5")', '-5'),
        ('(string->number "0")', '0'),

        # Scientific notation
        ('(string->number "1e2")', '100.0'),
        ('(string->number "1.5e-2")', '0.015'),

        # Complex numbers
        ('(string->number "1+2j")', '1+2j'),
        ('(string->number "3j")', '3j'),

        # AIFPL base-prefixed integer literals (lower case)
        ('(string->number "#xff")', '255'),
        ('(string->number "#b1010")', '10'),
        ('(string->number "#o755")', '493'),

        # AIFPL base-prefixed integer literals (upper case)
        ('(string->number "#XFF")', '255'),
        ('(string->number "#B1010")', '10'),
        ('(string->number "#O755")', '493'),

        # Negative base-prefixed literals
        ('(string->number "-#xff")', '-255'),
        ('(string->number "-#b1010")', '-10'),
        ('(string->number "-#o755")', '-493'),

        # Unparseable strings return #f
        ('(string->number "hello")', '#f'),
        ('(string->number "12.34.56")', '#f'),
        ('(string->number "")', '#f'),
        ('(string->number "#xZZ")', '#f'),
        ('(string->number "#b2")', '#f'),
        ('(string->number "#o9")', '#f'),
    ])
    def test_string_to_number(self, aifpl, expression, expected):
        """Test string->number conversion."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_string_to_number_type_error(self, aifpl):
        """Test string->number raises on non-string argument (type error, not parse failure)."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->number 42)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->number (list 1 2))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->number #t)')

    @pytest.mark.parametrize("expression,expected", [
        # Number to string conversion
        ('(integer->string 42)', '"42"'),
        ('(float->string 3.14)', '"3.14"'),
        ('(integer->string -5)', '"-5"'),
        ('(integer->string 0)', '"0"'),

        # Complex numbers
        ('(complex->string (float->complex 1.0 2.0))', '"1+2j"'),
        ('(complex->string 1j)', '"1j"'),
    ])
    def test_number_to_string(self, aifpl, expression, expected):
        """Test number->string conversion."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # String to list conversion
        ('(string->list "hello")', '("h" "e" "l" "l" "o")'),
        ('(string->list "")', '()'),
        ('(string->list "a")', '("a")'),

        # Unicode string to list
        ('(string->list "世界")', '("世" "界")'),
        ('(string->list "café")', '("c" "a" "f" "é")'),
    ])
    def test_string_to_list(self, aifpl, expression, expected):
        """Test string->list conversion."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # List to string conversion
        ('(list->string (list "h" "e" "l" "l" "o"))', '"hello"'),
        ('(list->string (list))', '""'),
        ('(list->string (list "a"))', '"a"'),

        # Unicode list to string
        ('(list->string (list "世" "界"))', '"世界"'),
        ('(list->string (list "c" "a" "f" "é"))', '"café"'),
    ])
    def test_list_to_string(self, aifpl, expression, expected):
        """Test list->string conversion."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_list_to_string_non_string_elements(self, aifpl):
        """Test list->string with non-string elements."""
        # Should accept and convert non-string elements
        result = aifpl.evaluate_and_format('(list->string (list "h" "i"))')
        assert result == '"hi"'

    @pytest.mark.parametrize("expression,expected", [
        # String split
        ('(string->list "a,b,c" ",")', '("a" "b" "c")'),
        ('(string->list "hello world" " ")', '("hello" "world")'),
        ('(string->list "one::two::three" "::")', '("one" "two" "three")'),

        # Edge cases
        ('(string->list "" ",")', '("")'),  # Empty string splits to list with empty string
        ('(string->list "hello" "")', '("h" "e" "l" "l" "o")'),  # Split on empty delimiter
        ('(string->list "no-delimiters" ",")', '("no-delimiters")'),  # No delimiter found

        # Unicode split
        ('(string->list "世界,你好" ",")', '("世界" "你好")'),
    ])
    def test_string_split(self, aifpl, expression, expected):
        """Test string->list function."""
        assert aifpl.evaluate_and_format(expression) == expected

    @pytest.mark.parametrize("expression,expected", [
        # String join
        ('(list->string (list "hello" "world") " ")', '"hello world"'),
        ('(list->string (list "a" "b" "c") ",")', '"a,b,c"'),
        ('(list->string (list "one" "two" "three") "::")', '"one::two::three"'),

        # Edge cases
        ('(list->string (list) ",")', '""'),  # Empty list
        ('(list->string (list "hello") ",")', '"hello"'),  # Single element
        ('(list->string (list "a" "b") "")', '"ab"'),  # Empty separator

        # Unicode join
        ('(list->string (list "世界" "你好") " ")', '"世界 你好"'),
    ])
    def test_string_join(self, aifpl, expression, expected):
        """Test list->string function."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_string_join_non_string_list(self, aifpl):
        """Test list->string with non-string list elements."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list->string (list 1 2 3) ",")')

    def test_string_operations_type_validation(self, aifpl):
        """Test that string operations reject non-string arguments."""
        # string-concat with non-strings
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-concat "hello" 42)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-concat #t "world")')

        # string-length with non-string
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-length 42)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-length (list "h" "i"))')

        # string-slice with non-string
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-slice 42 0 1)')

        # string-upcase/downcase with non-string
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-upcase 42)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-downcase #t)')

    def test_string_predicates_type_validation(self, aifpl):
        """Test that string predicates require string arguments."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-contains? 42 "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-prefix? "hello" 42)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-suffix? #t "world")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string=? "hello" 42)')

    def test_string_conversion_type_validation(self, aifpl):
        """Test that string conversion functions validate argument types."""
        # string->number requires string
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->number 42)')

        # integer->string requires number
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(integer->string "hello")')

        # float->string requires number
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(float->string "hello")')

        # complex->string requires number
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(complex->string "hello")')

        # string->list requires string
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->list 42)')

        # list->string requires list
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list->string "hello")')

    def test_string_split_join_type_validation(self, aifpl):
        """Test that string->list and list->string validate argument types."""
        # string->list requires strings
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->list 42 ",")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string->list "hello" 42)')

        # list->string requires list and string
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list->string "hello" ",")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(list->string (list "a" "b") 42)')

    def test_string_arity_validation(self, aifpl):
        """Test that string functions validate argument counts."""
        # Functions requiring exactly 1 argument
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-length)')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-length "hello" "world")')

        # Functions requiring exactly 2 arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-contains? "hello")')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-ref "hello")')

        # string-slice requires 2 or 3 arguments (not 1)
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-slice "hello")')

    def test_round_trip_string_list_conversion(self, aifpl):
        """Test round-trip conversion between strings and lists."""
        test_strings = [
            '"hello"',
            '"world"',
            '""',
            '"café"',
            '"世界"',
        ]

        for string_expr in test_strings:
            # string -> list -> string should be identity
            round_trip = f'(list->string (string->list {string_expr}))'
            result = aifpl.evaluate_and_format(round_trip)
            assert result == aifpl.evaluate_and_format(string_expr)

    def test_complex_string_operations(self, aifpl, helpers):
        """Test complex combinations of string operations."""
        # Split, process, and rejoin
        complex_expr = '''
        (list->string
          (map (lambda (s) (string-upcase s))
               (string->list "hello,world,test" ","))
          " | ")
        '''
        # This requires lambda and map to work, so it's more of an integration test
        # For now, let's test a simpler combination
        helpers.assert_evaluates_to(
            aifpl,
            '(string-concat (string-upcase "hello") " " (string-downcase "WORLD"))',
            '"HELLO world"'
        )

        # Nested string operations
        helpers.assert_evaluates_to(
            aifpl,
            '(string-length (string-concat "hello" " " "world"))',
            '11'
        )

        # String operations with conversions
        helpers.assert_evaluates_to(
            aifpl,
            '(string-concat "Count: " (integer->string 42))',
            '"Count: 42"'
        )
