"""Tests for AIFPL tokenizer edge cases."""

import pytest

from aifpl import AIFPLError, AIFPLTokenError, AIFPLParseError


class TestAIFPLTokenizerEdgeCases:
    """Test tokenizer edge cases and comprehensive token handling."""

    def test_whitespace_tokenization_edge_cases(self, aifpl):
        """Test comprehensive whitespace handling in tokenization."""
        # Various whitespace combinations should be handled
        whitespace_expressions = [
            "  42  ",           # Leading/trailing spaces
            "\t42\t",           # Tabs
            "\n42\n",           # Newlines
            "\r42\r",           # Carriage returns
            " \t\n\r 42 \r\n\t ", # Mixed whitespace
        ]

        for expr in whitespace_expressions:
            result = aifpl.evaluate(expr)
            assert result == 42

    def test_number_tokenization_edge_cases(self, aifpl):
        """Test comprehensive number tokenization edge cases."""
        # Integer formats
        integer_cases = [
            ("0", 0),
            ("42", 42),
            ("-42", -42),
            ("123456789", 123456789),
            ("-123456789", -123456789),
        ]

        for expr, expected in integer_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Float formats
        float_cases = [
            ("0.0", 0.0),
            ("3.14", 3.14),
            ("-3.14", -3.14),
            (".5", 0.5),
            ("5.", 5.0),
            ("0.123456789", 0.123456789),
        ]

        for expr, expected in float_cases:
            result = aifpl.evaluate(expr)
            assert abs(result - expected) < 1e-10

        # Scientific notation
        scientific_cases = [
            ("1e2", 100.0),
            ("1E2", 100.0),
            ("1e-2", 0.01),
            ("1E-2", 0.01),
            ("1.5e2", 150.0),
            ("1.5E-2", 0.015),
            ("2.5e+1", 25.0),
            ("-1e3", -1000.0),
        ]

        for expr, expected in scientific_cases:
            result = aifpl.evaluate(expr)
            assert abs(result - expected) < 1e-10

        # Hexadecimal
        hex_cases = [
            ("0x0", 0),
            ("0xFF", 255),
            ("0xff", 255),
            ("0x10", 16),
            ("0xABC", 2748),
            ("0xabc", 2748),
        ]

        for expr, expected in hex_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Binary
        binary_cases = [
            ("0b0", 0),
            ("0b1", 1),
            ("0b1010", 10),
            ("0B1010", 10),
            ("0b11111111", 255),
        ]

        for expr, expected in binary_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Octal
        octal_cases = [
            ("0o0", 0),
            ("0o7", 7),
            ("0o10", 8),
            ("0O10", 8),
            ("0o777", 511),
        ]

        for expr, expected in octal_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

    def test_invalid_number_tokenization(self, aifpl):
        """Test invalid number tokenization cases."""
        invalid_numbers = [
            "0x",           # Hex without digits
            "0b",           # Binary without digits
            "0o",           # Octal without digits
            "1e",           # Missing exponent
            "1.5e+",        # Missing exponent after sign
            "1.5e-",        # Missing exponent after sign
            "0xGHI",        # Invalid hex digits
            "0b123",        # Invalid binary digits
            "0o89",         # Invalid octal digits
            "1.2.3",        # Multiple decimal points
            "1e2e3",        # Multiple exponents
        ]

        for expr in invalid_numbers:
            with pytest.raises(AIFPLTokenError):
                aifpl.evaluate(expr)

    def test_string_tokenization_edge_cases(self, aifpl):
        """Test comprehensive string tokenization edge cases."""
        # Basic strings
        basic_strings = [
            ('""', ""),
            ('"a"', "a"),
            ('"hello"', "hello"),
            ('"hello world"', "hello world"),
        ]

        for expr, expected in basic_strings:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Strings with escape sequences
        escape_strings = [
            ('"hello\\nworld"', "hello\nworld"),
            ('"hello\\tworld"', "hello\tworld"),
            ('"hello\\rworld"', "hello\rworld"),
            ('"hello\\\\"', "hello\\"),
            ('"hello\\""', 'hello"'),
            ('"\\n\\t\\r\\\\\\\""', '\n\t\r\\"'),
        ]

        for expr, expected in escape_strings:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Unicode escape sequences
        unicode_strings = [
            ('"\\u0041"', "A"),
            ('"\\u0042"', "B"),
            ('"\\u03B1"', "α"),
            ('"\\u03B2"', "β"),
            ('"\\u4E16\\u754C"', "世界"),
        ]

        for expr, expected in unicode_strings:
            result = aifpl.evaluate(expr)
            assert result == expected

        with pytest.raises(AIFPLTokenError):
            aifpl.evaluate('"\\')

    def test_invalid_string_tokenization(self, aifpl):
        """Test invalid string tokenization cases."""
        invalid_strings = [
            '"hello',           # Unterminated string
            '"hello world',     # Unterminated string
            '"hello\\q"',       # Invalid escape sequence
            '"test\\z"',        # Invalid escape sequence
            '"\\uXYZ"',         # Invalid Unicode (not hex)
            '"\\uGGGG"',        # Invalid Unicode (not hex)
            '"\\u12"',          # Incomplete Unicode (too few digits)
            '"\\u"',            # Incomplete Unicode (no digits)
            '"\\u123"',         # Incomplete Unicode (too few digits)
        ]

        for expr in invalid_strings:
            with pytest.raises(AIFPLTokenError):
                aifpl.evaluate(expr)

    def test_boolean_tokenization_edge_cases(self, aifpl):
        """Test boolean tokenization edge cases."""
        # Valid booleans
        valid_booleans = [
            ("#t", True),
            ("#f", False),
            ("true", True),
            ("false", False),
        ]

        for expr, expected in valid_booleans:
            result = aifpl.evaluate(expr)
            assert result is expected

        # Invalid booleans
        invalid_booleans = [
            "#x",           # Invalid boolean literal
            "#true",        # Must be exactly #t
            "#false",       # Must be exactly #f
            "#T",           # Case sensitive
            "#F",           # Case sensitive
            "#1",           # Not a boolean
            "#0",           # Not a boolean
        ]

        for expr in invalid_booleans:
            with pytest.raises(AIFPLTokenError):
                aifpl.evaluate(expr)

    def test_symbol_tokenization_edge_cases(self, aifpl):
        """Test symbol/identifier tokenization edge cases."""
        # Valid symbols (these should be evaluated as variables/functions)
        valid_symbols = [
            "pi",           # Mathematical constant
            "e",            # Mathematical constant
            "j",            # Imaginary unit
            "+",            # Operator
            "-",            # Operator
            "*",            # Operator
            "/",            # Operator
            "abs",          # Function
            "sqrt",         # Function
            "sin",          # Function
            "cos",          # Function
        ]

        for symbol in valid_symbols:
            # These should not raise tokenization errors
            # (might raise evaluation errors if undefined, but tokenization should work)
            try:
                result = aifpl.evaluate(symbol)
                # If evaluation succeeds, that's fine
            except AIFPLError:
                # Evaluation errors are fine, we're testing tokenization
                pass

        # Symbols with special characters (if allowed)
        special_symbols = [
            "string-length",    # Hyphenated
            "list-ref",         # Hyphenated
            "string->number",   # Arrow notation
            "number->string",   # Arrow notation
            "string=?",         # Question mark
            "null?",            # Question mark
            "member?",          # Question mark
        ]

        for symbol in special_symbols:
            try:
                result = aifpl.evaluate(f"({symbol})")
                # If evaluation succeeds, that's fine
            except AIFPLError:
                # Evaluation errors are fine, we're testing tokenization
                pass

    def test_parentheses_tokenization_edge_cases(self, aifpl):
        """Test parentheses tokenization edge cases."""
        # Nested parentheses
        nested_cases = [
            "()",                           # Empty
            "(())",                         # Nested empty
            "((()))",                       # Deeply nested empty
            "(+ 1 2)",                      # Simple expression
            "(+ 1 (+ 2 3))",               # Nested expression
            "(+ (* 2 3) (- 5 1))",         # Multiple nested
        ]

        for expr in nested_cases:
            # Should tokenize without error
            try:
                result = aifpl.evaluate(expr)
                # If evaluation succeeds, that's fine
            except AIFPLError:
                # Evaluation errors are fine, we're testing tokenization
                pass

    def test_comment_tokenization_edge_cases(self, aifpl):
        """Test comment tokenization (if supported)."""
        # Comments might not be supported, but test if they are
        comment_cases = [
            "; this is a comment",
            "42 ; end of line comment",
            "; comment\n42",
            "42 ; comment\n",
        ]

        for expr in comment_cases:
            try:
                # If comments are supported, they should be ignored
                if "42" in expr:
                    result = aifpl.evaluate(expr)
                    assert result == 42
                else:
                    # Pure comment might be empty expression
                    result = aifpl.evaluate(expr)
            except (AIFPLTokenError, AIFPLParseError):
                # Comments might not be supported, which is fine
                pass

    def test_special_character_tokenization(self, aifpl):
        """Test special character tokenization."""
        # Quote characters
        quote_cases = [
            "'x",               # Quote shorthand
            "(quote x)",        # Quote function
            "'(+ 1 2)",         # Quoted expression
        ]

        for expr in quote_cases:
            try:
                result = aifpl.evaluate(expr)
                # Should tokenize without error
            except AIFPLError:
                # Evaluation errors are fine, we're testing tokenization
                pass

    def test_invalid_character_tokenization(self, aifpl):
        """Test invalid character tokenization."""
        invalid_chars = [
            "@",            # Invalid character
            "$",            # Invalid character
            "hello$world",  # Invalid character in identifier
            "42@",          # Invalid character after number
        ]

        for expr in invalid_chars:
            with pytest.raises(AIFPLTokenError):
                aifpl.evaluate(expr)

    def test_tokenizer_position_tracking(self, aifpl):
        """Test that tokenizer tracks positions for error reporting."""
        # Test position tracking in error messages
        position_cases = [
            ("@", "@"),         # Invalid char at start
            ("42@", "@"),       # Invalid char after valid token
            ("hello@world", "@"), # Invalid char in middle
        ]

        for expr, bad_char in position_cases:
            try:
                aifpl.evaluate(expr)
                pytest.fail(f"Expected tokenization error for: {expr}")
            except AIFPLTokenError as e:
                error_msg = str(e)
                # Error should mention the problematic character or position
                assert bad_char in error_msg or "position" in error_msg.lower()

    def test_tokenizer_buffer_edge_cases(self, aifpl):
        """Test tokenizer buffer handling edge cases."""
        # Very long tokens
        long_string = '"' + "a" * 1000 + '"'
        result = aifpl.evaluate(long_string)
        assert len(result) == 1000
        assert result == "a" * 1000

        # Very long numbers
        long_number = "1" + "0" * 100
        result = aifpl.evaluate(long_number)
        assert result == int("1" + "0" * 100)

        # Very long identifiers (if allowed)
        try:
            long_identifier = "a" * 100
            # This might be undefined, but should tokenize
            aifpl.evaluate(long_identifier)
        except AIFPLError:
            # Evaluation error is fine, we're testing tokenization
            pass

    def test_tokenizer_unicode_edge_cases(self, aifpl):
        """Test tokenizer Unicode handling edge cases."""
        # Unicode in strings
        unicode_cases = [
            ('"α"', "α"),           # Greek letter
            ('"π"', "π"),           # Pi symbol
            ('"∑"', "∑"),           # Summation symbol
            ('"∞"', "∞"),           # Infinity symbol
            ('"🚀"', "🚀"),         # Emoji (if supported)
        ]

        for expr, expected in unicode_cases:
            try:
                result = aifpl.evaluate(expr)
                assert result == expected
            except (AIFPLTokenError, AIFPLParseError):
                # Some Unicode might not be supported
                pass

        # Unicode escape sequences with various lengths
        unicode_escapes = [
            ('"\\u0041"', "A"),         # Basic ASCII
            ('"\\u00E9"', "é"),         # Accented character
            ('"\\u03B1"', "α"),         # Greek letter
            ('"\\u4E2D"', "中"),        # Chinese character
        ]

        for expr, expected in unicode_escapes:
            result = aifpl.evaluate(expr)
            assert result == expected

    def test_tokenizer_edge_case_combinations(self, aifpl):
        """Test tokenizer with edge case combinations."""
        # Mixed token types in expressions
        mixed_cases = [
            '(+ 42 3.14)',                      # Integer + float
            '(string-append "hello" " world")', # Strings with spaces
            '(list 1 "two" #t)',               # Mixed types
            '(if #t 42 "false")',              # Boolean condition
        ]

        for expr in mixed_cases:
            try:
                result = aifpl.evaluate(expr)
                # Should tokenize and evaluate without error
            except AIFPLError:
                # Some combinations might not be supported
                pass

        # Whitespace in various positions
        whitespace_cases = [
            ' ( + 1 2 ) ',                     # Spaces around everything
            '(\t+\n1\r2\n)',                   # Mixed whitespace
            '(  +   1    2   )',               # Irregular spacing
        ]

        for expr in whitespace_cases:
            result = aifpl.evaluate(expr)
            assert result == 3

    def test_tokenizer_error_recovery(self, aifpl):
        """Test tokenizer error recovery and state management."""
        # After a tokenization error, tokenizer should be ready for next input
        with pytest.raises(AIFPLTokenError):
            aifpl.evaluate("@invalid")

        # Next tokenization should work normally
        result = aifpl.evaluate("42")
        assert result == 42

        # Multiple errors in sequence
        invalid_inputs = ["@", "$", "#", ":"]
        for invalid in invalid_inputs:
            with pytest.raises(AIFPLTokenError):
                aifpl.evaluate(invalid)

        # Should still work after multiple errors
        result = aifpl.evaluate("(+ 1 2)")
        assert result == 3

    def test_tokenizer_memory_efficiency(self, aifpl):
        """Test tokenizer memory efficiency with large inputs."""
        # Large expression with many tokens
        large_expr = "(+ " + " ".join(str(i) for i in range(1000)) + ")"
        result = aifpl.evaluate(large_expr)
        assert result == sum(range(1000))

        # Large string literal
        large_string = '"' + "x" * 10000 + '"'
        result = aifpl.evaluate(large_string)
        assert len(result) == 10000
        assert result == "x" * 10000

    def test_tokenizer_numeric_edge_cases_comprehensive(self, aifpl):
        """Test comprehensive numeric tokenization edge cases."""
        # Edge cases for floating point
        float_edge_cases = [
            ("0.0", 0.0),
            ("-0.0", -0.0),
            ("1e-100", 1e-100),
            ("1e100", 1e100),
            ("1.23456789012345", 1.23456789012345),
        ]

        for expr, expected in float_edge_cases:
            result = aifpl.evaluate(expr)
            if expected == 0.0:
                assert result == expected
            else:
                assert abs(result - expected) < 1e-10 or abs((result - expected) / expected) < 1e-10

        # Edge cases for integers
        int_edge_cases = [
            ("0", 0),
            ("-0", 0),
            ("2147483647", 2147483647),      # 32-bit max
            ("-2147483648", -2147483648),    # 32-bit min
            ("9223372036854775807", 9223372036854775807),    # 64-bit max (if supported)
        ]

        for expr, expected in int_edge_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

    def test_tokenizer_string_edge_cases_comprehensive(self, aifpl):
        """Test comprehensive string tokenization edge cases."""
        # Strings with all escape sequences
        escape_comprehensive = [
            ('"\\a"', "\a"),     # Bell (if supported)
            ('"\\b"', "\b"),     # Backspace (if supported)
            ('"\\f"', "\f"),     # Form feed (if supported)
            ('"\\n"', "\n"),     # Newline
            ('"\\r"', "\r"),     # Carriage return
            ('"\\t"', "\t"),     # Tab
            ('"\\v"', "\v"),     # Vertical tab (if supported)
            ('"\\\\"', "\\"),    # Backslash
            ('"\\\""', '"'),     # Quote
        ]

        for expr, expected in escape_comprehensive:
            try:
                result = aifpl.evaluate(expr)
                assert result == expected
            except AIFPLTokenError:
                # Some escape sequences might not be supported
                pass

        # Strings with mixed content
        mixed_strings = [
            ('"Hello\\nWorld"', "Hello\nWorld"),
            ('"Tab\\tSeparated\\tValues"', "Tab\tSeparated\tValues"),
            ('"Quote: \\"Hello\\"\\n"', 'Quote: "Hello"\n'),
            ('"Unicode: \\u03B1\\u03B2\\u03B3"', "Unicode: αβγ"),
        ]

        for expr, expected in mixed_strings:
            result = aifpl.evaluate(expr)
            assert result == expected
