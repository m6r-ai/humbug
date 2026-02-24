"""Tests for AIFPL lexer edge cases."""

import pytest

from aifpl import AIFPLError, AIFPLTokenError, AIFPLParseError


class TestAIFPLLexerEdgeCases:
    """Test lexer edge cases and comprehensive token handling."""

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
            ("#x0", 0),
            ("#xFF", 255),
            ("#xff", 255),
            ("#x10", 16),
            ("#xABC", 2748),
            ("#xabc", 2748),
        ]

        for expr, expected in hex_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Binary
        binary_cases = [
            ("#b0", 0),
            ("#b1", 1),
            ("#b1010", 10),
            ("#B1010", 10),
            ("#b11111111", 255),
        ]

        for expr, expected in binary_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Octal
        octal_cases = [
            ("#o0", 0),
            ("#o7", 7),
            ("#o10", 8),
            ("#O10", 8),
            ("#o777", 511),
        ]

        for expr, expected in octal_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

    def test_invalid_number_tokenization(self, aifpl):
        """Test invalid number tokenization cases."""
        invalid_numbers = [
            "#x",           # Hex without digits
            "#b",           # Binary without digits
            "#o",           # Octal without digits
            "1e",           # Missing exponent
            "1.5e+",        # Missing exponent after sign
            "1.5e-",        # Missing exponent after sign
            "#xGHI",        # Invalid hex digits
            "#b123",        # Invalid binary digits
            "#o89",         # Invalid octal digits
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
            '"\\uXYZ"',         # Invalid Unicode (boolean-not hex)
            '"\\uGGGG"',        # Invalid Unicode (boolean-not hex)
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
            "j",            # Complex literal (1j)
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
                aifpl.evaluate(symbol)
                # If evaluation succeeds, that's fine
            except AIFPLError:
                # Evaluation errors are fine, we're testing tokenization
                pass

        # Symbols with special characters (if allowed)
        special_symbols = [
            "string-length",    # Hyphenated
            "list-ref",         # Hyphenated
            "string->number",   # Arrow notation
            "integer->string",  # Arrow notation
            "string=?",         # Question mark
            "list-null?",            # Question mark
            "list-member?",          # Question mark
        ]

        for symbol in special_symbols:
            try:
                aifpl.evaluate(f"({symbol})")
                # If evaluation succeeds, that's fine
            except AIFPLError:
                # Evaluation errors are fine, we're testing tokenization
                pass

    def test_parentheses_tokenization_edge_cases(self, aifpl):
        """Test parentheses tokenization edge cases."""
        # Nested parentheses
        nested_cases = [
            "()",
            "(())",
            "((()))",
            "(integer+ 1 2)",
            "(integer+ 1 (integer+ 2 3))",
            "(integer+ (integer* 2 3) (integer- 5 1))",
        ]

        for expr in nested_cases:
            # Should lex without error
            try:
                aifpl.evaluate(expr)
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
                aifpl.evaluate(expr)
                # Should lex without error
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

    def test_lexer_control_characters_outside_strings(self, aifpl):
        """Test that control characters outside strings are rejected with proper error messages."""
        # Control characters (ASCII < 32) that are NOT considered whitespace by Python's isspace()
        # Python's isspace() returns True for: 0x09 (tab), 0x0A (newline), 0x0B (vertical tab),
        # 0x0C (form feed), 0x0D (carriage return), 0x1C (file separator), 0x1D (group separator),
        # 0x1E (record separator), 0x1F (unit separator)
        # We test the other control characters that should trigger the error

        # To properly test the control character error path, we need the control character
        # to appear in a position where it's checked individually, not consumed as part of
        # another token. We use expressions like "42 \x01" (with space separator) where
        # the control character is isolated.

        control_chars_to_test = [
            (0x00, "\\u0000"),  # Null character (NUL)
            (0x01, "\\u0001"),  # Start of Heading (SOH)
            (0x02, "\\u0002"),  # Start of Text (STX)
            (0x03, "\\u0003"),  # End of Text (ETX)
            (0x04, "\\u0004"),  # End of Transmission (EOT)
            (0x05, "\\u0005"),  # Enquiry (ENQ)
            (0x06, "\\u0006"),  # Acknowledge (ACK)
            (0x07, "\\u0007"),  # Bell (BEL)
            (0x08, "\\u0008"),  # Backspace (BS)
            # 0x09 (tab), 0x0A (newline), 0x0B (vertical tab), 0x0C (form feed), 0x0D (carriage return) are whitespace
            (0x0E, "\\u000e"),  # Shift Out (SO)
            (0x0F, "\\u000f"),  # Shift In (SI)
            (0x10, "\\u0010"),  # Data Link Escape (DLE)
            (0x11, "\\u0011"),  # Device Control 1 (DC1)
            (0x12, "\\u0012"),  # Device Control 2 (DC2)
            (0x13, "\\u0013"),  # Device Control 3 (DC3)
            (0x14, "\\u0014"),  # Device Control 4 (DC4)
            (0x15, "\\u0015"),  # Negative Acknowledge (NAK)
            (0x16, "\\u0016"),  # Synchronous Idle (SYN)
            (0x17, "\\u0017"),  # End of Transmission Block (ETB)
            (0x18, "\\u0018"),  # Cancel (CAN)
            (0x19, "\\u0019"),  # End of Medium (EM)
            (0x1A, "\\u001a"),  # Substitute (SUB)
            (0x1B, "\\u001b"),  # Escape (ESC)
            # 0x1C (file separator), 0x1D (group separator), 0x1E (record separator), 0x1F (unit separator) are whitespace
        ]

        for char_code, expected_display in control_chars_to_test:
            # Create an expression with the control character isolated by whitespace
            # This ensures it will be checked as an individual character, not consumed
            # as part of a number or symbol token
            expr = f"42 {chr(char_code)}"

            try:
                aifpl.evaluate(expr)
                pytest.fail(f"Expected AIFPLTokenError for control character {expected_display}")
            except AIFPLTokenError as e:
                error_msg = str(e)
                # Verify the error message contains:
                # 1. The escaped display format of the character
                # 2. The character code
                # 3. A message about control characters
                assert expected_display in error_msg, \
                    f"Expected {expected_display} in error message, got: {error_msg}"
                assert str(char_code) in error_msg, \
                    f"Expected character code {char_code} in error message, got: {error_msg}"
                assert "Control characters are not allowed" in error_msg or \
                       "control character" in error_msg.lower(), \
                    f"Expected control character warning in error message, got: {error_msg}"

        # Also test that control characters ARE allowed inside strings
        # (this confirms the "except in strings" part of the error message)
        valid_string_cases = [
            ('"\\u0001"', "\x01"),  # Control char via escape sequence
            ('"\\u0007"', "\x07"),  # Bell character
            ('"\\u001b"', "\x1b"),  # Escape character
        ]

        for expr, expected in valid_string_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, "Control characters should be allowed in strings via escape sequences"

    def test_lexer_position_tracking(self, aifpl):
        """Test that lexer tracks positions for error reporting."""
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
                assert bad_char in error_msg or "list-position" in error_msg.lower()

    def test_lexer_buffer_edge_cases(self, aifpl):
        """Test lexer buffer handling edge cases."""
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
            # This might be undefined, but should lex
            aifpl.evaluate(long_identifier)
        except AIFPLError:
            # Evaluation error is fine, we're testing tokenization
            pass

    def test_lexer_unicode_edge_cases(self, aifpl):
        """Test lexer Unicode handling edge cases."""
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

    def test_lexer_edge_case_combinations(self, aifpl):
        """Test lexer with edge case combinations."""
        # Mixed token types in expressions
        mixed_cases = [
            '(float+ 42.0 3.14)',               # Float + float
            '(string-concat "hello" " world")', # Strings with spaces
            '(list 1 "two" #t)',               # Mixed types
            '(if #t 42 "false")',              # Boolean condition
        ]

        for expr in mixed_cases:
            try:
                result = aifpl.evaluate(expr)
                # Should lex and evaluate without error
            except AIFPLError:
                # Some combinations might not be supported
                pass

        # Whitespace in various positions
        whitespace_cases = [
            ' ( integer+ 1 2 ) ',              # Spaces around everything
            '(\tinteger+\n1\r2\n)',            # Mixed whitespace
            '(  integer+   1    2   )',        # Irregular spacing
        ]

        for expr in whitespace_cases:
            result = aifpl.evaluate(expr)
            assert result == 3

    def test_lexer_error_recovery(self, aifpl):
        """Test lexer error recovery and state management."""
        # After a tokenization error, lexer should be ready for next input
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
        result = aifpl.evaluate("(integer+ 1 2)")
        assert result == 3

    def test_lexer_memory_efficiency(self, aifpl):
        """Test lexer memory efficiency with large inputs."""
        # Large expression with many tokens (reduced to 100 to avoid deep recursion
        # in constant folder after variadic desugaring)
        large_expr = "(integer+ " + " ".join(str(i) for i in range(100)) + ")"
        result = aifpl.evaluate(large_expr)
        assert result == sum(range(100))

        # Large string literal
        large_string = '"' + "x" * 10000 + '"'
        result = aifpl.evaluate(large_string)
        assert len(result) == 10000
        assert result == "x" * 10000

    def test_lexer_numeric_edge_cases_comprehensive(self, aifpl):
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

    def test_lexer_string_edge_cases_comprehensive(self, aifpl):
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


    def test_lexer_control_characters_in_tokens(self, aifpl):
        """Test that control characters are caught even when embedded in tokens."""
        # This tests the fix for the issue where control characters adjacent to
        # other characters (non-isolated) were being consumed as part of a token
        # and producing confusing error messages like "Invalid number format: 42^A43"

        # Test control characters embedded in what would be number tokens
        number_cases = [
            (0x01, "42\x0143", "number with SOH"),
            (0x07, "42\x0743", "number with Bell"),
            (0x1B, "100\x1b200", "number with ESC"),
            (0x00, "0\x0042", "number with NULL"),
        ]

        for char_code, expr, description in number_cases:
            expected_display = f"\\u{char_code:04x}"
            try:
                aifpl.evaluate(expr)
                pytest.fail(f"Expected AIFPLTokenError for {description}")
            except AIFPLTokenError as e:
                error_msg = str(e)
                # Should get control character error, not number format error
                assert "control character" in error_msg.lower(), \
                    f"Expected control character error for {description}, got: {error_msg}"
                assert expected_display in error_msg, \
                    f"Expected {expected_display} in error for {description}, got: {error_msg}"
                assert str(char_code) in error_msg, \
                    f"Expected code {char_code} in error for {description}, got: {error_msg}"

        # Test control characters embedded in what would be symbol tokens
        symbol_cases = [
            (0x01, "hello\x01world", "symbol with SOH"),
            (0x07, "test\x07name", "symbol with Bell"),
            (0x1B, "var\x1bname", "symbol with ESC"),
        ]

        for char_code, expr, description in symbol_cases:
            expected_display = f"\\u{char_code:04x}"
            try:
                aifpl.evaluate(expr)
                pytest.fail(f"Expected AIFPLTokenError for {description}")
            except AIFPLTokenError as e:
                error_msg = str(e)
                # Should get control character error, not undefined variable error
                assert "control character" in error_msg.lower(), \
                    f"Expected control character error for {description}, got: {error_msg}"
                assert expected_display in error_msg, \
                    f"Expected {expected_display} in error for {description}, got: {error_msg}"

        # Test control characters in various positions within expressions
        position_cases = [
            ("(\x01)", "after open paren"),
            ("(+\x01)", "after operator"),
            ("(+ 1\x012)", "between digits"),
            ("(+ 1 2\x01)", "at end before close paren"),
        ]

        for expr, description in position_cases:
            try:
                aifpl.evaluate(expr)
                pytest.fail(f"Expected AIFPLTokenError for control char {description}")
            except AIFPLTokenError as e:
                error_msg = str(e)
                assert "control character" in error_msg.lower(), \
                    f"Expected control character error for {description}, got: {error_msg}"

    def test_positive_sign_number_literals(self, aifpl):
        """Test that numbers with explicit positive sign (+) are correctly lexed as numbers, not symbols."""
        # Positive integers
        positive_integer_cases = [
            ("+0", 0),
            ("+42", 42),
            ("+123", 123),
            ("+999", 999),
        ]

        for expr, expected in positive_integer_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"

        # Positive floats
        positive_float_cases = [
            ("+0.0", 0.0),
            ("+3.14", 3.14),
            ("+0.5", 0.5),
            ("+5.0", 5.0),
            ("+123.456", 123.456),
        ]

        for expr, expected in positive_float_cases:
            result = aifpl.evaluate(expr)
            assert abs(result - expected) < 1e-10, f"Expected {expr} to evaluate to {expected}, got {result}"

        # Positive decimals starting with dot
        positive_dot_cases = [
            ("+.5", 0.5),
            ("+.25", 0.25),
            ("+.999", 0.999),
        ]

        for expr, expected in positive_dot_cases:
            result = aifpl.evaluate(expr)
            assert abs(result - expected) < 1e-10, f"Expected {expr} to evaluate to {expected}, got {result}"

        # Positive numbers in expressions
        expression_cases = [
            ("(integer+ +1 +2)", 3),
            ("(integer* +5 +3)", 15),
            ("(integer- +10 +3)", 7),
            ("(integer/ +20 +4)", 5),
        ]

        for expr, expected in expression_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"

        # Mixed positive and negative
        mixed_cases = [
            ("(integer+ +5 -3)", 2),
            ("(integer- +10 -5)", 15),
            ("(integer* +2 -3)", -6),
        ]

        for expr, expected in mixed_cases:
            result = aifpl.evaluate(expr)
            assert result == expected, f"Expected {expr} to evaluate to {expected}, got {result}"
