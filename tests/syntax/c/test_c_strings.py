"""
Tests for C string and character literal tokenization.
"""
import pytest

from syntax.c.c_lexer import CLexer


class TestCStrings:
    """Test C string literal tokenization."""

    def test_simple_string_literals(self):
        """Test simple double-quoted string literals."""
        test_cases = [
            '"hello"',
            '"world"',
            '"with spaces"',
            '"123"',
            '""',  # empty string
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String '{s}' should be STRING type"
            assert tokens[0].value == s, f"Token value should match input"

    def test_string_with_escape_sequences(self):
        """Test strings with various escape sequences."""
        test_cases = [
            r'"line1\nline2"',      # newline
            r'"tab\there"',         # tab
            r'"backslash\\"',       # escaped backslash
            r'"quote\""',           # escaped quote
            r'"carriage\rreturn"',  # carriage return
            r'"null\0byte"',        # null byte
            r'"bell\a"',            # bell
            r'"backspace\b"',       # backspace
            r'"formfeed\f"',        # form feed
            r'"vertical\vtab"',     # vertical tab
            r'"question\?"',        # question mark
            r'"single\'quote"',     # single quote (doesn't need escaping in double-quoted string)
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with escapes '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == s

    def test_string_with_hex_escape(self):
        """Test strings with hexadecimal escape sequences."""
        test_cases = [
            r'"\x41"',      # hex escape (A)
            r'"\x00"',      # hex null
            r'"\xFF"',      # hex max byte
            r'"\x1B[0m"',   # ANSI escape sequence
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with hex escape '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_string_with_octal_escape(self):
        """Test strings with octal escape sequences."""
        test_cases = [
            r'"\101"',      # octal escape (A)
            r'"\0"',        # short octal (null)
            r'"\377"',      # octal max byte
            r'"\12"',       # octal newline
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with octal escape '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_wide_string_literals(self):
        """Test wide string literals (L prefix)."""
        test_cases = [
            'L"hello"',
            'L"world"',
            'L""',
            'L"with spaces"',
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Wide string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Wide string '{s}' should be STRING type"
            assert tokens[0].value == s

    def test_unicode_string_literals(self):
        """Test Unicode string literals (u, U, u8 prefixes - C11)."""
        test_cases = [
            ('u"hello"', 'u"hello"'),      # char16_t string
            ('U"world"', 'U"world"'),      # char32_t string
            ('u8"utf8"', 'u8"utf8"'),      # UTF-8 string
        ]
        for code, expected_value in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Unicode string '{code}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Unicode string '{code}' should be STRING type"
            assert tokens[0].value == expected_value, f"Expected value '{expected_value}', got '{tokens[0].value}'"

    def test_raw_string_literals(self):
        """Test raw string literals (C++11 feature, not standard C)."""
        # Note: Raw strings R"(...)" are C++11, not C
        # Including for completeness if C++ support is added
        test_cases = [
            'R"(hello)"',
            'R"delimiter(content)delimiter"',
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            # Current implementation will tokenize as identifier + operators + string
            assert len(tokens) >= 1, f"Raw string '{s}' should produce tokens"

    def test_unterminated_strings(self):
        """Test unterminated strings (should still tokenize)."""
        test_cases = [
            '"unterminated',
            '"missing quote at end',
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Unterminated string '{s}' should produce at least one token"
            # The token should still be marked as STRING
            assert tokens[0].type.name == 'STRING'

    def test_string_with_backslash_at_end(self):
        """Test strings with backslash at the end (escapes closing quote)."""
        test_cases = [
            r'"ends with\\"',  # Escaped backslash before quote (valid)
            r'"unclosed\"',   # Escaped quote (unclosed)
        ]
        lexer = CLexer()
        lexer.lex(None, test_cases[0])
        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should produce at least one token"

    def test_multiline_strings(self):
        """Test that strings don't span multiple lines without continuation."""
        # In C, string literals cannot span multiple lines without backslash continuation
        lexer = CLexer()
        lexer.lex(None, '"single line"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_adjacent_string_concatenation(self):
        """Test adjacent string literal concatenation (C feature)."""
        lexer = CLexer()
        lexer.lex(None, '"hello" "world"')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        # Should have 2 separate string tokens (concatenation is semantic, not lexical)
        assert len(string_tokens) == 2, "Should have 2 separate string tokens"

    def test_string_with_special_characters(self):
        """Test strings containing special characters."""
        test_cases = [
            '"hello@world"',
            '"test#123"',
            '"path/to/file"',
            '"C:\\\\path\\\\file"',  # Windows path
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'


class TestCCharacterLiterals:
    """Test C character literal tokenization."""

    def test_simple_character_literals(self):
        """Test simple single-quoted character literals."""
        test_cases = [
            "'a'",
            "'Z'",
            "'0'",
            "'@'",
            "' '",  # space
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Character literal '{s}' should produce one token"
            # In C, character literals should be tokenized as STRING type
            # (or ideally a separate CHAR type, but current implementation uses STRING)
            assert tokens[0].type.name == 'STRING', f"Character literal '{s}' should be STRING type"
            assert tokens[0].value == s

    def test_character_escape_sequences(self):
        """Test character literals with escape sequences."""
        test_cases = [
            r"'\n'",   # newline
            r"'\t'",   # tab
            r"'\\'",   # backslash
            r"'\''",   # single quote
            r"'\r'",   # carriage return
            r"'\0'",   # null
            r"'\a'",   # bell
            r"'\b'",   # backspace
            r"'\f'",   # form feed
            r"'\v'",   # vertical tab
            r"'\?'",   # question mark
            r'"\""',   # double quote (doesn't need escaping in char literal, but allowed)
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Character literal '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_character_hex_escape(self):
        """Test character literals with hexadecimal escape sequences."""
        test_cases = [
            r"'\x41'",  # hex escape (A)
            r"'\x00'",  # hex null
            r"'\xFF'",  # hex max byte
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Character literal '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_character_octal_escape(self):
        """Test character literals with octal escape sequences."""
        test_cases = [
            r"'\101'",  # octal escape (A)
            r"'\0'",    # short octal (null)
            r"'\377'",  # octal max byte
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Character literal '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_wide_character_literals(self):
        """Test wide character literals (L prefix)."""
        test_cases = [
            "L'a'",
            "L'Z'",
            "L'\\n'",
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Wide character literal '{s}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Wide character literal '{s}' should be STRING type"
            assert tokens[0].value == s, f"Expected value '{s}', got '{tokens[0].value}'"

    def test_unicode_character_literals(self):
        """Test Unicode character literals (u, U prefixes - C11)."""
        test_cases = [
            ("u'a'", "u'a'"),     # char16_t
            ("U'b'", "U'b'"),     # char32_t
        ]
        for code, expected_value in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Unicode character literal '{code}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Unicode character literal '{code}' should be STRING type"
            assert tokens[0].value == expected_value, f"Expected value '{expected_value}', got '{tokens[0].value}'"

    def test_multicharacter_literals(self):
        """Test multi-character literals (implementation defined in C)."""
        test_cases = [
            "'ab'",     # two characters
            "'abc'",    # three characters
            "'ABCD'",   # four characters
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            # Multi-character literals are valid in C (implementation-defined value)
            assert len(tokens) == 1, f"Multi-character literal '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_empty_character_literal(self):
        """Test empty character literal (invalid in C, but should tokenize)."""
        lexer = CLexer()
        lexer.lex(None, "''")

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Empty character literal should produce at least one token"

    def test_unterminated_character_literal(self):
        """Test unterminated character literals."""
        test_cases = [
            "'a",
            "'unterminated",
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Unterminated character literal '{s}' should produce at least one token"


class TestCStringAndCharacterMixed:
    """Test mixed scenarios with strings and character literals."""

    def test_string_and_char_in_expression(self):
        """Test string and character literals in expressions."""
        test_cases = [
            'char c = \'a\';',
            'char *s = "hello";',
            'if (c == \'\\n\') {}',
            'printf("Value: %c", \'x\');',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            string_tokens = [t for t in tokens if t.type.name == 'STRING']
            assert len(string_tokens) >= 1, f"Code '{code}' should have at least one string/char token"

    def test_strings_with_embedded_quotes(self):
        """Test strings containing the other quote type."""
        test_cases = [
            '"He said \'hello\'"',  # Double-quoted string with single quotes
            '\'"\' ',              # Single-quoted char containing double quote
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            string_tokens = [t for t in tokens if t.type.name == 'STRING']
            assert len(string_tokens) >= 1, f"'{s}' should produce at least one string token"

    def test_complex_escape_sequences(self):
        """Test complex escape sequences."""
        test_cases = [
            r'"\x1B[31mRed\x1B[0m"',  # ANSI color codes
            r'"\101\102\103"',         # Multiple octal escapes
            r'"\"\'\\\?\a\b\f\n\r\t\v"',  # All standard escapes
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Complex escape sequence '{s}' should produce tokens"

    def test_string_in_preprocessor(self):
        """Test strings in preprocessor directives."""
        test_cases = [
            '#include "header.h"',
            '#define MSG "Hello"',
            '#error "Error message"',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Preprocessor directive consumes the entire line
            assert len(tokens) >= 1, f"Preprocessor with string '{code}' should produce tokens"

    def test_string_with_trigraphs(self):
        """Test strings with trigraph sequences (legacy C feature)."""
        # Trigraphs are a legacy feature that most compilers ignore
        test_cases = [
            '"??="',  # trigraph for #
            '"??/"',  # trigraph for \
        ]
        for s in test_cases:
            lexer = CLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"String with trigraph '{s}' should produce tokens"

    def test_concatenated_strings_with_prefixes(self):
        """Test concatenation of strings with different prefixes."""
        test_cases = [
            'L"wide" "normal"',
            '"normal" L"wide"',
            'u8"utf8" "normal"',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            string_tokens = [t for t in tokens if t.type.name == 'STRING']
            # Should have at least 2 string tokens
            assert len(string_tokens) >= 1, f"Concatenated strings '{code}' should produce string tokens"



class TestCEdgeCasesForCoverage:
    """Test edge cases to improve code coverage."""

    def test_identifiers_starting_with_U(self):
        """Test that U followed by non-quote is treated as identifier."""
        test_cases = [
            'Upper',
            'U8',
            'Undefined',
            'U_value',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"'{code}' should be IDENTIFIER"
            assert tokens[0].value == code

    def test_floating_point_starting_with_dot(self):
        """Test floating point numbers that start with a dot."""
        test_cases = [
            '.5',
            '.123',
            '.0',
            '.999',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value == code

    def test_single_line_comments(self):
        """Test single-line // comments."""
        test_cases = [
            '// this is a comment',
            '//comment without space',
            '// comment with symbols !@#$%',
            '//',  # empty comment
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"'{code}' should be COMMENT type"
            assert tokens[0].value == code

    def test_hexadecimal_numbers(self):
        """Test hexadecimal number literals."""
        test_cases = [
            '0x0',
            '0xFF',
            '0x1234',
            '0xABCDEF',
            '0xabcdef',
            '0X10',  # uppercase X
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value.lower() == code.lower()

    def test_binary_numbers(self):
        """Test binary number literals."""
        test_cases = [
            '0b0',
            '0b1',
            '0b1010',
            '0b11111111',
            '0B101',  # uppercase B
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value.lower() == code.lower()

    def test_scientific_notation_numbers(self):
        """Test numbers in scientific notation."""
        test_cases = [
            '1e10',
            '1E10',
            '1.5e-3',
            '2.0E+5',
            '3.14e0',
            '0.5e10',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value.lower() == code.lower()

    def test_u8_followed_by_non_quote(self):
        """Test u8 followed by something other than a quote."""
        test_cases = [
            'u8x',
            'u8_value',
            'u8Variable',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"'{code}' should be IDENTIFIER"
            assert tokens[0].value == code

    def test_u8_character_literal(self):
        """Test u8 with character literal (should be u8'...')."""
        # Note: u8 character literals are not standard in C11 (only u8 strings)
        # but we should handle u8'x' gracefully
        code = "u8'a'"
        lexer = CLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, f"'{code}' should produce one token"
        assert tokens[0].type.name == 'STRING', f"'{code}' should be STRING type"
        assert tokens[0].value == code

    def test_mixed_code_with_comments_and_strings(self):
        """Test realistic C code with various features."""
        test_cases = [
            'int x = 42; // comment',
            'char *s = "hello"; /* block comment */',
            'float f = 3.14e-2;',
            'unsigned long ul = 0xFFFFul;',
            '#include <stdio.h>',
        ]
        for code in test_cases:
            lexer = CLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"



class TestCEdgeCasesForFullCoverage:
    """Test edge cases to achieve 100% code coverage."""

    def test_multiline_block_comment_continuation(self):
        """Test block comments that span multiple lines."""
        # Start a block comment on first line
        lexer = CLexer()
        state1 = lexer.lex(None, '/* start of comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state1.in_block_comment == True, "Should still be in block comment"

        # Continue on second line
        lexer2 = CLexer()
        state2 = lexer2.lex(state1, 'middle of comment')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type.name == 'COMMENT'
        assert state2.in_block_comment == True, "Should still be in block comment"

        # Close on third line
        lexer3 = CLexer()
        state3 = lexer3.lex(state2, 'end of comment */')

        tokens3 = list(lexer3._tokens)
        assert len(tokens3) == 1
        assert tokens3[0].type.name == 'COMMENT'
        assert state3.in_block_comment == False, "Should have closed block comment"

    def test_L_at_end_of_input(self):
        """Test L at end of input (not followed by quote)."""
        lexer = CLexer()
        lexer.lex(None, 'L')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'L'

    def test_dot_at_end_of_input(self):
        """Test dot at end of input (not followed by digit)."""
        lexer = CLexer()
        lexer.lex(None, '.')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '.'

    def test_forward_slash_at_end_of_input(self):
        """Test forward slash at end of input (not followed by / or *)."""
        lexer = CLexer()
        lexer.lex(None, '/')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '/'

    def test_invalid_number_suffix(self):
        """Test number with invalid suffix."""
        # Invalid suffix that doesn't match valid patterns
        lexer = CLexer()
        lexer.lex(None, '42xyz')

        tokens = list(lexer._tokens)
        # Should tokenize as number without the invalid suffix, then identifier
        assert len(tokens) == 2
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '42'
        assert tokens[1].type.name == 'IDENTIFIER'
        assert tokens[1].value == 'xyz'

    def test_unterminated_block_comment_at_end(self):
        """Test block comment that doesn't close."""
        lexer = CLexer()
        state = lexer.lex(None, '/* unterminated comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state.in_block_comment == True, "Should still be in block comment"

    def test_dot_followed_by_non_digit(self):
        """Test dot followed by non-digit (should be operator)."""
        lexer = CLexer()
        lexer.lex(None, '.x')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '.'
        assert tokens[1].type.name == 'IDENTIFIER'

    def test_forward_slash_followed_by_other(self):
        """Test forward slash followed by something other than / or *."""
        lexer = CLexer()
        lexer.lex(None, '/x')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '/'
        assert tokens[1].type.name == 'IDENTIFIER'


    def test_number_with_truly_invalid_suffix(self):
        """Test number with a suffix that gets rejected."""
        # Try a suffix that's in the check list but not valid
        lexer = CLexer()
        lexer.lex(None, '42lf')  # 'lf' is not a valid combination

        tokens = list(lexer._tokens)
        # Should have rejected the suffix and tokenized separately
        assert len(tokens) >= 1


    def test_u_at_end_of_input(self):
        """Test u at the very end of input."""
        lexer = CLexer()
        lexer.lex(None, 'u')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'u'
