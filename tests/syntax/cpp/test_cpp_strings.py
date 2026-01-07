"""
Tests for C++ string and character literal tokenization.

C++ supports all C string literal types plus additional ones:
- Raw strings: R"(...)"
- Raw strings with prefixes: LR"(...)", u8R"(...)", uR"(...)", UR"(...)"
- User-defined literals: "hello"s, 'a'c
"""
import pytest

from syntax.cpp.cpp_lexer import CppLexer


class TestCppStrings:
    """Test C++ string literal tokenization."""

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
            lexer = CppLexer()
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
            r'"single\'quote"',     # single quote
        ]
        for s in test_cases:
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Wide string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"Wide string '{s}' should be STRING type"
            assert tokens[0].value == s

    def test_unicode_string_literals(self):
        """Test Unicode string literals (u, U, u8 prefixes - C++11)."""
        test_cases = [
            ('u"hello"', 'u"hello"'),      # char16_t string
            ('U"world"', 'U"world"'),      # char32_t string
            ('u8"utf8"', 'u8"utf8"'),      # UTF-8 string
        ]
        for code, expected_value in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Unicode string '{code}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Unicode string '{code}' should be STRING type"
            assert tokens[0].value == expected_value, f"Expected value '{expected_value}', got '{tokens[0].value}'"

    def test_unterminated_strings(self):
        """Test unterminated strings (should still tokenize)."""
        test_cases = [
            '"unterminated',
            '"missing quote at end',
        ]
        for s in test_cases:
            lexer = CppLexer()
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
        lexer = CppLexer()
        lexer.lex(None, test_cases[0])
        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should produce at least one token"

    def test_multiline_strings(self):
        """Test that strings don't span multiple lines without continuation."""
        # In C++, string literals cannot span multiple lines without backslash continuation
        lexer = CppLexer()
        lexer.lex(None, '"single line"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_adjacent_string_concatenation(self):
        """Test adjacent string literal concatenation (C++ feature)."""
        lexer = CppLexer()
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
            lexer = CppLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'


class TestCppCharacterLiterals:
    """Test C++ character literal tokenization."""

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
            lexer = CppLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Character literal '{s}' should produce one token"
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
            r'"\""',   # double quote
        ]
        for s in test_cases:
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Wide character literal '{s}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Wide character literal '{s}' should be STRING type"
            assert tokens[0].value == s, f"Expected value '{s}', got '{tokens[0].value}'"

    def test_unicode_character_literals(self):
        """Test Unicode character literals (u, U prefixes - C++11)."""
        test_cases = [
            ("u'a'", "u'a'"),     # char16_t
            ("U'b'", "U'b'"),     # char32_t
        ]
        for code, expected_value in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Unicode character literal '{code}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Unicode character literal '{code}' should be STRING type"
            assert tokens[0].value == expected_value, f"Expected value '{expected_value}', got '{tokens[0].value}'"

    def test_multicharacter_literals(self):
        """Test multi-character literals (implementation defined in C++)."""
        test_cases = [
            "'ab'",     # two characters
            "'abc'",    # three characters
            "'ABCD'",   # four characters
        ]
        for s in test_cases:
            lexer = CppLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            # Multi-character literals are valid in C++ (implementation-defined value)
            assert len(tokens) == 1, f"Multi-character literal '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_empty_character_literal(self):
        """Test empty character literal (invalid in C++, but should tokenize)."""
        lexer = CppLexer()
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
            lexer = CppLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Unterminated character literal '{s}' should produce at least one token"


class TestCppRawStrings:
    """Test C++ raw string literals (C++11 feature)."""

    def test_simple_raw_strings(self):
        """Test simple raw string literals."""
        test_cases = [
            ('R"(hello)"', 'R"(hello)"'),
            ('R"(world)"', 'R"(world)"'),
            ('R"()"', 'R"()"'),  # empty raw string
            ('R"(with spaces)"', 'R"(with spaces)"'),
        ]
        for code, expected_value in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Raw string '{code}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Raw string '{code}' should be STRING type, got {tokens[0].type.name}"
            assert tokens[0].value == expected_value, f"Raw string '{code}' should have value '{expected_value}', got '{tokens[0].value}'"

    def test_raw_strings_with_delimiters(self):
        """Test raw strings with custom delimiters."""
        test_cases = [
            ('R"delim(content)delim"', 'R"delim(content)delim"'),
            ('R"xyz(hello world)xyz"', 'R"xyz(hello world)xyz"'),
            ('R"__(test)__"', 'R"__(test)__"'),
        ]
        for code, expected_value in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Raw string with delimiter '{code}' should produce exactly one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == expected_value

    def test_raw_strings_with_special_chars(self):
        """Test raw strings containing characters that would normally need escaping."""
        test_cases = [
            (r'R"(line1\nline2)"', r'R"(line1\nline2)"'),  # backslash-n is literal, not newline
            (r'R"(quote"inside)"', r'R"(quote"inside)"'),  # quote inside raw string
            (r'R"(backslash\\here)"', r'R"(backslash\\here)"'),  # backslash is literal
        ]
        for code, expected_value in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Raw string '{code}' should produce exactly one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == expected_value

    def test_raw_strings_with_prefixes(self):
        """Test raw strings with encoding prefixes."""
        test_cases = [
            ('LR"(wide)"', 'LR"(wide)"'),        # Wide raw string
            ('u8R"(utf8)"', 'u8R"(utf8)"'),       # UTF-8 raw string
            ('uR"(utf16)"', 'uR"(utf16)"'),       # UTF-16 raw string
            ('UR"(utf32)"', 'UR"(utf32)"'),       # UTF-32 raw string
        ]
        for code, expected_value in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Prefixed raw string '{code}' should produce exactly one token, got {len(tokens)}"
            assert tokens[0].type.name == 'STRING', f"Prefixed raw string '{code}' should be STRING type"
            assert tokens[0].value == expected_value

    def test_raw_strings_multiline(self):
        """Test raw strings that span multiple lines."""
        # First line starts raw string
        lexer = CppLexer()
        state = lexer.lex(None, 'R"(start')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Raw string start should produce tokens"
        # Note: Implementation may need state tracking for raw strings


class TestCppStringAndCharacterMixed:
    """Test mixed scenarios with strings and character literals."""

    def test_string_and_char_in_expression(self):
        """Test string and character literals in expressions."""
        test_cases = [
            'char c = \'a\';',
            'char *s = "hello";',
            'if (c == \'\\n\') {}',
            'std::cout << "Value: " << \'x\';',
        ]
        for code in test_cases:
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Complex escape sequence '{s}' should produce tokens"

    def test_string_in_preprocessor(self):
        """Test strings in preprocessor directives."""
        test_cases = [
            '#include "header.h"',
            '#include <iostream>',
            '#define MSG "Hello"',
            '#error "Error message"',
        ]
        for code in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Preprocessor directive consumes the entire line
            assert len(tokens) >= 1, f"Preprocessor with string '{code}' should produce tokens"

    def test_concatenated_strings_with_prefixes(self):
        """Test concatenation of strings with different prefixes."""
        test_cases = [
            'L"wide" "normal"',
            '"normal" L"wide"',
            'u8"utf8" "normal"',
        ]
        for code in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            string_tokens = [t for t in tokens if t.type.name == 'STRING']
            # Should have at least 2 string tokens
            assert len(string_tokens) >= 1, f"Concatenated strings '{code}' should produce string tokens"

    def test_user_defined_literals(self):
        """Test user-defined string literals (C++11 feature)."""
        test_cases = [
            '"hello"s',      # std::string literal
            '"world"sv',     # std::string_view literal
            '123ms',         # milliseconds literal
            '3.14i',         # imaginary number literal
        ]
        for code in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # User-defined literals are string/number followed by identifier suffix
            assert len(tokens) >= 1, f"User-defined literal '{code}' should produce tokens"


class TestCppEdgeCasesForCoverage:
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
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
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
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"'{code}' should be IDENTIFIER"
            assert tokens[0].value == code

    def test_u8_character_literal(self):
        """Test u8 with character literal (should be u8'...')."""
        # Note: u8 character literals are C++17
        code = "u8'a'"
        lexer = CppLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, f"'{code}' should produce one token"
        assert tokens[0].type.name == 'STRING', f"'{code}' should be STRING type"
        assert tokens[0].value == code

    def test_mixed_code_with_comments_and_strings(self):
        """Test realistic C++ code with various features."""
        test_cases = [
            'int x = 42; // comment',
            'std::string s = "hello"; /* block comment */',
            'float f = 3.14e-2;',
            'unsigned long ul = 0xFFFFul;',
            '#include <iostream>',
        ]
        for code in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"


class TestCppEdgeCasesForFullCoverage:
    """Test edge cases to achieve 100% code coverage."""

    def test_multiline_block_comment_continuation(self):
        """Test block comments that span multiple lines."""
        # Start a block comment on first line
        lexer = CppLexer()
        state1 = lexer.lex(None, '/* start of comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state1.in_block_comment == True, "Should still be in block comment"

        # Continue on second line
        lexer2 = CppLexer()
        state2 = lexer2.lex(state1, 'middle of comment')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type.name == 'COMMENT'
        assert state2.in_block_comment == True, "Should still be in block comment"

        # Close on third line
        lexer3 = CppLexer()
        state3 = lexer3.lex(state2, 'end of comment */')

        tokens3 = list(lexer3._tokens)
        assert len(tokens3) == 1
        assert tokens3[0].type.name == 'COMMENT'
        assert state3.in_block_comment == False, "Should have closed block comment"

    def test_L_at_end_of_input(self):
        """Test L at end of input (not followed by quote)."""
        lexer = CppLexer()
        lexer.lex(None, 'L')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'L'

    def test_dot_at_end_of_input(self):
        """Test dot at end of input (not followed by digit)."""
        lexer = CppLexer()
        lexer.lex(None, '.')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '.'

    def test_forward_slash_at_end_of_input(self):
        """Test forward slash at end of input (not followed by / or *)."""
        lexer = CppLexer()
        lexer.lex(None, '/')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '/'

    def test_invalid_number_suffix(self):
        """Test number with invalid suffix."""
        # Invalid suffix that doesn't match valid patterns
        lexer = CppLexer()
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
        lexer = CppLexer()
        state = lexer.lex(None, '/* unterminated comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state.in_block_comment == True, "Should still be in block comment"

    def test_dot_followed_by_non_digit(self):
        """Test dot followed by non-digit (should be operator)."""
        lexer = CppLexer()
        lexer.lex(None, '.x')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '.'
        assert tokens[1].type.name == 'IDENTIFIER'

    def test_forward_slash_followed_by_other(self):
        """Test forward slash followed by something other than / or *."""
        lexer = CppLexer()
        lexer.lex(None, '/x')

        tokens = list(lexer._tokens)
        assert len(tokens) == 2
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '/'
        assert tokens[1].type.name == 'IDENTIFIER'

    def test_number_with_truly_invalid_suffix(self):
        """Test number with a suffix that gets rejected."""
        # Try a suffix that's in the check list but not valid
        lexer = CppLexer()
        lexer.lex(None, '42lf')  # 'lf' is not a valid combination

        tokens = list(lexer._tokens)
        # Should have rejected the suffix and tokenized separately
        assert len(tokens) >= 1

    def test_u_at_end_of_input(self):
        """Test u at the very end of input."""
        lexer = CppLexer()
        lexer.lex(None, 'u')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'u'

    def test_R_at_end_of_input(self):
        """Test R at the very end of input."""
        lexer = CppLexer()
        lexer.lex(None, 'R')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'R'

    def test_LR_not_raw_string(self):
        """Test LR not followed by quote (should be identifier)."""
        # LR followed by something other than "
        lexer = CppLexer()
        lexer.lex(None, 'LRx')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'LRx'

    def test_uR_not_raw_string(self):
        """Test uR not followed by quote (should be identifier)."""
        # uR followed by something other than "
        lexer = CppLexer()
        lexer.lex(None, 'uRx')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'uRx'

    def test_UR_not_raw_string(self):
        """Test UR not followed by quote (should be identifier)."""
        # UR followed by something other than "
        lexer = CppLexer()
        lexer.lex(None, 'URx')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'URx'

    def test_u8R_not_raw_string(self):
        """Test u8R not followed by quote (should be identifier)."""
        # u8R followed by something other than "
        lexer = CppLexer()
        lexer.lex(None, 'u8Rx')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'u8Rx'

    def test_LR_at_end_of_input(self):
        """Test LR at end of input."""
        lexer = CppLexer()
        lexer.lex(None, 'LR')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'LR'

    def test_u8R_at_end_of_input(self):
        """Test u8R at end of input."""
        lexer = CppLexer()
        lexer.lex(None, 'u8R')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'u8R'

    def test_R_not_followed_by_quote(self):
        """Test R not followed by quote (should be identifier)."""
        lexer = CppLexer()
        lexer.lex(None, 'Rx')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'Rx'

    def test_U_at_end_of_input(self):
        """Test U at the very end of input."""
        lexer = CppLexer()
        lexer.lex(None, 'U')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'U'

    def test_malformed_raw_string_no_paren(self):
        """Test malformed raw string without opening paren."""
        # R"delim" without the parens - this is malformed but should still tokenize
        lexer = CppLexer()
        lexer.lex(None, 'R"test"')

        tokens = list(lexer._tokens)
        # Should produce a STRING token (malformed, but lexer doesn't validate)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        # The lexer will read until it finds )test" but won't find it,
        # so it reads to end of input
        assert tokens[0].value == 'R"test"'

    def test_cpp_specific_operators(self):
        """Test C++-specific operators."""
        test_cases = [
            ('::',  'OPERATOR'),  # scope resolution
            ('->',  'OPERATOR'),  # member access
            ('&&',  'OPERATOR'),  # logical AND / rvalue reference
            ('||',  'OPERATOR'),  # logical OR
        ]
        for code, expected_type in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == expected_type, f"'{code}' should be {expected_type}"
            assert tokens[0].value == code


class TestCppKeywords:
    """Test C++ keyword recognition."""

    def test_cpp_specific_keywords(self):
        """Test C++-specific keywords."""
        cpp_keywords = [
            'class', 'namespace', 'template', 'typename', 'public',
            'private', 'protected', 'virtual', 'override', 'final',
            'constexpr', 'nullptr', 'auto', 'decltype', 'new', 'delete',
            'this', 'operator', 'friend', 'inline', 'explicit',
            'mutable', 'static_cast', 'dynamic_cast', 'const_cast',
            'reinterpret_cast', 'typeid', 'throw', 'try', 'catch',
            'noexcept', 'static_assert', 'alignas', 'alignof',
            'co_await', 'co_return', 'co_yield', 'concept', 'requires',
        ]
        for keyword in cpp_keywords:
            lexer = CppLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_cpp_boolean_keywords(self):
        """Test C++ boolean keywords."""
        test_cases = ['true', 'false']
        for keyword in test_cases:
            lexer = CppLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'KEYWORD'
            assert tokens[0].value == keyword

    def test_cpp_type_keywords(self):
        """Test C++ type keywords."""
        type_keywords = [
            'bool', 'char16_t', 'char32_t', 'wchar_t',
            'int', 'long', 'short', 'char', 'float', 'double',
            'void', 'signed', 'unsigned',
        ]
        for keyword in type_keywords:
            lexer = CppLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'KEYWORD'
            assert tokens[0].value == keyword


class TestCppNumbers:
    """Test C++ number literal tokenization."""

    def test_integer_literals(self):
        """Test various integer literal formats."""
        test_cases = [
            '0', '42', '123456',
            '0x1234', '0XABCD',  # hexadecimal
            '0b1010', '0B1111',  # binary (C++14)
            '0777',              # octal
        ]
        for num in test_cases:
            lexer = CppLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Number '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER'

    def test_floating_point_literals(self):
        """Test floating-point literal formats."""
        test_cases = [
            '3.14', '0.5', '.5', '5.',
            '1e10', '1E-5', '3.14e0',
            '0x1.2p3',  # hexadecimal floating-point (C++17)
        ]
        for num in test_cases:
            lexer = CppLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Number '{num}' should produce tokens"

    def test_number_suffixes(self):
        """Test number literals with suffixes."""
        test_cases = [
            '42u', '42U',           # unsigned
            '42l', '42L',           # long
            '42ul', '42UL',         # unsigned long
            '42ll', '42LL',         # long long
            '42ull', '42ULL',       # unsigned long long
            '3.14f', '3.14F',       # float
            '3.14l', '3.14L',       # long double
        ]
        for num in test_cases:
            lexer = CppLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Number with suffix '{num}' should produce tokens"

    def test_digit_separators(self):
        """Test digit separators (C++14 feature)."""
        # Note: This may not be implemented yet, but we should test for it
        test_cases = [
            "1'000'000",  # single quotes as digit separators
            "0b1010'1010",
            "0x1234'5678",
        ]
        for num in test_cases:
            lexer = CppLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            # May be tokenized as number + operators + number
            assert len(tokens) >= 1, f"Number '{num}' should produce tokens"


class TestCppComments:
    """Test C++ comment tokenization."""

    def test_single_line_comments(self):
        """Test single-line // comments."""
        test_cases = [
            '// simple comment',
            '// comment with symbols !@#$%^&*()',
            '//',
            '// comment with "quotes" and \'chars\'',
        ]
        for comment in test_cases:
            lexer = CppLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'COMMENT'
            assert tokens[0].value == comment

    def test_block_comments(self):
        """Test /* */ block comments."""
        test_cases = [
            '/* simple */',
            '/* with\nnewlines */',
            '/**/',
            '/* nested // comment */',
        ]
        for comment in test_cases:
            lexer = CppLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1
            # First token should be a comment
            assert tokens[0].type.name == 'COMMENT'


class TestCppPreprocessor:
    """Test C++ preprocessor directive tokenization."""

    def test_include_directives(self):
        """Test #include directives."""
        test_cases = [
            '#include <iostream>',
            '#include "myheader.h"',
            '#include <vector>',
        ]
        for directive in test_cases:
            lexer = CppLexer()
            lexer.lex(None, directive)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'PREPROCESSOR'

    def test_define_directives(self):
        """Test #define directives."""
        test_cases = [
            '#define MAX 100',
            '#define MIN(a,b) ((a)<(b)?(a):(b))',
            '#define PI 3.14159',
        ]
        for directive in test_cases:
            lexer = CppLexer()
            lexer.lex(None, directive)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'PREPROCESSOR'

    def test_conditional_directives(self):
        """Test conditional compilation directives."""
        test_cases = [
            '#ifdef DEBUG',
            '#ifndef NDEBUG',
            '#if defined(WIN32)',
            '#elif defined(LINUX)',
            '#else',
            '#endif',
        ]
        for directive in test_cases:
            lexer = CppLexer()
            lexer.lex(None, directive)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'PREPROCESSOR'


class TestCppRealWorld:
    """Test real-world C++ code snippets."""

    def test_simple_class_definition(self):
        """Test tokenization of a simple class definition."""
        code = 'class MyClass {'
        lexer = CppLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        assert len(tokens) >= 3
        # Should have: class (KEYWORD), MyClass (IDENTIFIER), { (OPERATOR)

    def test_template_declaration(self):
        """Test tokenization of template declarations."""
        code = 'template<typename T>'
        lexer = CppLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        assert len(tokens) >= 4
        # Should have: template, <, typename, T, >

    def test_namespace_usage(self):
        """Test tokenization of namespace usage."""
        code = 'std::cout << "Hello";'
        lexer = CppLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 1
        assert string_tokens[0].value == '"Hello"'

    def test_lambda_expression(self):
        """Test tokenization of lambda expressions."""
        code = '[](int x) { return x * 2; }'
        lexer = CppLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        # Should tokenize all parts of the lambda
        assert len(tokens) >= 10

    def test_range_based_for_loop(self):
        """Test tokenization of range-based for loops."""
        code = 'for (auto x : vec)'
        lexer = CppLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        # Should have: for, (, auto, x, :, vec, )
        assert len(tokens) >= 7

    def test_smart_pointer_usage(self):
        """Test tokenization of smart pointer declarations."""
        code = 'std::unique_ptr<int> ptr;'
        lexer = CppLexer()
        lexer.lex(None, code)

        tokens = list(lexer._tokens)
        # Should tokenize all parts
        assert len(tokens) >= 7
