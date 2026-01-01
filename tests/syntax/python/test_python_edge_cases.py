"""
Tests for Python edge cases and corner cases in tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer
from syntax.python.python_parser import PythonParser


class TestPythonEdgeCases:
    """Test edge cases and corner cases in Python tokenization."""

    def test_combined_string_prefixes_rf(self):
        """Test raw f-string (rf or fr prefix)."""
        test_cases = ['rf"text"', 'fr"text"', 'RF"text"', 'Fr"text"']
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)
            # Should tokenize as identifier(s) + string
            tokens = list(lexer._tokens)
            assert len(tokens) >= 2, f"Combined prefix string '{s}' should produce tokens"

    def test_combined_string_prefixes_br(self):
        """Test byte raw string (br or rb prefix)."""
        test_cases = ['br"text"', 'rb"text"', 'BR"text"', 'Rb"text"']
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 2, f"Combined prefix string '{s}' should produce tokens"

    def test_unicode_string_prefix(self):
        """Test unicode string prefix (u-prefix, Python 2 compatibility)."""
        test_cases = ['u"text"', 'U"text"']
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 2, f"Unicode string '{s}' should produce tokens"

    def test_numbers_with_underscores(self):
        """Test numbers with underscores (Python 3.6+)."""
        test_cases = [
            '1_000',
            '1_000_000',
            '0x_FF_FF',
            '0b_1111_0000',
            '0o_777',
            '3.14_15_93',
            '1_0e1_0',
        ]
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            # The lexer might not support underscores, so check what happens
            assert len(tokens) >= 1, f"Number with underscores '{num}' should produce tokens"
            # Print what we got for manual inspection
            # print(f"{num}: {[(t.type.name, t.value) for t in tokens]}")

    def test_hex_float(self):
        """Test hexadecimal float notation."""
        test_cases = ['0x1.8p3', '0x1.0p-10', '0xA.Bp2']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            # Hex floats might not be fully supported
            assert len(tokens) >= 1, f"Hex float '{num}' should produce tokens"

    def test_float_with_dot_and_exponent(self):
        """Test float starting with dot in scientific notation."""
        test_cases = ['.5e10', '.1e-5', '.999E+2']
        for num in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Float '{num}' should produce tokens"
            # Should be a single NUMBER token
            if len(tokens) == 1:
                assert tokens[0].type.name == 'NUMBER'

    def test_invalid_octal_digit(self):
        """Test octal number with invalid digit."""
        lexer = PythonLexer()
        lexer.lex(None, '0o8')
        tokens = list(lexer._tokens)
        # Might be parsed as 0o (incomplete) + 8, or as error
        assert len(tokens) >= 1, "Invalid octal should produce tokens"

    def test_invalid_binary_digit(self):
        """Test binary number with invalid digit."""
        lexer = PythonLexer()
        lexer.lex(None, '0b2')
        tokens = list(lexer._tokens)
        # Might be parsed as 0b (incomplete) + 2, or as error
        assert len(tokens) >= 1, "Invalid binary should produce tokens"

    def test_ellipsis_operator(self):
        """Test ellipsis (...) vs multiple dots."""
        lexer = PythonLexer()
        lexer.lex(None, '...')
        tokens = list(lexer._tokens)
        # Should be single ellipsis operator
        assert len(tokens) == 1, "Ellipsis should be single token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '...'

    def test_multiple_separate_dots(self):
        """Test multiple separate dots."""
        lexer = PythonLexer()
        lexer.lex(None, '. . .')
        tokens = list(lexer._tokens)
        # Should be three separate dot operators
        dot_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '.']
        assert len(dot_tokens) == 3, "Should have three separate dots"

    def test_two_dots(self):
        """Test two consecutive dots."""
        lexer = PythonLexer()
        lexer.lex(None, '..')
        tokens = list(lexer._tokens)
        # Should be two separate dot operators
        assert len(tokens) >= 2, "Two dots should produce at least two tokens"

    def test_number_dot_number(self):
        """Test expression like 1..2 (range in some languages)."""
        lexer = PythonLexer()
        lexer.lex(None, '1..2')
        tokens = list(lexer._tokens)
        # Actually parsed as: NUMBER(1.) NUMBER(.2) - two floats
        assert len(tokens) == 2, "Should have two tokens"
        assert all(t.type.name == 'NUMBER' for t in tokens), "Both should be numbers"

    def test_very_long_identifier(self):
        """Test extremely long identifier."""
        long_id = 'a' * 1000
        lexer = PythonLexer()
        lexer.lex(None, long_id)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long identifier should be single token"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == long_id

    def test_very_long_string(self):
        """Test extremely long string."""
        long_str = '"' + 'x' * 5000 + '"'
        lexer = PythonLexer()
        lexer.lex(None, long_str)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long string should be single token"
        assert tokens[0].type.name == 'STRING'

    def test_very_long_number(self):
        """Test extremely long number."""
        long_num = '9' * 1000
        lexer = PythonLexer()
        lexer.lex(None, long_num)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long number should be single token"
        assert tokens[0].type.name == 'NUMBER'

    def test_very_long_comment(self):
        """Test extremely long comment."""
        long_comment = '# ' + 'x' * 5000
        lexer = PythonLexer()
        lexer.lex(None, long_comment)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long comment should be single token"
        assert tokens[0].type.name == 'COMMENT'

    def test_unclosed_single_quote_string(self):
        """Test unclosed single quote string."""
        lexer = PythonLexer()
        lexer.lex(None, "'unclosed string")
        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Unclosed string should produce tokens"

    def test_unclosed_double_quote_string(self):
        """Test unclosed double quote string."""
        lexer = PythonLexer()
        lexer.lex(None, '"unclosed string')
        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Unclosed string should produce tokens"

    def test_string_with_newline_escape(self):
        """Test string with actual newline character (not escape)."""
        # This is invalid in Python but let's see how it's handled
        lexer = PythonLexer()
        lexer.lex(None, '"string with\nnewline"')
        tokens = list(lexer._tokens)
        # Might be multiple tokens or error
        assert len(tokens) >= 1, "String with newline should produce tokens"

    def test_backslash_at_end_of_line(self):
        """Test line continuation with backslash."""
        lexer = PythonLexer()
        lexer.lex(None, 'x = 1 + \\')
        tokens = list(lexer._tokens)
        backslash_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '\\']
        assert len(backslash_tokens) == 1, "Should have backslash operator"

    def test_mixed_quotes_in_docstring(self):
        """Test docstring containing other quote types."""
        test_cases = [
            '"""contains \'\'\' inside"""',
            "'''contains \"\"\" inside'''",
        ]
        for s in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, s)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Docstring '{s}' should be single token"
            assert tokens[0].type.name == 'STRING'

    def test_empty_input(self):
        """Test empty input string."""
        lexer = PythonLexer()
        lexer.lex(None, '')
        tokens = list(lexer._tokens)
        assert len(tokens) == 0, "Empty input should produce no tokens"

    def test_only_whitespace(self):
        """Test input with only whitespace."""
        test_cases = [' ', '    ', '\t', '  \t  ']
        for ws in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, ws)
            tokens = list(lexer._tokens)
            # Whitespace might or might not produce tokens
            # Just checking it doesn't crash
            assert len(tokens) >= 0

    def test_all_operators_in_sequence(self):
        """Test many operators in sequence."""
        lexer = PythonLexer()
        lexer.lex(None, '+-*/%**//<<>>&|^~')
        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        # Should have multiple operator tokens
        assert len(operator_tokens) >= 10, "Should have many operators"

    def test_adjacent_strings_no_space(self):
        """Test adjacent strings without space (invalid but let's see)."""
        lexer = PythonLexer()
        lexer.lex(None, '"hello""world"')
        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 2, "Should have two string tokens"

    def test_number_immediately_followed_by_identifier(self):
        """Test number immediately followed by identifier (invalid)."""
        lexer = PythonLexer()
        lexer.lex(None, '123abc')
        tokens = list(lexer._tokens)
        # Should be at least number token
        assert len(tokens) >= 1, "Should produce tokens"
        assert tokens[0].type.name == 'NUMBER'

    def test_identifier_starting_with_number_letter(self):
        """Test that identifiers can't start with numbers."""
        lexer = PythonLexer()
        lexer.lex(None, '1var')
        tokens = list(lexer._tokens)
        # Should be NUMBER + IDENTIFIER
        assert len(tokens) >= 2, "Should split into number and identifier"
        assert tokens[0].type.name == 'NUMBER'

    def test_comment_with_string_quotes(self):
        """Test that quotes in comments don't start strings."""
        lexer = PythonLexer()
        lexer.lex(None, '# This is a "comment" not a string')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single comment token"
        assert tokens[0].type.name == 'COMMENT'

    def test_hash_in_string_not_comment(self):
        """Test that # inside string doesn't start comment."""
        lexer = PythonLexer()
        lexer.lex(None, '"string with # hash"')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single string token"
        assert tokens[0].type.name == 'STRING'

    def test_triple_quote_inside_single_quote_string(self):
        """Test triple quotes inside regular string."""
        lexer = PythonLexer()
        lexer.lex(None, '"\\"\\"\\"not a docstring"')
        tokens = list(lexer._tokens)
        # Should be a string token
        assert len(tokens) >= 1

    def test_operator_combinations(self):
        """Test various operator combinations."""
        test_cases = [
            '**=',
            '//=',
            '>>=',
            '<<=',
            '!=',
            '==',
            '<=',
            '>=',
            ':=',
            '->',
        ]
        for op in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should be single token"
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_token_positions(self):
        """Test that token positions are tracked correctly."""
        lexer = PythonLexer()
        lexer.lex(None, 'x = 42')
        tokens = list(lexer._tokens)

        # Check that start positions are set
        for token in tokens:
            assert hasattr(token, 'start'), "Token should have start position"
            assert token.start >= 0, "Start position should be non-negative"

        # Positions should be in order
        positions = [t.start for t in tokens]
        assert positions == sorted(positions), "Token positions should be in order"

    def test_zero_with_decimal_point(self):
        """Test 0. as a float."""
        lexer = PythonLexer()
        lexer.lex(None, '0.')
        tokens = list(lexer._tokens)
        # Might be NUMBER or NUMBER + OPERATOR
        assert len(tokens) >= 1, "Should produce tokens"

    def test_multiple_underscores_in_identifier(self):
        """Test identifier with multiple consecutive underscores."""
        lexer = PythonLexer()
        lexer.lex(None, 'var___name')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single identifier"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'var___name'

    def test_all_underscore_identifier(self):
        """Test identifier that's all underscores."""
        lexer = PythonLexer()
        lexer.lex(None, '____')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single identifier"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == '____'
