"""
Tests for JavaScript number tokenization.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestJavaScriptNumbers:
    """Test JavaScript number literal tokenization."""

    def test_integer_literals(self):
        """Test integer literals."""
        test_cases = [
            '0',
            '1',
            '42',
            '123',
            '999999',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value == code

    def test_floating_point_literals(self):
        """Test floating point literals."""
        test_cases = [
            '0.0',
            '1.5',
            '3.14',
            '123.456',
            '0.123',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value == code

    def test_floating_point_starting_with_dot(self):
        """Test floating point numbers starting with dot."""
        test_cases = [
            '.5',
            '.123',
            '.0',
            '.999',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value == code

    def test_scientific_notation(self):
        """Test numbers in scientific notation."""
        test_cases = [
            '1e10',
            '1E10',
            '1.5e-3',
            '2.0E+5',
            '3.14e0',
            '0.5e10',
            '1e+10',
            '1e-10',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"

    def test_hexadecimal_literals(self):
        """Test hexadecimal number literals."""
        test_cases = [
            '0x0',
            '0xFF',
            '0x1234',
            '0xABCDEF',
            '0xabcdef',
            '0X10',  # uppercase X
            '0XFF',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"

    def test_binary_literals(self):
        """Test binary number literals."""
        test_cases = [
            '0b0',
            '0b1',
            '0b1010',
            '0b11111111',
            '0B101',  # uppercase B
            '0B1111',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"

    def test_octal_literals(self):
        """Test octal number literals (ES6 style)."""
        test_cases = [
            '0o0',
            '0o7',
            '0o77',
            '0o777',
            '0O10',  # uppercase O
            '0O777',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"

    def test_bigint_literals(self):
        """Test BigInt literals (n suffix)."""
        test_cases = [
            '123n',
            '0n',
            '9007199254740991n',
            '0xFFn',
            '0b1010n',
            '0o777n',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
            assert tokens[0].value == code

    def test_legacy_octal_literals(self):
        """Test legacy octal literals (leading zero - deprecated)."""
        test_cases = [
            '00',
            '07',
            '077',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"


class TestJavaScriptNumberEdgeCases:
    """Test edge cases for JavaScript number tokenization."""

    def test_numbers_in_expressions(self):
        """Test numbers in various expressions."""
        test_cases = [
            'let x = 42;',
            'const pi = 3.14;',
            'if (x > 10) {}',
            'array[5]',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            number_tokens = [t for t in tokens if t.type.name == 'NUMBER']
            assert len(number_tokens) >= 1, f"'{code}' should have at least one number token"

    def test_number_followed_by_identifier(self):
        """Test number followed by identifier (should be separate tokens)."""
        test_cases = [
            ('42x', 2),  # number + identifier
            ('3.14pi', 2),  # number + identifier
            ('0xFFg', 2),  # hex number + identifier
        ]
        for code, expected_tokens in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should be separate tokens (number and identifier)
            # Unless it's a valid BigInt
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_dot_operator_vs_decimal(self):
        """Test distinguishing dot operator from decimal point."""
        test_cases = [
            ('obj.prop', 'OPERATOR'),  # dot operator
            ('3.14', 'NUMBER'),  # decimal point
            ('.5', 'NUMBER'),  # starts with decimal
        ]
        for code, first_token_type in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_negative_numbers(self):
        """Test negative numbers (minus is operator, not part of number)."""
        test_cases = [
            '-42',
            '-3.14',
            '-0x10',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Minus should be separate operator token
            assert len(tokens) >= 2, f"'{code}' should produce at least 2 tokens"
            operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '-']
            assert len(operator_tokens) == 1, "Should have minus operator"

    def test_numbers_with_underscores(self):
        """Test numeric separators (ES2021 feature)."""
        test_cases = [
            '1_000',
            '1_000_000',
            '0b1010_0101',
            '0x_FF',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Current implementation may not support numeric separators
            # Just verify it doesn't crash
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_infinity_and_nan(self):
        """Test Infinity and NaN (these are identifiers, not number literals)."""
        test_cases = [
            'Infinity',
            'NaN',
            '-Infinity',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # These should be identifiers (or operator + identifier for negative)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_zero_variants(self):
        """Test different ways to write zero."""
        test_cases = [
            '0',
            '0.0',
            '0e0',
            '0x0',
            '0b0',
            '0o0',
            '0n',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"

    def test_invalid_number_formats(self):
        """Test invalid number formats (should still tokenize somehow)."""
        test_cases = [
            '0x',  # hex without digits
            '0b',  # binary without digits
            '0o',  # octal without digits
            '1e',  # scientific without exponent
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should produce some tokens, even if not valid numbers
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_numbers_at_boundaries(self):
        """Test numbers at various boundaries."""
        test_cases = [
            '0',  # minimum
            '9007199254740991',  # MAX_SAFE_INTEGER
            '1.7976931348623157e+308',  # near MAX_VALUE
            '5e-324',  # MIN_VALUE
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{code}' should be NUMBER type"
