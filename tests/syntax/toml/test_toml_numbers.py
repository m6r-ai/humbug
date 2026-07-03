"""
Tests for TOML number, boolean, and datetime tokenization.
"""
from syntax.toml.toml_lexer import TOMLLexer


class TestTOMLNumbers:
    """Test TOML number tokenization."""

    def test_integers(self):
        """Test integer literals."""
        for num in ['0', '42', '100', '1_000_000']:
            lexer = TOMLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{num}' should be NUMBER"

    def test_negative_integers(self):
        """Test negative integer literals."""
        for num in ['-1', '-42']:
            lexer = TOMLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'NUMBER'

    def test_positive_integers(self):
        """Test positive integer literals with explicit +."""
        lexer = TOMLLexer()
        lexer.lex(None, '+42')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_floats(self):
        """Test floating-point literals."""
        for num in ['3.14', '0.5', '42.0', '1.5e10', '1.5e-3', '1.5E+10', '1e6']:
            lexer = TOMLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{num}' should be NUMBER"

    def test_float_starting_with_dot(self):
        """Test float literals starting with a dot."""
        lexer = TOMLLexer()
        lexer.lex(None, '.5')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_underscores_in_numbers(self):
        """Test underscores as digit separators."""
        lexer = TOMLLexer()
        lexer.lex(None, '1_000')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_hex_literals(self):
        """Test hexadecimal literals."""
        lexer = TOMLLexer()
        lexer.lex(None, '0xDEAD_BEEF')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '0xDEAD_BEEF'

    def test_octal_literals(self):
        """Test octal literals."""
        lexer = TOMLLexer()
        lexer.lex(None, '0o755')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '0o755'

    def test_binary_literals(self):
        """Test binary literals."""
        lexer = TOMLLexer()
        lexer.lex(None, '0b1010_1010')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_infinity(self):
        """Test infinity literals."""
        lexer = TOMLLexer()
        lexer.lex(None, 'inf')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_nan(self):
        """Test NaN literals."""
        lexer = TOMLLexer()
        lexer.lex(None, 'nan')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_signed_infinity_and_nan(self):
        """Test signed infinity and NaN."""
        for val in ['+inf', '-inf', '+nan', '-nan']:
            lexer = TOMLLexer()
            lexer.lex(None, val)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'NUMBER'


class TestTOMLBooleans:
    """Test TOML boolean tokenization."""

    def test_true(self):
        """Test 'true' is recognized as a boolean."""
        lexer = TOMLLexer()
        lexer.lex(None, 'true')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'BOOLEAN'
        assert tokens[0].value == 'true'

    def test_false(self):
        """Test 'false' is recognized as a boolean."""
        lexer = TOMLLexer()
        lexer.lex(None, 'false')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'BOOLEAN'
        assert tokens[0].value == 'false'

    def test_booleans_must_be_lowercase(self):
        """Test that True/False are NOT booleans (TOML requires lowercase)."""
        for val in ['True', 'False', 'TRUE', 'FALSE']:
            lexer = TOMLLexer()
            lexer.lex(None, val)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'TEXT', f"'{val}' should be TEXT, not BOOLEAN"


class TestTOMLDatetimes:
    """Test TOML datetime tokenization."""

    def test_date(self):
        """Test date literals (YYYY-MM-DD)."""
        lexer = TOMLLexer()
        lexer.lex(None, '2025-07-17')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_time(self):
        """Test time literals (HH:MM:SS)."""
        lexer = TOMLLexer()
        lexer.lex(None, '10:30:00')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_datetime_with_t(self):
        """Test datetime with T separator."""
        lexer = TOMLLexer()
        lexer.lex(None, '2025-07-17T10:30:00Z')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_datetime_with_offset(self):
        """Test datetime with timezone offset."""
        lexer = TOMLLexer()
        lexer.lex(None, '2025-07-17T10:30:00-05:00')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'
