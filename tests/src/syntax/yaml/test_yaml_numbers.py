"""
Tests for YAML number and scalar value tokenization.
"""
from syntax.yaml.yaml_lexer import YAMLLexer


class TestYAMLNumbers:
    """Test YAML number tokenization."""

    def test_integers(self):
        """Test integer literals."""
        for num in ['0', '42', '100']:
            lexer = YAMLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{num}' should be NUMBER"
            assert tokens[0].value == num

    def test_negative_integers(self):
        """Test negative integer literals."""
        for num in ['-1', '-42', '-100']:
            lexer = YAMLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{num}' should be NUMBER"

    def test_floats(self):
        """Test floating-point literals."""
        for num in ['3.14', '0.5', '42.0', '1.5e10', '1.5e-3', '1.5E+10']:
            lexer = YAMLLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"'{num}' should be NUMBER"

    def test_hex_literals(self):
        """Test hexadecimal literals."""
        lexer = YAMLLexer()
        lexer.lex(None, '0xff')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '0xff'

    def test_octal_literals(self):
        """Test octal literals."""
        lexer = YAMLLexer()
        lexer.lex(None, '0o755')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '0o755'

    def test_binary_literals(self):
        """Test binary literals."""
        lexer = YAMLLexer()
        lexer.lex(None, '0b1010')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '0b1010'

    def test_float_starting_with_dot(self):
        """Test float literals starting with a dot."""
        lexer = YAMLLexer()
        lexer.lex(None, '.5')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '.5'

    def test_negative_float_starting_with_dot(self):
        """Test negative float starting with a dot."""
        lexer = YAMLLexer()
        lexer.lex(None, '-.5')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_infinity(self):
        """Test infinity literals."""
        lexer = YAMLLexer()
        lexer.lex(None, '.inf')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'

    def test_nan(self):
        """Test NaN literals."""
        lexer = YAMLLexer()
        lexer.lex(None, '.nan')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'NUMBER'


class TestYAMLKeywords:
    """Test YAML keyword (boolean/null) tokenization."""

    def test_true(self):
        """Test 'true' is recognized as a keyword."""
        lexer = YAMLLexer()
        lexer.lex(None, 'true')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'
        assert tokens[0].value == 'true'

    def test_false(self):
        """Test 'false' is recognized as a keyword."""
        lexer = YAMLLexer()
        lexer.lex(None, 'false')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'

    def test_null(self):
        """Test 'null' is recognized as a keyword."""
        lexer = YAMLLexer()
        lexer.lex(None, 'null')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'

    def test_yes_no_on_off(self):
        """Test YAML 1.1 boolean-like values."""
        for val in ['yes', 'no', 'on', 'off', '~']:
            lexer = YAMLLexer()
            lexer.lex(None, val)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'KEYWORD', f"'{val}' should be KEYWORD"

    def test_case_insensitive_keywords(self):
        """Test that True, FALSE, Null etc. are also keywords."""
        for val in ['True', 'False', 'Null', 'TRUE', 'FALSE', 'NULL']:
            lexer = YAMLLexer()
            lexer.lex(None, val)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'KEYWORD', f"'{val}' should be KEYWORD"
