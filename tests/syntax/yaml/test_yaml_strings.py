"""
Tests for YAML string tokenization.
"""
from syntax.yaml.yaml_lexer import YAMLLexer


class TestYAMLStrings:
    """Test YAML string tokenization."""

    def test_double_quoted_strings(self):
        """Test double-quoted strings."""
        for s in ['"hello"', '"with spaces"', '"123"', '""']:
            lexer = YAMLLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == s

    def test_single_quoted_strings(self):
        """Test single-quoted strings."""
        for s in ["'hello'", "'with spaces'", "'123'", "''"]:
            lexer = YAMLLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == s

    def test_double_quoted_with_escapes(self):
        """Test double-quoted strings with escape sequences."""
        for s in [r'"line\nbreak"', r'"tab\there"', r'"quote\"inside"', r'"backslash\\here"']:
            lexer = YAMLLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Escaped string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_single_quoted_with_doubled_quote(self):
        """Test single-quoted strings with doubled single quotes (YAML escape)."""
        lexer = YAMLLexer()
        lexer.lex(None, "'it''s ok'")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == "'it''s ok'"

    def test_string_containing_other_quote(self):
        """Test strings containing the other quote type."""
        lexer = YAMLLexer()
        lexer.lex(None, "'she said \"hi\"'")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_string_with_colon(self):
        """Test strings containing colons (not parsed as key separators)."""
        lexer = YAMLLexer()
        lexer.lex(None, '"http://example.com"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == '"http://example.com"'

    def test_unclosed_double_quoted(self):
        """Test unclosed double-quoted string still produces a token."""
        lexer = YAMLLexer()
        lexer.lex(None, '"unclosed')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) >= 1
