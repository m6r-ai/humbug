"""
Tests for Bash lexer strings.
"""
from syntax.bash.bash_lexer import BashLexer


class TestBashStrings:
    """Test Bash string tokenization."""

    def test_single_quoted_string(self):
        """Test single-quoted strings (no escape processing)."""
        lexer = BashLexer()
        lexer.lex(None, "'hello world'")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == "'hello world'"

    def test_double_quoted_string(self):
        """Test double-quoted strings."""
        lexer = BashLexer()
        lexer.lex(None, '"hello world"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == '"hello world"'

    def test_string_with_variable(self):
        """Test that variables inside strings are part of the string token."""
        lexer = BashLexer()
        lexer.lex(None, '"hello $NAME"')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 1
        assert string_tokens[0].value == '"hello $NAME"'

    def test_single_quoted_with_backslash(self):
        """Test that backslashes are literal in single-quoted strings."""
        lexer = BashLexer()
        lexer.lex(None, r"'hello\nworld'")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == r"'hello\nworld'"

    def test_double_quoted_with_escape(self):
        """Test escape processing in double-quoted strings."""
        lexer = BashLexer()
        lexer.lex(None, r'"hello \"world\""')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_dollar_quoted_string(self):
        """Test dollar-quoted strings ($'...' and $"...")."""
        lexer = BashLexer()
        lexer.lex(None, "$'hello world'")

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 1
        assert string_tokens[0].value == "$'hello world'"

    def test_backtick_string(self):
        """Test backtick command substitution."""
        lexer = BashLexer()
        lexer.lex(None, '`echo hello`')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 1
        assert string_tokens[0].value == '`echo hello`'

    def test_unterminated_string(self):
        """Test that unterminated strings consume to end of line."""
        lexer = BashLexer()
        lexer.lex(None, '"unterminated')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
