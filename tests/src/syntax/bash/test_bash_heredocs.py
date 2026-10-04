"""
Tests for Bash lexer heredoc handling.
"""
from syntax.bash.bash_lexer import BashLexer, BashLexerState


class TestBashHeredocs:
    """Test Bash heredoc tokenization."""

    def test_simple_heredoc_delimiter(self):
        """Test that heredoc redirect and delimiter are tokenized."""
        lexer = BashLexer()
        lexer.lex(None, 'cat << EOF')

        tokens = list(lexer._tokens)
        assert lexer._in_heredoc
        assert lexer._heredoc_delimiter == 'EOF'
        assert not lexer._heredoc_quoted

    def test_heredoc_body_line(self):
        """Test consuming a heredoc body line."""
        state = BashLexerState(
            in_heredoc=True,
            heredoc_delimiter='EOF',
            heredoc_quoted=False
        )
        lexer = BashLexer()
        lexer.lex(state, 'some content here')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == 'some content here'
        assert lexer._in_heredoc

    def test_heredoc_terminator(self):
        """Test that the delimiter line ends the heredoc."""
        state = BashLexerState(
            in_heredoc=True,
            heredoc_delimiter='EOF',
            heredoc_quoted=False
        )
        lexer = BashLexer()
        lexer.lex(state, 'EOF')

        tokens = list(lexer._tokens)
        assert not lexer._in_heredoc
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_heredoc_terminator_with_trailing_whitespace(self):
        """Test that delimiter with trailing whitespace still terminates."""
        state = BashLexerState(
            in_heredoc=True,
            heredoc_delimiter='EOF',
            heredoc_quoted=False
        )
        lexer = BashLexer()
        lexer.lex(state, 'EOF   ')

        assert not lexer._in_heredoc

    def test_heredoc_multiline_sequence(self):
        """Test a complete multi-line heredoc sequence."""
        lines = [
            'cat << EOF',
            'line one',
            'line two',
            'EOF',
        ]
        state = None
        for line in lines:
            lexer = BashLexer()
            state = lexer.lex(state, line)

        assert not lexer._in_heredoc

    def test_quoted_heredoc_delimiter(self):
        """Test that a quoted delimiter is detected."""
        lexer = BashLexer()
        lexer.lex(None, "cat << 'EOF'")

        assert lexer._in_heredoc
        assert lexer._heredoc_delimiter == 'EOF'
        assert lexer._heredoc_quoted

    def test_dash_heredoc(self):
        """Test <<- heredoc (strip leading tabs)."""
        lexer = BashLexer()
        lexer.lex(None, 'cat <<- END')

        assert lexer._in_heredoc
        assert lexer._heredoc_delimiter == 'END'

    def test_heredoc_terminator_with_leading_whitespace(self):
        """Test that <<- delimiter can have leading whitespace."""
        state = BashLexerState(
            in_heredoc=True,
            heredoc_delimiter='END',
            heredoc_quoted=False
        )
        lexer = BashLexer()
        lexer.lex(state, '\t\tEND')

        assert not lexer._in_heredoc
