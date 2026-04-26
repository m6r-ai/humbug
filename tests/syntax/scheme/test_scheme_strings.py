"""Tests for Scheme string literal tokenization."""

from syntax.scheme.scheme_lexer import SchemeLexer, SchemeLexerState
from syntax.lexer import TokenType


class TestSchemeStrings:
    """Tests for Scheme string literal tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def _lex_with_state(self, source: str, prev_state=None):
        lexer = SchemeLexer()
        state = lexer.lex(prev_state, source)
        return list(lexer._tokens), state

    def test_simple_string(self):
        """A simple quoted string is tokenized as STRING."""
        tokens = self._lex('"hello"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"hello"'

    def test_empty_string(self):
        """An empty string '\"\"' is tokenized as STRING."""
        tokens = self._lex('""')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '""'

    def test_string_with_spaces(self):
        """A string containing spaces is a single STRING token."""
        tokens = self._lex('"hello world"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"hello world"'

    def test_string_escape_quote(self):
        """A string with an escaped quote \\\" is a single STRING token."""
        tokens = self._lex(r'"say \"hi\""')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_string_escape_backslash(self):
        """A string with an escaped backslash \\\\ is a single STRING token."""
        tokens = self._lex(r'"a\\b"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_string_escape_newline(self):
        """A string with \\n escape is a single STRING token."""
        tokens = self._lex(r'"line1\nline2"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_string_escape_tab(self):
        """A string with \\t escape is a single STRING token."""
        tokens = self._lex(r'"col1\tcol2"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_string_escape_carriage_return(self):
        """A string with \\r escape is a single STRING token."""
        tokens = self._lex(r'"foo\rbar"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_string_start_position(self):
        """The start position of a string is correctly recorded."""
        tokens = self._lex('  "hi"')
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_string_in_list(self):
        """A string inside a list expression is correctly tokenized."""
        tokens = self._lex('("hello")')
        str_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(str_tokens) == 1
        assert str_tokens[0].value == '"hello"'

    def test_two_strings_whitespace_separated(self):
        """Two strings separated by whitespace are both tokenized."""
        tokens = self._lex('"foo" "bar"')
        str_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(str_tokens) == 2
        assert str_tokens[0].value == '"foo"'
        assert str_tokens[1].value == '"bar"'

    def test_unclosed_string_produces_string_token(self):
        """An unclosed string produces a STRING token containing everything to end of input."""
        tokens = self._lex('"unclosed')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"unclosed'

    def test_unclosed_string_sets_in_string_state(self):
        """An unclosed string sets in_string=True in the returned lexer state."""
        _, state = self._lex_with_state('"unclosed')
        assert state.in_string is True

    def test_closed_string_clears_in_string_state(self):
        """A closed string leaves in_string=False in the returned lexer state."""
        _, state = self._lex_with_state('"closed"')
        assert state.in_string is False

    def test_multiline_string_continuation(self):
        """A string continued from a previous line via in_string state is tokenized as STRING."""
        _, state = self._lex_with_state('"line one')
        assert state.in_string is True
        tokens2, state2 = self._lex_with_state('line two"', prev_state=state)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.STRING
        assert tokens2[0].value == 'line two"'
        assert state2.in_string is False

    def test_multiline_string_still_open_on_middle_line(self):
        """A string that spans two lines and is still open keeps in_string=True."""
        _, state = self._lex_with_state('"line one')
        tokens2, state2 = self._lex_with_state('still open', prev_state=state)
        assert tokens2[0].type == TokenType.STRING
        assert state2.in_string is True

    def test_multiline_string_three_lines(self):
        """A string spanning three lines is correctly continued across all lines."""
        _, state1 = self._lex_with_state('"line one')
        _, state2 = self._lex_with_state('line two', prev_state=state1)
        assert state2.in_string is True
        tokens3, state3 = self._lex_with_state('line three"', prev_state=state2)
        assert tokens3[0].type == TokenType.STRING
        assert state3.in_string is False

    def test_string_with_parens_inside(self):
        """Parentheses inside a string are not treated as delimiters."""
        tokens = self._lex('"(not a list)"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"(not a list)"'

    def test_string_with_semicolon_inside(self):
        """A semicolon inside a string does not start a comment."""
        tokens = self._lex('"not ; a comment"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"not ; a comment"'

    def test_string_with_hash_inside(self):
        """A hash inside a string does not trigger hash-token processing."""
        tokens = self._lex('"#t is true"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"#t is true"'

    def test_string_followed_by_identifier(self):
        """A string followed by an identifier produces STRING then IDENTIFIER."""
        tokens = self._lex('"hello" world')
        assert tokens[0].type == TokenType.STRING
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 1
        assert ident_tokens[0].value == "world"

    def test_string_followed_by_comment(self):
        """A string followed by a comment produces STRING then COMMENT."""
        tokens = self._lex('"hello" ; world')
        assert tokens[0].type == TokenType.STRING
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

    def test_multiple_strings_in_expression(self):
        """Multiple strings in a list expression are all tokenized."""
        tokens = self._lex('(string-append "hello" " " "world")')
        str_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(str_tokens) == 3
        assert str_tokens[0].value == '"hello"'
        assert str_tokens[1].value == '" "'
        assert str_tokens[2].value == '"world"'

    def test_very_long_string(self):
        """A very long string is tokenized as a single STRING token."""
        long_str = '"' + "x" * 10000 + '"'
        tokens = self._lex(long_str)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
