"""Tests for Scheme identifier tokenization."""

from syntax.scheme.scheme_lexer import SchemeLexer
from syntax.lexer import TokenType


class TestSchemeIdentifiers:
    """Tests for Scheme identifier tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def test_simple_identifier(self):
        """A plain alphabetic identifier is tokenized as IDENTIFIER."""
        tokens = self._lex("foo")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo"

    def test_identifier_with_digits(self):
        """An identifier may contain digits after the first character."""
        tokens = self._lex("foo123")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo123"

    def test_identifier_with_hyphen(self):
        """An identifier may contain hyphens (kebab-case)."""
        tokens = self._lex("foo-bar")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo-bar"

    def test_identifier_predicate_style(self):
        """An identifier ending with ? is valid."""
        tokens = self._lex("null?")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "null?"

    def test_identifier_bang_style(self):
        """An identifier ending with ! is valid."""
        tokens = self._lex("set-value!")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "set-value!"

    def test_identifier_with_star(self):
        """An identifier may contain an asterisk."""
        tokens = self._lex("make-array*")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "make-array*"

    def test_identifier_with_slash(self):
        """An identifier may contain a forward slash."""
        tokens = self._lex("a/b")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "a/b"

    def test_identifier_with_angle_brackets(self):
        """An identifier may contain < and > characters."""
        tokens = self._lex("<foo>")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "<foo>"

    def test_identifier_with_equals(self):
        """An identifier may contain an equals sign."""
        tokens = self._lex("foo=bar")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo=bar"

    def test_identifier_with_underscore(self):
        """An identifier may contain an underscore."""
        tokens = self._lex("foo_bar")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo_bar"

    def test_identifier_with_dot_in_middle(self):
        """A dot in the middle of an identifier is consumed as part of it."""
        tokens = self._lex("foo.bar")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo.bar"

    def test_special_identifier_plus(self):
        """The single character + is a valid R5RS identifier."""
        tokens = self._lex("+")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "+"

    def test_special_identifier_minus(self):
        """The single character - is a valid R5RS identifier."""
        tokens = self._lex("-")
        assert len(tokens) == 1, f"Expected 1 token, got {len(tokens)}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "-"

    def test_special_identifier_ellipsis(self):
        """R5RS: '...' is a valid identifier — the lexer incorrectly splits it into three DOTs."""
        tokens = self._lex("...")
        assert len(tokens) == 1, f"Expected 1 IDENTIFIER token for '...', got {len(tokens)}: {[(t.type, t.value) for t in tokens]}"
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "..."

    def test_plus_before_space_is_identifier(self):
        """A bare + followed by a space and identifier produces two IDENTIFIER tokens."""
        tokens = self._lex("+ foo")
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 2, f"Expected 2 IDENTIFIER tokens, got {ident_tokens}"
        assert ident_tokens[0].value == "+"
        assert ident_tokens[1].value == "foo"

    def test_minus_before_space_is_identifier(self):
        """A bare - followed by a space and identifier produces two IDENTIFIER tokens."""
        tokens = self._lex("- foo")
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 2, f"Expected 2 IDENTIFIER tokens, got {ident_tokens}"
        assert ident_tokens[0].value == "-"
        assert ident_tokens[1].value == "foo"

    def test_identifier_stops_at_open_paren(self):
        """An identifier stops when a ( delimiter is encountered."""
        tokens = self._lex("foo(")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo"
        assert tokens[1].type == TokenType.LPAREN

    def test_identifier_stops_at_close_paren(self):
        """An identifier stops when a ) delimiter is encountered."""
        tokens = self._lex("foo)")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo"
        assert tokens[1].type == TokenType.RPAREN

    def test_identifier_stops_at_hash(self):
        """An identifier stops when a # delimiter is encountered."""
        tokens = self._lex("foo#t")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo"
        assert tokens[1].type == TokenType.BOOLEAN

    def test_identifier_stops_at_whitespace(self):
        """An identifier stops at whitespace."""
        tokens = self._lex("foo bar")
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 2
        assert ident_tokens[0].value == "foo"
        assert ident_tokens[1].value == "bar"

    def test_apostrophe_followed_by_identifier(self):
        """' followed by an identifier is read as a single IDENTIFIER token."""
        tokens = self._lex("'foo")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "'foo"

    def test_apostrophe_alone_is_identifier(self):
        """A lone ' is read as an IDENTIFIER."""
        tokens = self._lex("'")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "'"

    def test_apostrophe_before_paren(self):
        """' before ( produces IDENTIFIER \"'\" then LPAREN."""
        tokens = self._lex("'(")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "'"
        assert tokens[1].type == TokenType.LPAREN

    def test_identifier_case_preserved(self):
        """The lexer preserves the original case of identifiers."""
        tokens = self._lex("FooBar")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "FooBar"

    def test_multiple_identifiers_in_expression(self):
        """Multiple identifiers in a list expression are all tokenized correctly."""
        tokens = self._lex("(foo bar baz)")
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 3
        assert ident_tokens[0].value == "foo"
        assert ident_tokens[1].value == "bar"
        assert ident_tokens[2].value == "baz"

    def test_identifier_start_position(self):
        """The start position of an identifier is correctly recorded."""
        tokens = self._lex("   foo")
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert ident_tokens[0].start == 3, f"Expected start=3, got {ident_tokens[0].start}"

    def test_minus_hash_splits_tokens(self):
        """-#xFF splits into IDENTIFIER '-' and NUMBER '#xFF' because # is a delimiter."""
        tokens = self._lex("-#xFF")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "-"
        assert tokens[1].type == TokenType.NUMBER
        assert tokens[1].value == "#xFF"

    def test_common_scheme_identifiers(self):
        """Common Scheme standard library identifiers are tokenized as IDENTIFIER."""
        for ident in ['car', 'cdr', 'cons', 'list', 'map', 'apply', 'null?', 'pair?',
                      'string->number', 'number->string', 'list->vector', 'vector->list']:
            tokens = self._lex(ident)
            assert len(tokens) == 1, f"Expected 1 token for '{ident}', got {len(tokens)}"
            assert tokens[0].type == TokenType.IDENTIFIER, f"Expected IDENTIFIER for '{ident}'"

    def test_very_long_identifier(self):
        """A very long identifier is tokenized as a single IDENTIFIER token."""
        long_id = "a" * 10000
        tokens = self._lex(long_id)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == long_id
