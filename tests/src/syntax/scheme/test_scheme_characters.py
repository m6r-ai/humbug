"""Tests for Scheme character literal tokenization."""

from syntax.scheme.scheme_lexer import SchemeLexer
from syntax.lexer import TokenType


class TestSchemeCharacters:
    """Tests for Scheme character literal tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def test_character_letter_a(self):
        """'#\\a' is tokenized as CHARACTER."""
        tokens = self._lex("#\\a")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\a"

    def test_character_letter_z(self):
        """'#\\z' is tokenized as CHARACTER."""
        tokens = self._lex("#\\z")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\z"

    def test_character_uppercase_letter(self):
        """'#\\A' is tokenized as CHARACTER."""
        tokens = self._lex("#\\A")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\A"

    def test_character_digit(self):
        """'#\\0' is tokenized as CHARACTER."""
        tokens = self._lex("#\\0")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\0"

    def test_character_named_space(self):
        """'#\\space' is tokenized as CHARACTER."""
        tokens = self._lex("#\\space")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\space"

    def test_character_named_newline(self):
        """'#\\newline' is tokenized as CHARACTER."""
        tokens = self._lex("#\\newline")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\newline"

    def test_character_named_tab(self):
        """'#\\tab' is tokenized as CHARACTER."""
        tokens = self._lex("#\\tab")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\tab"

    def test_character_named_nul(self):
        """'#\\nul' is tokenized as CHARACTER."""
        tokens = self._lex("#\\nul")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\nul"

    def test_character_named_return(self):
        """'#\\return' is tokenized as CHARACTER."""
        tokens = self._lex("#\\return")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\return"

    def test_character_exclamation(self):
        """'#\\!' is tokenized as CHARACTER."""
        tokens = self._lex("#\\!")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\!"

    def test_character_stops_at_whitespace(self):
        """A character literal stops reading at whitespace."""
        tokens = self._lex("#\\space foo")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\space"
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert ident_tokens[0].value == "foo"

    def test_character_stops_at_open_paren(self):
        """A character literal stops reading at ( because ( is a delimiter."""
        tokens = self._lex("#\\a(")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\a"
        assert tokens[1].type == TokenType.LPAREN

    def test_character_stops_at_close_paren(self):
        """A character literal stops reading at ) because ) is a delimiter."""
        tokens = self._lex("#\\space)")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\space"
        assert tokens[1].type == TokenType.RPAREN

    def test_character_stops_at_hash(self):
        """A character literal stops reading at # because # is a delimiter."""
        tokens = self._lex("#\\a#t")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\a"
        assert tokens[1].type == TokenType.BOOLEAN

    def test_character_space_literal(self):
        """'#\\ ' (hash-backslash-space) stops at the space and produces CHARACTER '#\\'."""
        tokens = self._lex("#\\ ")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\"

    def test_character_open_paren_literal(self):
        """'#\\(' stops at ( and produces CHARACTER '#\\' then LPAREN."""
        tokens = self._lex("#\\(")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\"
        assert tokens[1].type == TokenType.LPAREN

    def test_character_close_paren_literal(self):
        """'#\\)' stops at ) and produces CHARACTER '#\\' then RPAREN."""
        tokens = self._lex("#\\)")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].value == "#\\"
        assert tokens[1].type == TokenType.RPAREN

    def test_character_start_position(self):
        """The start position of a character literal is correctly recorded."""
        tokens = self._lex("  #\\a")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_character_in_list(self):
        """A character literal inside a list is correctly tokenized."""
        tokens = self._lex("(#\\a)")
        char_tokens = [t for t in tokens if t.type == TokenType.CHARACTER]
        assert len(char_tokens) == 1
        assert char_tokens[0].value == "#\\a"

    def test_two_characters_whitespace_separated(self):
        """Two character literals separated by whitespace are both tokenized."""
        tokens = self._lex("#\\a #\\b")
        char_tokens = [t for t in tokens if t.type == TokenType.CHARACTER]
        assert len(char_tokens) == 2
        assert char_tokens[0].value == "#\\a"
        assert char_tokens[1].value == "#\\b"

    def test_character_not_confused_with_boolean_t(self):
        """'#\\t' is a CHARACTER, not the boolean #t."""
        tokens = self._lex("#\\t")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].type != TokenType.BOOLEAN

    def test_character_not_confused_with_boolean_f(self):
        """'#\\f' is a CHARACTER, not the boolean #f."""
        tokens = self._lex("#\\f")
        assert tokens[0].type == TokenType.CHARACTER
        assert tokens[0].type != TokenType.BOOLEAN

    def test_character_followed_by_number(self):
        """A character literal followed by a number produces CHARACTER then NUMBER."""
        tokens = self._lex("#\\a 42")
        assert tokens[0].type == TokenType.CHARACTER
        num_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(num_tokens) == 1
        assert num_tokens[0].value == "42"
