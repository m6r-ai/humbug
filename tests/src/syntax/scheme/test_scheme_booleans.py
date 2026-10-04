"""Tests for Scheme boolean tokenization."""

from syntax.scheme.scheme_lexer import SchemeLexer
from syntax.lexer import TokenType


class TestSchemeBooleans:
    """Tests for Scheme boolean tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def test_true_lowercase(self):
        """'#t' is tokenized as BOOLEAN."""
        tokens = self._lex("#t")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == "#t"

    def test_false_lowercase(self):
        """'#f' is tokenized as BOOLEAN."""
        tokens = self._lex("#f")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == "#f"

    def test_true_uppercase(self):
        """'#T' is tokenized as BOOLEAN."""
        tokens = self._lex("#T")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == "#T"

    def test_false_uppercase(self):
        """'#F' is tokenized as BOOLEAN."""
        tokens = self._lex("#F")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == "#F"

    def test_boolean_start_position(self):
        """The start position of a boolean is correctly recorded."""
        tokens = self._lex("   #t")
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].start == 3, f"Expected start=3, got {tokens[0].start}"

    def test_boolean_in_list(self):
        """Booleans inside a list expression are correctly tokenized."""
        tokens = self._lex("(#t #f)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 2
        assert bool_tokens[0].value == "#t"
        assert bool_tokens[1].value == "#f"

    def test_boolean_in_if_expression(self):
        """A boolean in an if expression is correctly tokenized."""
        tokens = self._lex("(if #t 1 0)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 1
        assert bool_tokens[0].value == "#t"

    def test_boolean_in_and_expression(self):
        """Booleans in an and expression are correctly tokenized."""
        tokens = self._lex("(and #t #f)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 2

    def test_boolean_followed_by_identifier(self):
        """A boolean followed by a space and identifier produces correct tokens."""
        tokens = self._lex("#t foo")
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == "#t"
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 1
        assert ident_tokens[0].value == "foo"

    def test_boolean_followed_by_rparen(self):
        """A boolean immediately followed by ) produces BOOLEAN then RPAREN."""
        tokens = self._lex("#t)")
        assert tokens[0].type == TokenType.BOOLEAN
        assert tokens[0].value == "#t"
        assert tokens[1].type == TokenType.RPAREN

    def test_boolean_preceded_by_lparen(self):
        """A boolean immediately after ( produces LPAREN then BOOLEAN."""
        tokens = self._lex("(#f")
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.BOOLEAN
        assert tokens[1].value == "#f"

    def test_two_booleans_whitespace_separated(self):
        """Two booleans separated by whitespace are both tokenized."""
        tokens = self._lex("#t #f")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 2
        assert bool_tokens[0].value == "#t"
        assert bool_tokens[1].value == "#f"

    def test_boolean_positions_are_ordered(self):
        """Boolean start positions are in ascending order."""
        tokens = self._lex("#t #f")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert bool_tokens[0].start == 0
        assert bool_tokens[1].start == 3

    def test_boolean_value_case_preserved(self):
        """The original case of the boolean token value is preserved."""
        tokens = self._lex("#T")
        assert tokens[0].value == "#T"

    def test_boolean_not_confused_with_character_t(self):
        """'#\\t' is a CHARACTER, not the boolean #t."""
        tokens = self._lex("#\\t")
        assert tokens[0].type == TokenType.CHARACTER

    def test_boolean_not_confused_with_character_f(self):
        """'#\\f' is a CHARACTER, not the boolean #f."""
        tokens = self._lex("#\\f")
        assert tokens[0].type == TokenType.CHARACTER

    def test_boolean_not_confused_with_hex_number(self):
        """'#xff' is a NUMBER, not a boolean."""
        tokens = self._lex("#xff")
        assert tokens[0].type == TokenType.NUMBER

    def test_adjacent_booleans_no_whitespace(self):
        """Two adjacent booleans with no whitespace between them are both tokenized."""
        tokens = self._lex("(#t#f)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 2
        assert bool_tokens[0].value == "#t"
        assert bool_tokens[1].value == "#f"

    def test_boolean_in_let_binding(self):
        """A boolean in a let binding is correctly tokenized."""
        tokens = self._lex("(let ((x #t)) x)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 1
        assert bool_tokens[0].value == "#t"

    def test_multiple_booleans_in_vector(self):
        """Booleans in a vector literal are correctly tokenized."""
        tokens = self._lex("#(#t #f #t)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 3
