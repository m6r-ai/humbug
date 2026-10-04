"""Tests for Scheme delimiter, dot, and vector tokenization."""

from syntax.scheme.scheme_lexer import SchemeLexer
from syntax.lexer import TokenType


class TestSchemeDelimiters:
    """Tests for Scheme delimiter, dot, and vector tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def test_open_paren(self):
        """'(' is tokenized as LPAREN."""
        tokens = self._lex("(")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[0].value == "("

    def test_close_paren(self):
        """')' is tokenized as RPAREN."""
        tokens = self._lex(")")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.RPAREN
        assert tokens[0].value == ")"

    def test_open_paren_start_position(self):
        """The start position of ( is correctly recorded."""
        tokens = self._lex("  (")
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_close_paren_start_position(self):
        """The start position of ) is correctly recorded."""
        tokens = self._lex("  )")
        assert tokens[0].type == TokenType.RPAREN
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_matched_parens(self):
        """A matched pair of parentheses produces LPAREN and RPAREN."""
        tokens = self._lex("()")
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN

    def test_nested_parens(self):
        """Nested parentheses produce the correct sequence of LPAREN/RPAREN tokens."""
        tokens = self._lex("((()))")
        paren_types = [t.type for t in tokens]
        assert paren_types == [
            TokenType.LPAREN, TokenType.LPAREN, TokenType.LPAREN,
            TokenType.RPAREN, TokenType.RPAREN, TokenType.RPAREN,
        ], f"Unexpected paren sequence: {paren_types}"

    def test_simple_list(self):
        """A simple list produces LPAREN, identifiers, and RPAREN."""
        tokens = self._lex("(a b c)")
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[-1].type == TokenType.RPAREN
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 3

    def test_many_open_parens(self):
        """Many open parens produce the correct number of LPAREN tokens."""
        tokens = self._lex("(((")
        assert len(tokens) == 3
        assert all(t.type == TokenType.LPAREN for t in tokens)

    def test_many_close_parens(self):
        """Many close parens produce the correct number of RPAREN tokens."""
        tokens = self._lex(")))")
        assert len(tokens) == 3
        assert all(t.type == TokenType.RPAREN for t in tokens)

    def test_dot_alone(self):
        """A lone '.' is tokenized as DOT."""
        tokens = self._lex(".")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.DOT
        assert tokens[0].value == "."

    def test_dot_in_pair_notation(self):
        """A dot in pair notation '(a . b)' produces a DOT token."""
        tokens = self._lex("(a . b)")
        dot_tokens = [t for t in tokens if t.type == TokenType.DOT]
        assert len(dot_tokens) == 1
        assert dot_tokens[0].value == "."

    def test_dot_start_position(self):
        """The start position of a dot is correctly recorded."""
        tokens = self._lex("  .")
        assert tokens[0].type == TokenType.DOT
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_dot_followed_by_digit_is_number(self):
        """'.5' is tokenized as NUMBER, not DOT."""
        tokens = self._lex(".5")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == ".5"

    def test_dot_followed_by_letter_is_dot(self):
        """'.' followed by a letter produces a DOT token."""
        tokens = self._lex(". foo")
        assert tokens[0].type == TokenType.DOT

    def test_dot_followed_by_paren_is_dot(self):
        """'.' followed by '(' produces DOT then LPAREN."""
        tokens = self._lex(".(")
        assert tokens[0].type == TokenType.DOT
        assert tokens[1].type == TokenType.LPAREN

    def test_ellipsis_produces_three_dot_tokens(self):
        """R5RS: '...' is a valid identifier — the lexer incorrectly produces three DOT tokens."""
        tokens = self._lex("...")
        assert len(tokens) == 1, (
            f"Expected 1 IDENTIFIER token for '...', got {len(tokens)}: "
            f"{[(t.type, t.value) for t in tokens]}"
        )
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "..."

    def test_vector_start(self):
        """'#(' is tokenized as VECTOR_START."""
        tokens = self._lex("#(")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.VECTOR_START
        assert tokens[0].value == "#("

    def test_vector_start_position(self):
        """The start position of #( is correctly recorded."""
        tokens = self._lex("  #(")
        assert tokens[0].type == TokenType.VECTOR_START
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_vector_with_numbers(self):
        """A vector literal '#(1 2 3)' produces VECTOR_START, numbers, and RPAREN."""
        tokens = self._lex("#(1 2 3)")
        assert tokens[0].type == TokenType.VECTOR_START
        assert tokens[-1].type == TokenType.RPAREN
        num_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(num_tokens) == 3

    def test_vector_with_booleans(self):
        """A vector of booleans '#(#t #f)' produces VECTOR_START, booleans, and RPAREN."""
        tokens = self._lex("#(#t #f)")
        assert tokens[0].type == TokenType.VECTOR_START
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 2

    def test_vector_with_strings(self):
        """A vector of strings produces VECTOR_START, strings, and RPAREN."""
        tokens = self._lex('#("a" "b")')
        assert tokens[0].type == TokenType.VECTOR_START
        str_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(str_tokens) == 2

    def test_hash_is_delimiter_for_identifiers(self):
        """'#' acts as a delimiter, stopping identifier reading."""
        tokens = self._lex("foo#t")
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "foo"
        assert tokens[1].type == TokenType.BOOLEAN

    def test_pair_notation_full_token_sequence(self):
        """Full pair notation '(a . b)' produces LPAREN, IDENTIFIER, DOT, IDENTIFIER, RPAREN."""
        tokens = self._lex("(a . b)")
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[-1].type == TokenType.RPAREN
        dot_tokens = [t for t in tokens if t.type == TokenType.DOT]
        assert len(dot_tokens) == 1

    def test_deeply_nested_list(self):
        """A deeply nested list produces the correct number of parens."""
        expr = "(" * 50 + "x" + ")" * 50
        tokens = self._lex(expr)
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        rparen_count = sum(1 for t in tokens if t.type == TokenType.RPAREN)
        assert lparen_count == 50
        assert rparen_count == 50
