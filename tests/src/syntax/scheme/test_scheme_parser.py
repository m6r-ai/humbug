"""Tests for Scheme parser."""

from syntax.scheme.scheme_parser import SchemeParser, SchemeParserState
from syntax.lexer import TokenType


class TestSchemeParser:
    """Tests for Scheme parser."""

    def _parse(self, source: str, prev_state=None) -> tuple:
        parser = SchemeParser()
        state = parser.parse(prev_state, source)
        return list(parser._tokens), state

    def test_parse_simple_expression(self):
        """A simple expression parses without error."""
        tokens, state = self._parse("(+ 1 2)")
        assert isinstance(state, SchemeParserState)
        assert state.parsing_continuation is False
        assert state.continuation_state == 0

    def test_parse_empty_input(self):
        """Empty input produces no tokens and a clean state."""
        tokens, state = self._parse("")
        assert len(tokens) == 0
        assert state.parsing_continuation is False

    def test_parse_returns_scheme_parser_state(self):
        """parse() always returns a SchemeParserState."""
        _, state = self._parse("(define x 42)")
        assert isinstance(state, SchemeParserState)

    def test_parse_keywords(self):
        """Keywords are correctly identified in a parsed expression."""
        tokens, _ = self._parse("(define (square x) (* x x))")
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(kw_tokens) == 1
        assert kw_tokens[0].value == "define"

    def test_parse_numbers(self):
        """Numbers are correctly identified in a parsed expression."""
        tokens, _ = self._parse("(+ 1 2 3)")
        num_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(num_tokens) == 3

    def test_parse_strings(self):
        """Strings are correctly passed through by the parser."""
        tokens, _ = self._parse('(display "hello")')
        str_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(str_tokens) == 1
        assert str_tokens[0].value == '"hello"'

    def test_parse_booleans(self):
        """Booleans are correctly passed through by the parser."""
        tokens, _ = self._parse("(if #t 1 0)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(bool_tokens) == 1
        assert bool_tokens[0].value == "#t"

    def test_parse_characters(self):
        """Character literals are correctly passed through by the parser."""
        tokens, _ = self._parse("(char=? #\\a #\\b)")
        char_tokens = [t for t in tokens if t.type == TokenType.CHARACTER]
        assert len(char_tokens) == 2

    def test_parse_comment(self):
        """Single-line comments are correctly passed through by the parser."""
        tokens, _ = self._parse("; this is a comment")
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == "; this is a comment"

    def test_parse_block_comment(self):
        """Block comments are correctly passed through by the parser."""
        tokens, _ = self._parse("#| block comment |#")
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

    def test_parse_delimiters(self):
        """Parentheses are correctly identified."""
        tokens, _ = self._parse("()")
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN

    def test_parse_dot_becomes_operator(self):
        """A dot in pair notation is converted to OPERATOR by the parser."""
        tokens, _ = self._parse("(a . b)")
        dot_tokens = [t for t in tokens if t.value == "."]
        assert len(dot_tokens) == 1
        assert dot_tokens[0].type == TokenType.OPERATOR

    def test_parse_vector_start(self):
        """A vector #( is correctly passed through as VECTOR_START."""
        tokens, _ = self._parse("#(1 2 3)")
        assert tokens[0].type == TokenType.VECTOR_START
        assert tokens[0].value == "#("

    def test_parse_vector_rparen_decrements_state(self):
        """The closing ) of a vector correctly decrements continuation_state."""
        tokens, state = self._parse("#(1 2 3)")
        assert state.continuation_state == 0
        assert state.in_vector is False

    def test_parse_unclosed_vector(self):
        """An unclosed vector leaves in_vector=True and continuation_state > 0."""
        tokens, state = self._parse("#(1 2 3")
        assert state.in_vector is True

    def test_parse_multiline_string_start(self):
        """An unclosed string sets parsing_continuation=True."""
        _, state = self._parse('"unclosed')
        assert state.parsing_continuation is True
        assert state.continuation_state == 1
        assert state.lexer_state.in_string is True

    def test_parse_multiline_string_continuation(self):
        """A string continuation line closes the string correctly."""
        _, state1 = self._parse('"line one')
        tokens2, state2 = self._parse('line two"', prev_state=state1)
        assert state2.parsing_continuation is False
        assert state2.lexer_state.in_string is False
        str_tokens = [t for t in tokens2 if t.type == TokenType.STRING]
        assert len(str_tokens) == 1

    def test_parse_block_comment_continuation(self):
        """An unclosed block comment sets parsing_continuation=True."""
        _, state = self._parse("#| unclosed")
        assert state.parsing_continuation is True
        assert state.lexer_state.in_comment is True

    def test_parse_block_comment_continuation_closes(self):
        """A block comment closed on a subsequent line clears the state."""
        _, state1 = self._parse("#| line one")
        tokens2, state2 = self._parse("line two |#", prev_state=state1)
        assert state2.parsing_continuation is False
        assert state2.lexer_state.in_comment is False
        comment_tokens = [t for t in tokens2 if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

    def test_parse_preserves_lexer_state(self):
        """The parser correctly preserves lexer state across lines."""
        _, state1 = self._parse('"unclosed')
        assert state1.lexer_state is not None
        assert state1.lexer_state.in_string is True
        _, state2 = self._parse('closed"', prev_state=state1)
        assert state2.lexer_state.in_string is False

    def test_parse_real_fibonacci(self):
        """A real Scheme Fibonacci definition parses without error."""
        source = "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
        tokens, state = self._parse(source)
        assert state.parsing_continuation is False
        error_tokens = [t for t in tokens if t.type == TokenType.ERROR]
        assert len(error_tokens) == 0

    def test_parse_define_syntax(self):
        """A define-syntax expression parses correctly."""
        source = "(define-syntax my-and (syntax-rules () ((my-and) #t) ((my-and e) e)))"
        tokens, state = self._parse(source)
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        kw_values = [t.value for t in kw_tokens]
        assert "define-syntax" in kw_values
        assert "syntax-rules" in kw_values

    def test_parse_vector_with_characters(self):
        """A vector of character literals parses correctly."""
        tokens, state = self._parse("#(#\\a #\\e #\\i #\\o #\\u)")
        assert tokens[0].type == TokenType.VECTOR_START
        char_tokens = [t for t in tokens if t.type == TokenType.CHARACTER]
        assert len(char_tokens) == 5
        assert state.in_vector is False

    def test_parse_pair_notation(self):
        """Pair notation (a . b) produces a DOT converted to OPERATOR."""
        tokens, _ = self._parse("(a . b)")
        types = [t.type for t in tokens]
        assert TokenType.LPAREN in types
        assert TokenType.RPAREN in types
        assert TokenType.OPERATOR in types

    def test_parse_multiple_dots_in_expression(self):
        """Multiple dots in an expression are all converted to OPERATOR."""
        tokens, _ = self._parse("((a . b) (c . d))")
        dot_tokens = [t for t in tokens if t.type == TokenType.OPERATOR and t.value == "."]
        assert len(dot_tokens) == 2

    def test_parse_ellipsis_is_identifier(self):
        """The '...' ellipsis is an IDENTIFIER, not three OPERATOR tokens."""
        tokens, _ = self._parse("(syntax-rules () ((f x ... y ...) x))")
        ellipsis_tokens = [t for t in tokens if t.value == "..."]
        assert len(ellipsis_tokens) == 2
        assert all(t.type == TokenType.IDENTIFIER for t in ellipsis_tokens)
