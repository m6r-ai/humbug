"""Tests for Scheme lexer edge cases and real-code integration."""

from syntax.scheme.scheme_lexer import SchemeLexer, SchemeLexerState
from syntax.lexer import TokenType


class TestSchemeEdgeCases:
    """Tests for Scheme lexer edge cases and real-code integration."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def _lex_with_state(self, source: str, prev_state=None):
        lexer = SchemeLexer()
        state = lexer.lex(prev_state, source)
        return list(lexer._tokens), state

    def test_empty_input(self):
        """Empty input produces no tokens."""
        tokens = self._lex("")
        assert len(tokens) == 0

    def test_whitespace_only(self):
        """Whitespace-only input produces no meaningful tokens."""
        tokens = self._lex("   \t  ")
        assert all(t.type != TokenType.ERROR for t in tokens)

    def test_empty_input_state(self):
        """Empty input returns a state with in_comment=False and in_string=False."""
        _, state = self._lex_with_state("")
        assert state.in_comment is False
        assert state.in_string is False

    def test_lone_hash_at_end_of_input(self):
        """A lone '#' at end of input produces an ERROR token."""
        tokens = self._lex("#")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.ERROR
        assert tokens[0].value == "#"

    def test_hash_followed_by_unrecognized_char(self):
        """'#' followed by an unrecognized character produces ERROR '#' then IDENTIFIER."""
        tokens = self._lex("#z")
        assert tokens[0].type == TokenType.ERROR
        assert tokens[0].value == "#"
        assert tokens[1].type == TokenType.IDENTIFIER
        assert tokens[1].value == "z"

    def test_hash_followed_by_digit_is_error(self):
        """'#3' produces ERROR '#' then NUMBER '3' — not a valid Scheme prefix."""
        tokens = self._lex("#3")
        assert tokens[0].type == TokenType.ERROR
        assert tokens[0].value == "#"
        assert tokens[1].type == TokenType.NUMBER

    def test_prev_state_none_does_not_raise(self):
        """Passing None as prev_lexer_state does not raise an error."""
        tokens = self._lex("foo")
        assert len(tokens) == 1

    def test_prev_state_in_comment_continues_block_comment(self):
        """A state with in_comment=True causes the next line to continue a block comment."""
        state = SchemeLexerState(in_comment=True, in_string=False)
        tokens, new_state = self._lex_with_state("still in comment |#", prev_state=state)
        assert tokens[0].type == TokenType.COMMENT
        assert new_state.in_comment is False

    def test_prev_state_in_string_continues_string(self):
        """A state with in_string=True causes the next line to continue a string."""
        state = SchemeLexerState(in_comment=False, in_string=True)
        tokens, new_state = self._lex_with_state('continuation"', prev_state=state)
        assert tokens[0].type == TokenType.STRING
        assert new_state.in_string is False

    def test_token_start_positions_are_non_decreasing(self):
        """Token start positions are non-decreasing across a tokenized expression."""
        tokens = self._lex("(define x 42)")
        starts = [t.start for t in tokens]
        for i in range(1, len(starts)):
            assert starts[i] >= starts[i - 1], f"Positions not non-decreasing: {starts}"

    def test_all_token_types_in_one_expression(self):
        """A complex expression exercises many token types at once."""
        source = '(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) ; factorial'
        tokens = self._lex(source)
        types = {t.type for t in tokens}
        assert TokenType.LPAREN in types
        assert TokenType.RPAREN in types
        assert TokenType.KEYWORD in types
        assert TokenType.IDENTIFIER in types
        assert TokenType.NUMBER in types
        assert TokenType.COMMENT in types

    def test_real_scheme_define_function(self):
        """A real Scheme function definition tokenizes without errors."""
        source = "(define (square x) (* x x))"
        tokens = self._lex(source)
        error_tokens = [t for t in tokens if t.type == TokenType.ERROR]
        assert len(error_tokens) == 0
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert kw_tokens[0].value == "define"

    def test_real_scheme_fibonacci(self):
        """A Scheme Fibonacci definition tokenizes without errors."""
        source = "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
        tokens = self._lex(source)
        error_tokens = [t for t in tokens if t.type == TokenType.ERROR]
        assert len(error_tokens) == 0

    def test_real_scheme_let_expression(self):
        """A real Scheme let expression tokenizes correctly."""
        source = "(let ((x 1) (y 2)) (+ x y))"
        tokens = self._lex(source)
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert kw_tokens[0].value == "let"

    def test_real_scheme_cond_expression(self):
        """A real Scheme cond expression tokenizes correctly."""
        source = "(cond ((= x 0) 'zero) (else 'nonzero))"
        tokens = self._lex(source)
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        kw_values = [t.value for t in kw_tokens]
        assert "cond" in kw_values
        assert "else" in kw_values

    def test_real_scheme_define_syntax(self):
        """A define-syntax expression tokenizes the keywords correctly."""
        source = "(define-syntax my-and (syntax-rules () ((my-and) #t) ((my-and e) e)))"
        tokens = self._lex(source)
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        kw_values = [t.value for t in kw_tokens]
        assert "define-syntax" in kw_values
        assert "syntax-rules" in kw_values

    def test_real_scheme_vector_literal(self):
        """A real Scheme vector literal tokenizes correctly."""
        source = "#(1 2 3)"
        tokens = self._lex(source)
        assert tokens[0].type == TokenType.VECTOR_START
        num_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(num_tokens) == 3

    def test_real_scheme_character_usage(self):
        """A Scheme expression using character literals tokenizes correctly."""
        source = "(char=? #\\a #\\b)"
        tokens = self._lex(source)
        char_tokens = [t for t in tokens if t.type == TokenType.CHARACTER]
        assert len(char_tokens) == 2

    def test_mixed_booleans_and_numbers(self):
        """Booleans and numbers in a list are all correctly tokenized."""
        tokens = self._lex("(#t 42 #f 3.14)")
        bool_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        num_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(bool_tokens) == 2
        assert len(num_tokens) == 2

    def test_block_comment_then_string_then_identifier(self):
        """A block comment, string, and identifier on one line all tokenize correctly."""
        source = '#| comment |# "hello" world'
        tokens = self._lex(source)
        assert tokens[0].type == TokenType.COMMENT
        str_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(str_tokens) == 1
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert ident_tokens[-1].value == "world"

    def test_multiple_hash_errors(self):
        """Multiple lone '#' characters each produce an ERROR token."""
        tokens = self._lex("# #")
        error_tokens = [t for t in tokens if t.type == TokenType.ERROR]
        assert len(error_tokens) == 2

    def test_very_long_identifier(self):
        """A very long identifier is tokenized as a single IDENTIFIER token."""
        long_id = "a" * 10000
        tokens = self._lex(long_id)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_very_long_string(self):
        """A very long string is tokenized as a single STRING token."""
        long_str = '"' + "x" * 10000 + '"'
        tokens = self._lex(long_str)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_very_long_comment(self):
        """A very long single-line comment is tokenized as a single COMMENT token."""
        long_comment = ";" + "x" * 10000
        tokens = self._lex(long_comment)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_very_long_block_comment(self):
        """A very long block comment is tokenized as a single COMMENT token."""
        long_comment = "#|" + "x" * 10000 + "|#"
        tokens = self._lex(long_comment)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_whitespace_varieties(self):
        """Identifiers separated by tabs and spaces are all correctly tokenized."""
        source = "foo\tbar\tbaz"
        tokens = self._lex(source)
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 3
        assert ident_tokens[0].value == "foo"
        assert ident_tokens[1].value == "bar"
        assert ident_tokens[2].value == "baz"

    def test_nested_list_with_strings_and_numbers(self):
        """A nested list with strings and numbers tokenizes all elements correctly."""
        source = '((1 "two") (3 "four"))'
        tokens = self._lex(source)
        num_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        str_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(num_tokens) == 2
        assert len(str_tokens) == 2

    def test_zero_variations(self):
        """All zero representations produce NUMBER tokens."""
        for num in ['0', '0.0', '#x0', '#b0', '#o0', '#d0', '0e0', '0i']:
            tokens = self._lex(num)
            assert len(tokens) == 1, f"Expected 1 token for '{num}', got {len(tokens)}"
            assert tokens[0].type == TokenType.NUMBER, f"Expected NUMBER for '{num}'"
