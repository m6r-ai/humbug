"""Tests for Scheme comment tokenization."""

from syntax.scheme.scheme_lexer import SchemeLexer, SchemeLexerState
from syntax.lexer import TokenType


class TestSchemeComments:
    """Tests for Scheme comment tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def _lex_with_state(self, source: str, prev_state=None):
        lexer = SchemeLexer()
        state = lexer.lex(prev_state, source)
        return list(lexer._tokens), state

    def test_single_line_comment(self):
        """A semicolon comment is tokenized as COMMENT."""
        tokens = self._lex("; this is a comment")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == "; this is a comment"

    def test_single_line_comment_lone_semicolon(self):
        """A lone semicolon is tokenized as COMMENT."""
        tokens = self._lex(";")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == ";"

    def test_single_line_comment_double_semicolon(self):
        """Multiple semicolons at the start are all part of the COMMENT token."""
        tokens = self._lex(";; section header")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == ";; section header"

    def test_single_line_comment_triple_semicolon(self):
        """Triple-semicolon documentation comments are tokenized as COMMENT."""
        tokens = self._lex(";;; doc comment")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == ";;; doc comment"

    def test_single_line_comment_start_position(self):
        """The start position of a single-line comment is correctly recorded."""
        tokens = self._lex("  ; comment")
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_single_line_comment_after_code(self):
        """A comment after code on the same line produces code tokens then COMMENT."""
        tokens = self._lex("(foo) ; comment")
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == "; comment"

    def test_single_line_comment_consumes_rest_of_line(self):
        """A semicolon comment consumes everything to the end of the input string."""
        tokens = self._lex("foo ; bar baz")
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == "; bar baz"

    def test_single_line_comment_does_not_set_in_comment_state(self):
        """A single-line comment does not set in_comment in the lexer state."""
        _, state = self._lex_with_state("; comment")
        assert state.in_comment is False

    def test_single_line_comment_does_not_continue_to_next_line(self):
        """A single-line comment does not affect the next line's lexing."""
        _, state = self._lex_with_state("; comment")
        tokens2, _ = self._lex_with_state("42", prev_state=state)
        num_tokens = [t for t in tokens2 if t.type == TokenType.NUMBER]
        assert len(num_tokens) == 1
        assert num_tokens[0].value == "42"

    def test_single_line_comment_with_string_inside(self):
        """A string-like sequence inside a comment is not tokenized as a string."""
        tokens = self._lex('; "not a string"')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_single_line_comment_with_hash_inside(self):
        """Hash tokens inside a comment are not processed."""
        tokens = self._lex("; #t #f #x1F")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_block_comment_single_line(self):
        """A block comment on a single line is tokenized as COMMENT."""
        tokens = self._lex("#| this is a block comment |#")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == "#| this is a block comment |#"

    def test_block_comment_empty(self):
        """An empty block comment '#||#' is tokenized as COMMENT."""
        tokens = self._lex("#||#")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == "#||#"

    def test_block_comment_start_position(self):
        """The start position of a block comment is correctly recorded."""
        tokens = self._lex("  #| comment |#")
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].start == 2, f"Expected start=2, got {tokens[0].start}"

    def test_block_comment_closed_clears_state(self):
        """A closed block comment leaves in_comment=False in the lexer state."""
        _, state = self._lex_with_state("#| comment |#")
        assert state.in_comment is False

    def test_block_comment_unclosed_sets_state(self):
        """An unclosed block comment sets in_comment=True in the lexer state."""
        _, state = self._lex_with_state("#| unclosed comment")
        assert state.in_comment is True

    def test_block_comment_unclosed_produces_comment_token(self):
        """An unclosed block comment produces a COMMENT token with everything to end of input."""
        tokens, _ = self._lex_with_state("#| unclosed")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == "#| unclosed"

    def test_block_comment_spanning_two_lines(self):
        """A block comment spanning two lines is continued via in_comment state."""
        tokens1, state1 = self._lex_with_state("#| line one")
        assert state1.in_comment is True
        tokens2, state2 = self._lex_with_state("line two |#", prev_state=state1)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.COMMENT
        assert tokens2[0].value == "line two |#"
        assert state2.in_comment is False

    def test_block_comment_middle_line_keeps_state(self):
        """A middle line of a multi-line block comment keeps in_comment=True."""
        _, state1 = self._lex_with_state("#| start")
        tokens2, state2 = self._lex_with_state("middle", prev_state=state1)
        assert tokens2[0].type == TokenType.COMMENT
        assert state2.in_comment is True

    def test_block_comment_spanning_three_lines(self):
        """A block comment spanning three lines is correctly continued."""
        _, state1 = self._lex_with_state("#| line one")
        _, state2 = self._lex_with_state("line two", prev_state=state1)
        assert state2.in_comment is True
        tokens3, state3 = self._lex_with_state("line three |#", prev_state=state2)
        assert tokens3[0].type == TokenType.COMMENT
        assert state3.in_comment is False

    def test_block_comment_before_code(self):
        """A block comment before code on the same line produces COMMENT then code tokens."""
        tokens = self._lex("#| comment |# foo")
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == "#| comment |#"
        ident_tokens = [t for t in tokens if t.type == TokenType.IDENTIFIER]
        assert len(ident_tokens) == 1
        assert ident_tokens[0].value == "foo"

    def test_block_comment_after_code(self):
        """A block comment after code on the same line produces code tokens then COMMENT."""
        tokens = self._lex("foo #| comment |#")
        assert tokens[0].type == TokenType.IDENTIFIER
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == "#| comment |#"

    def test_semicolon_inside_block_comment(self):
        """A semicolon inside a block comment does not start a new comment."""
        tokens = self._lex("#| ; not a line comment |#")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == "#| ; not a line comment |#"

    def test_hash_inside_block_comment_not_processed(self):
        """A hash inside a block comment that is not |# does not end the comment."""
        tokens = self._lex("#| #t is not a boolean here |#")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_block_comment_with_code_after(self):
        """Code after a closed block comment is correctly tokenized."""
        tokens = self._lex("#| comment |# 42")
        num_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(num_tokens) == 1
        assert num_tokens[0].value == "42"
