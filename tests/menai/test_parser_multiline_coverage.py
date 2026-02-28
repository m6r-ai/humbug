"""
Tests that exercise previously uncovered loops in the parser.

_line_col_to_char for-loop
---------------------------
Iterates over preceding lines to compute a character offset.  The loop body
only executes when `line > 1`, i.e. when the unclosed opening paren is on the
second line or later of the source string.  All previous tests pass single-line
expressions (or multi-line expressions whose *outermost* paren is still on
line 1), so the loop was never entered.

detect_expression_type whitespace-skipping while-loop
------------------------------------------------------
Skips any whitespace between the opening paren and the first symbol, e.g.
`( lambda …` or `(\n  if …`.  This loop was never entered because every
existing test writes `(keyword` with no space after the paren.  The tests
below use forms like `( lambda`, `( if`, `(\tlet`, and a paren followed only
by whitespace (the `i >= len` early-return branch).
"""

import pytest
from menai import MenaiLexer, MenaiParser, MenaiParseError


class TestLineColToCharLoop:
    """Exercises the for-loop inside _line_col_to_char."""

    def test_unclosed_paren_on_line_2(self):
        """
        Unclosed paren on line 2 forces _line_col_to_char to iterate once.

        Source layout (1-indexed lines):
          line 1: (let (
          line 2:   (x (+ 1 2    ← opening paren of binding on line 2, never closed
        """
        code = "(let (\n  (x (+ 1 2"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        # The error must reference the binding on line 2
        assert "line 2" in error.context
        # Variable name should appear in the context
        assert "'x'" in error.context

    def test_unclosed_paren_on_line_3(self):
        """
        Unclosed paren on line 3 forces _line_col_to_char to iterate twice.

        Source layout:
          line 1: (let (
          line 2:   (x 1)
          line 3:   (y (+ 2   ← binding opened on line 3, never closed
        """
        code = "(let (\n  (x 1)\n  (y (+ 2"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        assert "line 3" in error.context
        assert "'y'" in error.context

    def test_unclosed_paren_on_line_4(self):
        """
        Unclosed paren on line 4 forces _line_col_to_char to iterate three times.

        Source layout:
          line 1: (let (
          line 2:   (a 10)
          line 3:   (b 20)
          line 4:   (c (lambda (n)   ← binding opened on line 4, never closed
        """
        code = "(let (\n  (a 10)\n  (b 20)\n  (c (lambda (n)"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        assert "line 4" in error.context
        assert "'c'" in error.context

    def test_context_snippet_content_from_line_2(self):
        """
        Verify that get_context_snippet correctly extracts text starting on line 2.

        The snippet for the frame at line 2 should begin with the content of
        that line, not with content from line 1.
        """
        code = "(let (\n  (myvar (integer+ 1 2"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        # The context snippet for the binding frame on line 2 should contain
        # the text from that line, not the text from line 1.
        assert "myvar" in error.context

    def test_detect_expression_type_on_line_2(self):
        """
        Verify that detect_expression_type works correctly for a paren on line 2.

        A (lambda …) opened on line 2 should be identified as 'lambda function'.
        """
        code = "(let (\n  (f (lambda (x) (+ x"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        # The lambda opened on line 2 should be classified correctly
        assert "lambda" in error.context

    def test_multiline_nested_let_line_col_to_char(self):
        """
        Nested let where the inner let's opening paren is on line 2.

        This ensures _line_col_to_char is called with line=2 from within
        detect_expression_type for the inner let frame.
        """
        code = "(let ((x 1))\n  (let ((y"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        assert "let" in error.context
        assert "'y'" in error.context


class TestDetectExpressionTypeWhitespaceLoop:
    """
    Exercises the whitespace-skipping while-loop in detect_expression_type.

    The loop on the line:
        while i < len(self.expression) and self.expression[i].isspace():
            i += 1
    only executes when the opening paren is followed by at least one whitespace
    character before the first symbol.  All pre-existing tests use `(keyword`
    with no gap, so the loop body was never reached.
    """

    def test_space_before_lambda_keyword(self):
        """
        `( lambda …` — one space between paren and keyword.

        detect_expression_type must skip the space to read 'lambda' and return
        'lambda function'.
        """
        code = "( lambda (x) (+ x"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()
        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        assert "lambda" in error.context

    def test_space_before_if_keyword(self):
        """
        `( if …` — space before keyword, classified as 'if expression'.
        """
        code = "( if (> x 0) x"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        assert "if" in error.context

    def test_tab_before_keyword(self):
        """
        `(\tlet …` — tab character between paren and keyword.
        """
        code = "(\tlet ((x 5)) x"
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        assert "let" in error.context

    def test_paren_followed_by_only_whitespace_returns_list(self):
        """
        `( ` with nothing after the whitespace — hits the `i >= len` early-return
        branch and returns 'list'.

        We trigger this by making the *outermost* unclosed paren contain only
        spaces so that when detect_expression_type scans past the whitespace it
        reaches end-of-string and falls through to `return "list"`.
        """
        code = "(  "
        lexer = MenaiLexer()
        tokens = lexer.lex(code)
        parser = MenaiParser()

        with pytest.raises(MenaiParseError) as exc_info:
            parser.parse(tokens, code)

        error = exc_info.value
        # The expression type for `(  ` should fall back to 'list'
        assert "list" in error.context
