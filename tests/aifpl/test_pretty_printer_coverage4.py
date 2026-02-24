"""Test cases to hit remaining uncovered lines in pretty printer."""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


class TestExcessiveBlankLines:
    """Test the blank_count > 2 branch (line 125->122)."""

    def test_more_than_two_consecutive_blank_lines(self):
        """Test that more than 2 consecutive blank lines are reduced to 2."""
        printer = AIFPLPrettyPrinter()

        # Create input with many blank lines
        code = "(+ 1 2)\n\n\n\n\n\n\n(+ 3 4)"
        result = printer.format(code)

        # Count max consecutive blank lines
        lines = result.split("\n")
        max_consecutive = 0
        current = 0
        for line in lines:
            if line == "":
                current += 1
                max_consecutive = max(max_consecutive, current)
            else:
                current = 0

        # Should be reduced to at most 2
        assert max_consecutive <= 2
        # But should have at least 1 blank line
        assert max_consecutive >= 1


class TestMalformedExpressions:
    """Test malformed expressions to hit error handling branches."""

    def test_multiline_list_without_closing_paren_lines_338_341(self):
        """Test line 338->341: Multiline list missing closing paren."""
        printer = AIFPLPrettyPrinter()

        # Malformed list - missing closing paren
        code = "(+ 1 2 3 4 5 6 7 8 9 10"
        result = printer.format(code)

        # Should still format something
        assert "+" in result

    def test_let_without_bindings_list_current_token_none_lines_351_354(self):
        """Test line 351->354: Let with no bindings (current_token is None)."""
        printer = AIFPLPrettyPrinter()

        # Let with no bindings at all - just ends
        code = "(let"
        result = printer.format(code)

        # Should format what it can
        assert "let" in result

    def test_let_form_body_with_rparen_lines_420_441(self):
        """Test line 420->441: Let form where body check finds RPAREN."""
        printer = AIFPLPrettyPrinter()

        # Let with bindings but no body
        code = "(let ((x 5)))"
        result = printer.format(code)

        # Should format the bindings
        assert "(x 5)" in result

    def test_let_form_body_after_comments_with_rparen_lines_435_441(self):
        """Test line 435->441: Let form with comments but then RPAREN (no body)."""
        printer = AIFPLPrettyPrinter()

        # Let with bindings, comment, but no body
        code = """(let ((x 5))
  ; comment but no body
)"""
        result = printer.format(code)

        # Should format the bindings and comment
        assert "(x 5)" in result
        assert "; comment but no body" in result

    def test_binding_without_name_lines_468_481(self):
        """Test line 468->481: Binding where current_token is None (no name)."""
        printer = AIFPLPrettyPrinter()

        # Binding that's just open paren
        code = "(let (("
        result = printer.format(code)

        # Should format what it can
        assert "let" in result

    def test_binding_without_value_lines_482_485(self):
        """Test line 482->485: Binding with name but no value (RPAREN immediately)."""
        printer = AIFPLPrettyPrinter()

        # Binding with name but missing value
        code = "(let ((x)) x)"
        result = printer.format(code)

        # Should format the binding
        assert "(x)" in result

    def test_lambda_without_params_list_lines_494_497(self):
        """Test line 494->497: Lambda with non-list params (current_token is None)."""
        printer = AIFPLPrettyPrinter()

        # Lambda with no params list
        code = "(lambda"
        result = printer.format(code)

        # Should format what it can
        assert "lambda" in result

    def test_lambda_params_without_closing_paren_lines_514_517(self):
        """Test line 514->517: Lambda params without closing paren."""
        printer = AIFPLPrettyPrinter()

        # Lambda with params but missing closing paren for params
        code = "(lambda (x y"
        result = printer.format(code)

        # Should format what it can
        assert "lambda" in result
        assert "x" in result

    def test_lambda_without_body_lines_537_543(self):
        """Test line 537->543: Lambda without body (RPAREN after params)."""
        printer = AIFPLPrettyPrinter()

        # Lambda with params but no body
        code = "(lambda (x))"
        result = printer.format(code)

        # Should format params
        assert "(x)" in result

    def test_if_without_then_branch_lines_568_573(self):
        """Test line 568->573: If without then branch (RPAREN after condition)."""
        printer = AIFPLPrettyPrinter()

        # If with just condition
        code = "(if #t)"
        result = printer.format(code)

        # Should format condition
        assert "#t" in result

    def test_match_without_value_lines_687_690(self):
        """Test line 687->690: Match without closing paren."""
        printer = AIFPLPrettyPrinter()

        # Match without closing paren
        code = "(match x"
        result = printer.format(code)

        # Should format what it can
        assert "match" in result


class TestStandaloneCommentsInSpecialForms:
    """Test standalone comments that appear after expressions."""

    def test_standalone_comment_after_match_clause(self):
        """Test standalone comment after match clause (boolean-not EOL)."""
        printer = AIFPLPrettyPrinter()

        code = """(match x
  (1 "one")
  ; standalone comment
)"""
        result = printer.format(code)

        # Comment should be preserved
        assert "; standalone comment" in result
        # Should be on its own line
        assert "\n  ; standalone comment" in result

    def test_standalone_comment_after_lambda_body(self):
        """Test standalone comment after lambda body."""
        printer = AIFPLPrettyPrinter()

        code = """(lambda (x)
  (+ x 1)
  ; comment after body
)"""
        result = printer.format(code)

        # Comment should be preserved
        assert "; comment after body" in result

    def test_standalone_comment_after_if_else(self):
        """Test standalone comment after if else branch."""
        printer = AIFPLPrettyPrinter()

        code = """(if #t
  42
  99
  ; comment after else
)"""
        result = printer.format(code)

        # Comment should be preserved
        assert "; comment after else" in result


class TestEdgeCasesForCompleteness:
    """Additional edge cases to ensure robustness."""

    def test_empty_input(self):
        """Test empty input."""
        printer = AIFPLPrettyPrinter()
        result = printer.format("")
        assert result == ""

    def test_just_whitespace(self):
        """Test input with just whitespace."""
        printer = AIFPLPrettyPrinter()
        result = printer.format("   \n  \n  ")
        assert result == ""

    def test_just_comment(self):
        """Test input with just a comment."""
        printer = AIFPLPrettyPrinter()
        result = printer.format("; just a comment")
        assert "; just a comment" in result

    def test_multiple_top_level_expressions_with_many_blanks(self):
        """Test multiple expressions with excessive blank lines."""
        printer = AIFPLPrettyPrinter()

        code = """(+ 1 2)


\n

(+ 3 4)"""
        result = printer.format(code)

        # Should have both expressions
        assert "(+ 1 2)" in result
        assert "(+ 3 4)" in result
        # But not excessive blanks
        assert "\n\n\n\n" not in result


class TestStandaloneCommentsWithClosingParens:
    """Test that standalone comments don't consume closing parens."""

    def test_match_standalone_comment_closing_paren_separate(self):
        """Test that match standalone comment doesn't consume closing paren."""
        from aifpl.aifpl_lexer import AIFPLLexer

        printer = AIFPLPrettyPrinter()
        lexer = AIFPLLexer()

        code = """(match x
  (1 "one")
  ; standalone comment
)"""
        result = printer.format(code)

        # Lex the result to verify tokens
        tokens = lexer.lex(result, preserve_comments=True)

        # Find the comment token
        comment_token = None
        rparen_after_comment = None
        for i, token in enumerate(tokens):
            if token.type.name == 'COMMENT' and 'standalone comment' in token.value:
                comment_token = token
                # Check next token
                if i + 1 < len(tokens):
                    rparen_after_comment = tokens[i + 1]
                break

        # Comment should not contain the closing paren
        assert comment_token is not None
        assert ')' not in comment_token.value

        # Next token should be RPAREN
        assert rparen_after_comment is not None
        assert rparen_after_comment.type.name == 'RPAREN'

    def test_lambda_standalone_comment_no_body_closing_paren_separate(self):
        """Test lambda with standalone comment but no body doesn't consume closing paren."""
        from aifpl.aifpl_lexer import AIFPLLexer

        printer = AIFPLPrettyPrinter()
        lexer = AIFPLLexer()

        code = """(lambda (x)
  ; comment but no body
)"""
        result = printer.format(code)

        # Lex the result
        tokens = lexer.lex(result, preserve_comments=True)

        # Find the comment
        comment_token = None
        for token in tokens:
            if token.type.name == 'COMMENT':
                comment_token = token
                break

        # Comment should not contain closing paren
        assert comment_token is not None
        assert ')' not in comment_token.value

        # Should have a separate RPAREN token
        rparen_count = sum(1 for t in tokens if t.type.name == 'RPAREN')
        assert rparen_count >= 2  # One for params, one for lambda

    def test_if_standalone_comment_no_then_closing_paren_separate(self):
        """Test if with standalone comment but no then branch doesn't consume closing paren."""
        from aifpl.aifpl_lexer import AIFPLLexer

        printer = AIFPLPrettyPrinter()
        lexer = AIFPLLexer()

        code = """(if #t
  ; comment
)"""
        result = printer.format(code)

        # Lex the result
        tokens = lexer.lex(result, preserve_comments=True)

        # Find the comment
        comment_token = None
        for token in tokens:
            if token.type.name == 'COMMENT':
                comment_token = token
                break

        # Comment should not contain closing paren
        assert comment_token is not None
        assert ')' not in comment_token.value

        # Should have a separate RPAREN token
        rparen_count = sum(1 for t in tokens if t.type.name == 'RPAREN')
        assert rparen_count >= 1


class TestStandaloneCommentsIndentation:
    """Test that closing parens are properly indented after standalone comments."""

    def test_lambda_standalone_comment_no_body_indented_closing_paren(self):
        """Test lambda closing paren is indented after standalone comment with no body."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((f (lambda (x)
           ; comment but no body
         )))
  (f 5))"""
        result = printer.format(code)

        # The closing paren should be indented to match the opening (lambda
        # With new formatting, closing parens align with lambda opening paren (6 spaces)
        # Check that closing parens are on the next line with 6 spaces
        assert "; comment but no body\n      )))" in result

    def test_if_standalone_comment_indented_closing_paren(self):
        """Test if closing paren is indented after standalone comment."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((x 5))
  (if (> x 0)
    ; comment
  ))"""
        result = printer.format(code)

        # The closing paren should be indented
        assert "; comment\n  ))" in result

    def test_match_standalone_comment_indented_closing_paren(self):
        """Test match closing paren is indented after standalone comment."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((x 5))
  (match x
    ; comment
  ))"""
        result = printer.format(code)

        # The closing paren should be indented
        assert "; comment\n  ))" in result


class TestStandaloneCommentsNoDoubleNewline:
    """Test that standalone comments don't create double newlines before body."""

    def test_lambda_standalone_comment_with_body_single_newline(self):
        """Test lambda with standalone comment and body has single newline between them."""
        printer = AIFPLPrettyPrinter()

        code = """(lambda (x)
  ; comment before body
  (+ x 1))"""
        result = printer.format(code)

        # Should have single newline between comment and body, not double
        assert "\n\n" not in result or result.count("\n\n") == 0
        assert "; comment before body\n  (+ x 1)" in result

    def test_if_standalone_comment_then_single_newline(self):
        """Test if with standalone comment before then has single newline."""
        printer = AIFPLPrettyPrinter()

        code = """(if #t
  ; then comment
  42
  99)"""
        result = printer.format(code)

        # Should not have double newline
        assert "; then comment\n  42" in result

    def test_if_standalone_comment_else_single_newline(self):
        """Test if with standalone comment before else has single newline."""
        printer = AIFPLPrettyPrinter()

        code = """(if #t
  42
  ; else comment
  99)"""
        result = printer.format(code)

        # Should not have double newline
        assert "; else comment\n  99" in result

    def test_match_standalone_comment_with_clauses_formatting(self):
        """Test match with standalone comments between clauses."""
        printer = AIFPLPrettyPrinter()

        code = """(match x
  (1 "one")
  ; standalone comment
  (2 "two"))"""
        result = printer.format(code)

        # Comment should be preserved and properly formatted
        assert "; standalone comment" in result
        assert "\n  (2 \"two\")" in result
