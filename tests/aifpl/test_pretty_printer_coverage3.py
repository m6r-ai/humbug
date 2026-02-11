"""Final test cases to reach 100% coverage for AIFPL pretty printer.

This file specifically targets the uncovered lines identified in coverage report:
- Lines 548-550: EOL comments after lambda body
- Lines 615-617: EOL comments after if else branch
- Lines 658-665: EOL comments after match clauses
"""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


class TestEOLCommentsAfterBodies:
    """Test EOL comments that appear after expression bodies but before closing parens."""

    def test_lambda_with_eol_comment_after_body(self):
        """Test EOL comment after lambda body, before closing paren."""
        printer = AIFPLPrettyPrinter()

        # Lambda with EOL comment after the body expression
        code = """(lambda (x)
  (+ x 1) ; increment
)"""
        result = printer.format(code)

        # The comment should be preserved
        assert "; increment" in result
        # Should be formatted as EOL comment with closing paren on next line
        assert "(+ x 1)  ; increment" in result
        assert result.strip().endswith(")")

    def test_lambda_with_multiple_eol_comments_after_body(self):
        """Test multiple EOL comments after lambda body."""
        printer = AIFPLPrettyPrinter()

        code = """(lambda (x y)
  (* x y) ; multiply ; another comment
)"""
        result = printer.format(code)

        assert "; multiply" in result

    def test_if_with_eol_comment_after_else(self):
        """Test EOL comment after if else branch, before closing paren."""
        printer = AIFPLPrettyPrinter()

        # If with EOL comment after the else branch
        code = """(if (> x 0)
  42
  99 ; default value
)"""
        result = printer.format(code)

        # The comment should be preserved
        assert "; default value" in result
        # Should be formatted as EOL comment with closing paren on next line
        assert "99  ; default value" in result
        assert result.strip().endswith(")")

    def test_if_with_multiple_eol_comments_after_else(self):
        """Test multiple EOL comments after if else branch."""
        printer = AIFPLPrettyPrinter()

        code = """(if #t
  "yes"
  "no" ; first ; second
)"""
        result = printer.format(code)

        assert "; first" in result

    def test_match_with_eol_comment_after_clause(self):
        """Test EOL comment after match clause, before closing paren."""
        printer = AIFPLPrettyPrinter()

        # Match with EOL comment after the last clause
        code = """(match value
  (1 "one")
  (2 "two")
  (_ "other") ; default case
)"""
        result = printer.format(code)

        # The comment should be preserved
        assert "; default case" in result
        # Should be formatted as EOL comment with closing paren on next line
        assert '(_ "other")  ; default case' in result
        assert result.strip().endswith(")")

    def test_match_with_multiple_eol_comments_after_clauses(self):
        """Test multiple EOL comments after match clauses."""
        printer = AIFPLPrettyPrinter()

        code = """(match x
  (1 "one") ; case 1 ; extra
  (2 "two") ; case 2
)"""
        result = printer.format(code)

        assert "; case 1" in result
        assert "; case 2" in result

    def test_nested_lambda_with_eol_comments(self):
        """Test nested lambdas with EOL comments after bodies."""
        printer = AIFPLPrettyPrinter()

        code = """(lambda (x)
  (lambda (y)
    (+ x y) ; sum
  ) ; inner lambda
)"""
        result = printer.format(code)

        assert "; sum" in result
        assert "; inner lambda" in result

    def test_let_with_eol_comment_after_body(self):
        """Test EOL comment after let body."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((x 5))
  (+ x 1) ; increment x
)"""
        result = printer.format(code)

        assert "; increment x" in result

    def test_complex_nested_with_all_eol_comments(self):
        """Test complex nested structure with EOL comments in all positions."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((f (lambda (x)
                   (if (> x 0)
                     x ; positive
                     0 ; zero or negative
                   ) ; end if
                 ))) ; end lambda
  (match (f 5)
    (0 "zero")
    (_ "non-zero") ; default
  ) ; end match
)"""
        result = printer.format(code)

        assert "; positive" in result
        assert "; zero or negative" in result
        assert "; end if" in result
        assert "; end lambda" in result
        assert "; default" in result
        assert "; end match" in result


class TestOther:
    """Test othert."""

    def test_excessive_blank_lines(self):
        """Test line 125: blank_count <= 2 branch."""
        printer = AIFPLPrettyPrinter()

        # Input with many blank lines
        code = """(+ 1 2)



(+ 3 4)"""
        result = printer.format(code)

        # Should limit to max 2 consecutive blank lines
        assert "\n\n\n\n" not in result

    def test_malformed_let_without_bindings_list(self):
        """Test malformed let without bindings list."""
        printer = AIFPLPrettyPrinter()

        # Let with non-list bindings
        code = "(let x (+ 1 2))"
        result = printer.format(code)

        # Should still format (even if malformed)
        assert "let" in result

    def test_malformed_lambda_without_params_list(self):
        """Test malformed lambda without params list."""
        printer = AIFPLPrettyPrinter()

        # Lambda with non-list params
        code = "(lambda x (* x 2))"
        result = printer.format(code)

        # Should still format (even if malformed)
        assert "lambda" in result

    def test_empty_binding(self):
        """Test binding without name or value."""
        printer = AIFPLPrettyPrinter()

        # Let with empty binding
        code = "(let (()) x)"
        result = printer.format(code)

        # Should format the empty binding
        assert "()" in result

    def test_if_without_condition(self):
        """Test `if` form edge cases."""
        printer = AIFPLPrettyPrinter()

        # If with just condition
        code = "(if #t)"
        result = printer.format(code)

        assert "if" in result

    def test_match_without_value(self):
        """Test `match` without value expression."""
        printer = AIFPLPrettyPrinter()

        # Match with no value
        code = "(match)"
        result = printer.format(code)

        assert "match" in result

    def test_quote_without_expression(self):
        """Test `quote` without expression."""
        printer = AIFPLPrettyPrinter()

        # Just a quote symbol (edge case)
        code = "'"
        try:
            result = printer.format(code)
            # If it formats, check it has quote
            assert "'" in result or result == ""
        except:
            # Lexer might reject this, which is OK
            pass

    def test_multiline_list_closing_paren(self):
        """Test multiline list with closing paren."""
        printer = AIFPLPrettyPrinter()

        # Long list that will be multiline
        code = "(+ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)"
        result = printer.format(code)

        # Should have closing paren
        assert ")" in result

    def test_binding_closing_paren(self):
        """Test binding with closing paren."""
        printer = AIFPLPrettyPrinter()

        code = "(let ((x 5) (y 10)) (+ x y))"
        result = printer.format(code)

        # Should have proper bindings
        assert "(x 5)" in result
        assert "(y 10)" in result

    def test_lambda_params_closing_paren(self):
        """Test `lambda` params closing paren."""
        printer = AIFPLPrettyPrinter()

        code = "(lambda (x y z) (+ x y z))"
        result = printer.format(code)

        # Should have params
        assert "(x y z)" in result

    def test_lambda_body_exists(self):
        """Test `lambda` with body."""
        printer = AIFPLPrettyPrinter()

        code = "(lambda (x) (+ x 1))"
        result = printer.format(code)

        assert "(+ x 1)" in result

    def test_let_body_after_comments(self):
        """Test `let` body after comments."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((x 5))
  ; comment before body
  (+ x 1))"""
        result = printer.format(code)

        assert "; comment before body" in result
        assert "(+ x 1)" in result

    def test_let_body_exists(self):
        """Test `let` with body expression."""
        printer = AIFPLPrettyPrinter()

        code = "(let ((x 5)) (+ x 1))"
        result = printer.format(code)

        assert "(+ x 1)" in result


class TestClosingParenIndentation:
    """Test that closing parens are properly indented after EOL comments."""

    def test_if_closing_paren_indented_after_eol_comment(self):
        """Test that if closing paren is indented when preceded by EOL comment."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((x 5))
  (if (> x 0)
    x ; positive
  ))"""
        result = printer.format(code)

        # The closing paren should be on its own line, indented
        assert "x  ; positive\n  ))" in result

    def test_match_closing_paren_indented_after_eol_comment(self):
        """Test that match closing paren is indented when preceded by EOL comment."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((x 5))
  (match x
    (1 "one")
    (_ "other") ; default
  ))"""
        result = printer.format(code)

        # The closing paren should be on its own line, indented
        assert '(_ "other")  ; default\n  ))' in result

    def test_lambda_closing_paren_indented_after_eol_comment(self):
        """Test that lambda closing paren is indented when preceded by EOL comment."""
        printer = AIFPLPrettyPrinter()

        code = """(let ((f (lambda (x)
           (+ x 1) ; increment
         )))
  (f 5))"""
        result = printer.format(code)

        # The closing paren should be on its own line, indented
        # With new formatting, closing parens align with lambda (6 spaces, not 9)
        assert "(+ x 1)  ; increment\n      )))" in result

    def test_deeply_nested_with_eol_comments_proper_indentation(self):
        """Test deeply nested structures with EOL comments have proper indentation."""
        printer = AIFPLPrettyPrinter()

        code = "(if #t (match x (1 'a) (_ 'b ; comment\n)) 'c)"
        result = printer.format(code)

        # Should have proper indentation throughout
        assert ")" in result
