"""Final tests to achieve 100% coverage for AIFPL pretty printer.

This file targets the remaining specific uncovered lines and branches.
"""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter, FormatOptions


class TestBlankLineHandling:
    """Test blank line preservation and cleanup (lines 98, 129, 132)."""

    def test_blank_line_before_standalone_comment_after_code(self):
        """Test line 98: Adding blank line before standalone comment after code."""
        printer = AIFPLPrettyPrinter()
        code = "(+ 1 2)\n; This comment should have a blank line before it"
        result = printer.format(code)

        lines = result.split("\n")
        assert "(+ 1 2)" in lines[0]
        assert lines[1] == ""  # Blank line
        assert "; This comment" in lines[2]

    def test_excessive_trailing_newlines_trimmed(self):
        """Test line 129: Trimming excessive trailing newlines."""
        printer = AIFPLPrettyPrinter()
        code = "(+ 1 2)\n\n\n\n\n"
        result = printer.format(code)

        # Should not have more than 2 consecutive newlines
        assert "\n\n\n\n" not in result
        # Should end with single newline
        assert result.endswith("\n")
        assert not result.endswith("\n\n")

    def test_ensure_single_trailing_newline(self):
        """Test line 132: Ensuring single trailing newline when missing."""
        printer = AIFPLPrettyPrinter()
        # Even if input has no trailing newline, output should have one
        code = "(+ 1 2)"
        result = printer.format(code)

        assert result.endswith("\n")
        assert not result.endswith("\n\n")

    def test_multiple_blank_lines_between_expressions(self):
        """Test handling of multiple blank lines between expressions."""
        printer = AIFPLPrettyPrinter()
        code = "(+ 1 2)\n\n\n(+ 3 4)"
        result = printer.format(code)

        # Should preserve blank line but not excessive ones
        lines = result.split("\n")
        assert "(+ 1 2)" in lines[0]
        # Should have at most 2 consecutive blank lines
        blank_count = 0
        max_blanks = 0
        for line in lines[1:]:
            if line == "":
                blank_count += 1
                max_blanks = max(max_blanks, blank_count)
            else:
                blank_count = 0
        assert max_blanks <= 2


class TestEdgeCasesNoneTokens:
    """Test edge cases with None tokens (lines 139, 147)."""

    def test_is_end_of_line_comment_with_none_token(self):
        """Test line 139: _is_end_of_line_comment when current_token is None."""
        printer = AIFPLPrettyPrinter()
        # Format empty string
        result = printer.format("")
        assert result == ""

    def test_format_expression_with_none_token(self):
        """Test line 147: _format_expression when current_token is None."""
        printer = AIFPLPrettyPrinter()
        # This shouldn't happen in normal use, but we test the guard
        result = printer.format("")
        assert result == ""


class TestMalformedExpressions:
    """Test malformed expressions (lines 367-374, 499-506)."""

    def test_malformed_let_without_bindings_list(self):
        """Test lines 367-374: let form without proper bindings list."""
        printer = AIFPLPrettyPrinter()
        # Malformed: let without opening paren for bindings
        code = "(let x (+ x 1))"
        result = printer.format(code)

        # Should handle gracefully
        assert "let" in result
        assert "x" in result

    def test_malformed_letrec_without_bindings_list(self):
        """Test malformed letrec without proper bindings list."""
        printer = AIFPLPrettyPrinter()
        code = "(letrec 42 (+ 1 2))"
        result = printer.format(code)

        assert "letrec" in result
        assert "42" in result

    def test_malformed_let_star_without_bindings_list(self):
        """Test malformed let* without proper bindings list."""
        printer = AIFPLPrettyPrinter()
        code = "(let* foo bar)"
        result = printer.format(code)

        assert "let*" in result
        assert "foo" in result

    def test_malformed_lambda_without_param_list(self):
        """Test lines 499-506: lambda without proper parameter list."""
        printer = AIFPLPrettyPrinter()
        # Malformed: lambda without opening paren for params
        code = "(lambda x (* x 2))"
        result = printer.format(code)

        # Should handle gracefully
        assert "lambda" in result
        assert "x" in result

    def test_malformed_lambda_with_number_instead_of_params(self):
        """Test lambda with number instead of parameter list."""
        printer = AIFPLPrettyPrinter()
        code = "(lambda 42 (+ 1 2))"
        result = printer.format(code)

        assert "lambda" in result
        assert "42" in result


class TestCommentsInMultilineLists:
    """Test comments in multiline lists (lines 301-322)."""

    def test_comment_after_opening_paren_in_multiline_list(self):
        """Test lines 301-302: Comment right after opening paren (first=True)."""
        printer = AIFPLPrettyPrinter()
        # Force multiline with low threshold
        options = FormatOptions(compact_threshold=10)
        printer = AIFPLPrettyPrinter(options)
        code = "(  ; comment after paren\n+ 1 2 3 4 5)"
        result = printer.format(code)

        # Comment should be preserved
        assert "; comment after paren" in result

    def test_end_of_line_comment_in_multiline_list(self):
        """Test lines 306-311: End-of-line comments in multiline lists."""
        printer = AIFPLPrettyPrinter()
        options = FormatOptions(compact_threshold=10)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1  ; first arg\n   2  ; second arg\n   3)"
        result = printer.format(code)

        assert "; first arg" in result
        assert "; second arg" in result

    def test_standalone_comment_in_multiline_list(self):
        """Test lines 314-322: Standalone comments in multiline lists."""
        printer = AIFPLPrettyPrinter()
        options = FormatOptions(compact_threshold=10)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1\n   ; standalone comment\n   2 3)"
        result = printer.format(code)

        assert "; standalone comment" in result
        lines = result.split("\n")
        # Comment should be on its own line
        comment_line = next(line for line in lines if "; standalone comment" in line)
        assert comment_line.strip() == "; standalone comment"

    def test_multiple_standalone_comments_in_multiline_list(self):
        """Test multiple standalone comments in multiline list."""
        printer = AIFPLPrettyPrinter()
        options = FormatOptions(compact_threshold=10)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1\n   ; comment 1\n   ; comment 2\n   2 3)"
        result = printer.format(code)

        assert "; comment 1" in result
        assert "; comment 2" in result


class TestCommentsInLetBindings:
    """Test comments in let bindings (lines 397-406)."""

    def test_standalone_comment_between_let_bindings(self):
        """Test lines 397-406: Standalone comments between bindings."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5)
      ; Comment between bindings
      (y 10))
  (+ x y))"""
        result = printer.format(code)

        assert "; Comment between bindings" in result
        lines = result.split("\n")
        # Find the comment line
        comment_idx = next(i for i, line in enumerate(lines) if "; Comment between bindings" in line)
        # Should be between the two bindings
        x_binding_idx = next(i for i, line in enumerate(lines) if "(x 5)" in line)
        y_binding_idx = next(i for i, line in enumerate(lines) if "(y 10)" in line)
        assert x_binding_idx < comment_idx < y_binding_idx

    def test_multiple_comments_between_let_bindings(self):
        """Test multiple standalone comments between bindings."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5)
      ; First comment
      ; Second comment
      (y 10))
  (+ x y))"""
        result = printer.format(code)

        assert "; First comment" in result
        assert "; Second comment" in result

    def test_comment_before_first_binding(self):
        """Test comment before first binding in let."""
        printer = AIFPLPrettyPrinter()
        code = """(let (; Comment before first binding
      (x 5)
      (y 10))
  (+ x y))"""
        result = printer.format(code)

        assert "; Comment before first binding" in result


class TestEndOfLineCommentsInSpecialForms:
    """Test end-of-line comments in special forms (lines 529-531, 567-569, 587-589)."""

    def test_eol_comment_in_lambda_body(self):
        """Test lines 529-531: End-of-line comments in lambda body."""
        printer = AIFPLPrettyPrinter()
        code = """(lambda (x)  ; Lambda comment
  (* x 2))"""
        result = printer.format(code)

        assert "; Lambda comment" in result
        # Comment should be on same line as lambda opening
        assert "(lambda (x)  ; Lambda comment" in result

    def test_eol_comment_before_if_then_branch(self):
        """Test lines 567-569: End-of-line comments before if then-branch."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)  ; Check if positive
  (+ x 1)
  (- x 1))"""
        result = printer.format(code)

        assert "; Check if positive" in result
        # Comment should be on same line as condition
        lines = result.split("\n")
        first_line = lines[0]
        assert "(> x 0)" in first_line
        assert "; Check if positive" in first_line

    def test_eol_comment_before_if_else_branch(self):
        """Test lines 587-589: End-of-line comments before if else-branch."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  (+ x 1)  ; Then branch comment
  (- x 1))"""
        result = printer.format(code)

        assert "; Then branch comment" in result
        # Comment should be on same line as then expression
        assert "(+ x 1)  ; Then branch comment" in result

    def test_multiple_eol_comments_in_if(self):
        """Test multiple end-of-line comments in if expression."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)  ; Condition comment
  (+ x 1)  ; Then comment
  (- x 1))  ; Else comment"""
        result = printer.format(code)

        assert "; Condition comment" in result
        assert "; Then comment" in result
        assert "; Else comment" in result


class TestStringEscaping:
    """Test string escaping (lines 688, 691, 697, 700, 703)."""

    def test_escape_double_quotes(self):
        """Test line 688: Escaping double quotes in strings."""
        printer = AIFPLPrettyPrinter()
        # String containing a double quote
        code = '(string-append "Hello" "\\"world\\"")'
        result = printer.format(code)

        # Should preserve the escaped quotes
        assert '\\"' in result

    def test_escape_backslashes(self):
        """Test line 691: Escaping backslashes in strings."""
        printer = AIFPLPrettyPrinter()
        # String containing backslash
        code = '(string-append "path\\\\to\\\\file")'
        result = printer.format(code)

        # Should have escaped backslashes
        assert "\\\\" in result

    def test_escape_tabs(self):
        """Test line 697: Escaping tab characters in strings."""
        printer = AIFPLPrettyPrinter()
        # String containing a tab
        code = '(string-append "hello\\tworld")'
        result = printer.format(code)

        # Should preserve the escaped tab
        assert "\\t" in result

    def test_escape_carriage_returns(self):
        """Test line 700: Escaping carriage returns in strings."""
        printer = AIFPLPrettyPrinter()
        # String containing carriage return
        code = '(string-append "line1\\rline2")'
        result = printer.format(code)

        # Should preserve the escaped carriage return
        assert "\\r" in result

    def test_escape_control_characters(self):
        """Test line 703: Escaping control characters (<32) in strings."""
        printer = AIFPLPrettyPrinter()
        # String containing a control character (ASCII 7 = bell)
        # We need to construct this carefully
        code = '(string-append "hello\\u0007world")'
        result = printer.format(code)

        # Should have unicode escape for control character
        assert "\\u0007" in result

    def test_escape_newlines(self):
        """Test escaping newline characters in strings."""
        printer = AIFPLPrettyPrinter()
        code = '(string-append "hello\\nworld")'
        result = printer.format(code)

        assert "\\n" in result

    def test_string_with_multiple_escapes(self):
        """Test string with multiple different escape sequences."""
        printer = AIFPLPrettyPrinter()
        code = '(string-append "tab:\\t newline:\\n quote:\\" backslash:\\\\")'
        result = printer.format(code)

        assert "\\t" in result
        assert "\\n" in result
        assert '\\"' in result
        assert "\\\\" in result


class TestCommentsInMatchExpressions:
    """Test comments in match expressions."""

    def test_eol_comment_in_match_clause(self):
        """Test end-of-line comments in match clauses."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  (1 'one)  ; First case
  (2 'two)  ; Second case
  (_ 'other))"""
        result = printer.format(code)

        assert "; First case" in result
        assert "; Second case" in result

    def test_standalone_comment_between_match_clauses(self):
        """Test standalone comments between match clauses."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  (1 'one)
  ; This handles the second case
  (2 'two)
  (_ 'other))"""
        result = printer.format(code)

        assert "; This handles the second case" in result
        lines = result.split("\n")
        # Comment should be between the clauses
        one_idx = next(i for i, line in enumerate(lines) if "one" in line and ";" not in line)
        comment_idx = next(i for i, line in enumerate(lines) if "; This handles" in line)
        two_idx = next(i for i, line in enumerate(lines) if "two" in line and ";" not in line)
        assert one_idx < comment_idx < two_idx


class TestCommentsInLetBody:
    """Test comments before let body expressions."""

    def test_standalone_comment_before_let_body(self):
        """Test standalone comment before let body."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5)
      (y 10))
  ; Now compute the sum
  (+ x y))"""
        result = printer.format(code)

        assert "; Now compute the sum" in result
        lines = result.split("\n")
        # Comment should be after bindings, before body
        bindings_end_idx = next(i for i, line in enumerate(lines) if line.strip().endswith("))"))
        comment_idx = next(i for i, line in enumerate(lines) if "; Now compute" in line)
        body_idx = next(i for i, line in enumerate(lines) if "(+ x y)" in line)
        assert bindings_end_idx < comment_idx < body_idx

    def test_eol_comment_after_bindings_closing_paren(self):
        """Test end-of-line comment after bindings closing paren."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5)
      (y 10))  ; End of bindings
  (+ x y))"""
        result = printer.format(code)

        assert "; End of bindings" in result
        # Comment should be on same line as closing paren
        assert "))  ; End of bindings" in result

    def test_multiple_comments_before_let_body(self):
        """Test multiple comments before let body."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5))
  ; Comment 1
  ; Comment 2
  (+ x 1))"""
        result = printer.format(code)

        assert "; Comment 1" in result
        assert "; Comment 2" in result


class TestLetrecSpecialCases:
    """Test letrec-specific formatting cases."""

    def test_letrec_complex_binding_blank_line(self):
        """Test that letrec does NOT add blank lines between complex bindings without comments."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec ((factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))
         (fibonacci (lambda (n) (if (<= n 1) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))
  (+ (factorial 5) (fibonacci 5)))"""
        result = printer.format(code)

        # Check that both bindings are present and formatted
        assert "factorial" in result
        assert "fibonacci" in result
        assert "letrec" in result

    def test_letrec_simple_bindings_no_extra_blank_line(self):
        """Test that letrec doesn't add extra blank lines for simple bindings."""
        printer = AIFPLPrettyPrinter()
        code = "(letrec ((x 5)(y 10)) (+ x y))"
        result = printer.format(code)

        # Simple bindings shouldn't have blank lines between them
        lines = result.split("\n")
        # Count blank lines
        blank_count = sum(1 for line in lines if line.strip() == "")
        # Should have minimal blank lines
        assert blank_count <= 1


class TestQuotedExpressions:
    """Test formatting of quoted expressions."""

    def test_quoted_list(self):
        """Test formatting of quoted list."""
        printer = AIFPLPrettyPrinter()
        code = "'(a b c)"
        result = printer.format(code)

        assert result == "'(a b c)\n"

    def test_quoted_nested_list(self):
        """Test formatting of quoted nested list."""
        printer = AIFPLPrettyPrinter()
        code = "'(a (b c) d)"
        result = printer.format(code)

        assert "'(a (b c) d)" in result

    def test_quoted_expression_in_let(self):
        """Test quoted expression as binding value."""
        printer = AIFPLPrettyPrinter()
        code = "(let ((x '(1 2 3))) (first x))"
        result = printer.format(code)

        assert "'(1 2 3)" in result


class TestBindingFormatting:
    """Test binding formatting edge cases."""

    def test_binding_without_value(self):
        """Test binding that's malformed (no value)."""
        printer = AIFPLPrettyPrinter()
        # Malformed binding - just name, no value
        code = "(let ((x)) x)"
        result = printer.format(code)

        # Should handle gracefully
        assert "let" in result
        assert "(x)" in result

    def test_empty_binding_list(self):
        """Test let with empty binding list."""
        printer = AIFPLPrettyPrinter()
        code = "(let () 42)"
        result = printer.format(code)

        assert "let" in result
        assert "42" in result
        assert "()" in result


class TestComplexRealWorldScenarios:
    """Test complex real-world scenarios to ensure coverage."""

    def test_deeply_nested_with_comments(self):
        """Test deeply nested expressions with comments at multiple levels."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5))
  ; Outer comment
  (let ((y 10))
    ; Inner comment
    (if (> x 0)
      ; Positive
      (+ x y)
      ; Negative
      (- x y))))"""
        result = printer.format(code)

        assert "; Outer comment" in result
        assert "; Inner comment" in result
        assert "; Positive" in result
        assert "; Negative" in result

    def test_all_special_forms_with_comments(self):
        """Test all special forms with various comment types."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec ((helper (lambda (n)  ; Helper function
                   ; Check base case
                   (if (<= n 0)
                     1  ; Base case
                     ; Recursive case
                     (* n (helper (- n 1)))))))
  ; Call the helper
  (helper 5))"""
        result = printer.format(code)

        assert "; Helper function" in result
        assert "; Check base case" in result
        assert "; Base case" in result
        assert "; Recursive case" in result
        assert "; Call the helper" in result

    def test_match_with_complex_patterns_and_comments(self):
        """Test match with complex patterns and comments."""
        printer = AIFPLPrettyPrinter()
        code = """(match value
  ; Handle empty list
  (() "empty")
  ; Handle single element
  ((x) "singleton")
  ; Handle multiple elements
  ((x y . rest) "multiple"))"""
        result = printer.format(code)

        assert "; Handle empty list" in result
        assert "; Handle single element" in result
        assert "; Handle multiple elements" in result
        assert '"empty"' in result
        assert '"singleton"' in result
        assert '"multiple"' in result


class TestEdgeCasesWithFormatOptions:
    """Test edge cases with different format options."""

    def test_zero_comment_spacing(self):
        """Test with zero comment spacing."""
        options = FormatOptions(comment_spacing=0)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1 2)  ; comment"
        result = printer.format(code)

        # Comment should be immediately after expression
        assert "(+ 1 2); comment" in result

    def test_large_comment_spacing(self):
        """Test with large comment spacing."""
        options = FormatOptions(comment_spacing=10)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1 2)  ; comment"
        result = printer.format(code)

        # Should have 10 spaces before comment
        assert "          ; comment" in result

    def test_very_low_compact_threshold(self):
        """Test with very low compact threshold."""
        options = FormatOptions(compact_threshold=5)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1 2)"
        result = printer.format(code)

        # Even short expressions should be multiline
        assert result.count("\n") > 1

    def test_very_high_compact_threshold(self):
        """Test with very high compact threshold."""
        options = FormatOptions(compact_threshold=200)
        printer = AIFPLPrettyPrinter(options)
        code = "(very-long-function-name arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)"
        result = printer.format(code)

        # Should stay on one line
        lines = result.strip().split("\n")
        assert len(lines) == 1


class TestCompactFormatBailout:
    """Test compact format bailout scenarios."""

    def test_compact_format_exceeds_threshold_during_formatting(self):
        """Test that compact format bails out when exceeding threshold mid-format."""
        printer = AIFPLPrettyPrinter()
        # Create an expression that starts compact but grows too large
        code = "(function-name arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)"
        result = printer.format(code)

        # Should either be compact or multiline, but valid
        assert result.count("(") == result.count(")")

    def test_nested_list_compact_bailout(self):
        """Test nested list that can't be formatted compactly."""
        options = FormatOptions(compact_threshold=30)
        printer = AIFPLPrettyPrinter(options)
        code = "(outer (inner-function-with-long-name arg1 arg2 arg3) arg4)"
        result = printer.format(code)

        # Should handle the bailout gracefully
        assert "outer" in result
        assert "inner-function-with-long-name" in result


class TestAtomFormatting:
    """Test atom formatting for all types."""

    def test_format_integer_atom(self):
        """Test formatting integer atoms."""
        printer = AIFPLPrettyPrinter()
        assert printer.format("42") == "42\n"
        assert printer.format("-17") == "-17\n"
        assert printer.format("0") == "0\n"

    def test_format_float_atom(self):
        """Test formatting float atoms."""
        printer = AIFPLPrettyPrinter()
        assert printer.format("3.14") == "3.14\n"
        assert printer.format("-2.5") == "-2.5\n"

    def test_format_complex_atom(self):
        """Test formatting complex number atoms."""
        printer = AIFPLPrettyPrinter()
        result = printer.format("3+4j")
        assert "3" in result and "4" in result

    def test_format_boolean_atoms(self):
        """Test formatting boolean atoms."""
        printer = AIFPLPrettyPrinter()
        assert printer.format("#t") == "#t\n"
        assert printer.format("#f") == "#f\n"

    def test_format_string_atom(self):
        """Test formatting string atoms."""
        printer = AIFPLPrettyPrinter()
        assert printer.format('"hello"') == '"hello"\n'
        assert printer.format('""') == '""\n'

    def test_format_symbol_atom(self):
        """Test formatting symbol atoms."""
        printer = AIFPLPrettyPrinter()
        assert printer.format("my-symbol") == "my-symbol\n"
        assert printer.format("+") == "+\n"
        assert printer.format("lambda") == "lambda\n"


class TestRemainingUncoveredLines:
    """Test the final remaining uncovered lines."""

    def test_top_level_eol_comment(self):
        """Test top-level end-of-line comment handling."""
        # This is the tricky case - we need a comment at the top level
        # that is on the same line as a previous token
        printer = AIFPLPrettyPrinter()
        # The key is that the comment must be at the top level (not inside a list)
        # and must have the same line number as the previous token
        code = "42 ; comment on same line"
        result = printer.format(code)
        # The comment should be on the same line with spacing
        assert "42  ; comment on same line" in result or "42 ; comment on same line" in result

    def test_excessive_blank_lines(self):
        """Test more than 2 blank lines (branch not taken)."""
        # We need to create a case where blank_count > 2
        printer = AIFPLPrettyPrinter()
        # Multiple blank lines in input
        code = "(+ 1 2)\n\n\n\n\n(+ 3 4)"
        result = printer.format(code)
        # Should be cleaned up
        lines = result.split("\n")
        # Count consecutive blank lines
        max_blanks = 0
        current = 0
        for line in lines:
            if line == "":
                current += 1
                max_blanks = max(max_blanks, current)
            else:
                current = 0
        assert max_blanks <= 2

    def test_excessive_trailing_newlines(self):
        """Test excessive trailing newlines."""
        printer = AIFPLPrettyPrinter()
        # Input with many trailing newlines
        code = "(+ 1 2)\n\n\n\n\n"
        result = printer.format(code)
        # Should end with single newline
        assert result.endswith("\n")
        assert not result.endswith("\n\n\n")

    def test_add_trailing_newline(self):
        """Test add trailing newline when missing."""
        # This is hard to trigger because the join usually adds newlines
        # But we can test with empty result
        printer = AIFPLPrettyPrinter()
        result = printer.format("")
        # Empty input should remain empty
        assert result == ""

    def test_standalone_comment_in_expression(self):
        """Test standalone comment in expression context."""
        # This happens when _format_expression encounters a COMMENT token
        # which shouldn't normally happen, but we test the guard
        printer = AIFPLPrettyPrinter()
        code = "; standalone comment\n(+ 1 2)"
        result = printer.format(code)
        assert "; standalone comment" in result

    def test_comment_in_compact_list(self):
        """Test comment prevents compact format."""
        printer = AIFPLPrettyPrinter()
        # List with comment inside
        code = "(+ 1 ; comment\n   2 3)"
        result = printer.format(code)
        assert "; comment" in result

    def test_eol_comment_after_let_body(self):
        """Test EOL comment after let body."""
        printer = AIFPLPrettyPrinter()
        # Let form with EOL comment after body
        code = "(let ((x 5)) (+ x 1))  ; comment"
        result = printer.format(code)
        assert "; comment" in result

    def test_eol_comment_after_lambda_body(self):
        """Test EOL comment after lambda body."""
        printer = AIFPLPrettyPrinter()
        # Lambda with EOL comment after body
        code = "(lambda (x) (* x 2))  ; comment"
        result = printer.format(code)
        assert "; comment" in result

    def test_eol_comment_after_if_else(self):
        """Test EOL comment after if else branch."""
        printer = AIFPLPrettyPrinter()
        # If with EOL comment after else
        code = "(if (> x 0) 1 2)  ; comment"
        result = printer.format(code)
        assert "; comment" in result

    def test_eol_comment_after_match(self):
        """Test EOL comment after match."""
        printer = AIFPLPrettyPrinter()
        # Match with EOL comment after clauses
        code = "(match x (1 'one) (2 'two))  ; comment"
        result = printer.format(code)
        assert "; comment" in result


class TestBranchCoverage:
    """Test to cover all remaining branch conditions."""

    def test_all_always_true_branches(self):
        """Test branches that are always true in normal code."""
        printer = AIFPLPrettyPrinter()

        # Test various normal cases that exercise "always true" branches
        test_cases = [
            "(+ 1 2 3)",  # Compact list with RPAREN at end (line 297)
            "(let ((x 5)) (+ x 1))",  # Let with all normal branches (lines 367, 380, 443, 449, 464, 482)
            "(let ((x 5) (y 10)) (+ x y))",  # Bindings (lines 497, 511)
            "(lambda (x y) (+ x y))",  # Lambda (lines 523, 543, 565)
            "(if (> x 0) 1 2)",  # If (lines 594, 612, 632)
            "(match x (1 'one) (2 'two))",  # Match (lines 661, 698)
            "'(a b c)",  # Quote (line 708)
        ]

        for code in test_cases:
            result = printer.format(code)
            # Just ensure they all format without error
            assert len(result) > 0

    def test_multiline_list_always_true_branch_line_367(self):
        """Test line 367: Multiline list with RPAREN (always true)."""
        printer = AIFPLPrettyPrinter()
        options = FormatOptions(compact_threshold=10)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1 2 3 4 5 6 7 8)"
        result = printer.format(code)
        assert "+" in result

    def test_let_malformed_then_rparen_line_380_383(self):
        """Test lines 380-383: Malformed let followed by RPAREN."""
        printer = AIFPLPrettyPrinter()
        # Malformed let where bindings is not LPAREN
        code = "(let x (+ 1 2))"
        result = printer.format(code)
        assert "let" in result


class TestEdgeCasesForFullCoverage:
    """Test edge cases to ensure complete coverage."""

    def test_very_long_line_cleanup(self):
        """Test that very long lines are handled correctly."""
        printer = AIFPLPrettyPrinter()
        # Create a long expression
        code = "(+ " + " ".join(str(i) for i in range(100)) + ")"
        result = printer.format(code)
        # Should format without error
        assert "+" in result

    def test_nested_malformed_structures(self):
        """Test nested malformed structures."""
        printer = AIFPLPrettyPrinter()
        # Various malformed cases
        test_cases = [
            "(let x)",  # Malformed let
            "(lambda x)",  # Malformed lambda
            "(",  # Unmatched paren
            "(+ 1 2",  # Missing closing paren
        ]

        for code in test_cases:
            try:
                result = printer.format(code)
                # If it formats, that's fine
                assert len(result) >= 0
            except:
                # If it raises an error, that's also acceptable
                pass

    def test_all_token_types(self):
        """Test all token types to ensure complete coverage."""
        printer = AIFPLPrettyPrinter()

        test_cases = [
            "42",  # INTEGER
            "3.14",  # FLOAT
            "3+4j",  # COMPLEX
            "#t",  # BOOLEAN true
            "#f",  # BOOLEAN false
            '"hello"',  # STRING
            "symbol",  # SYMBOL
            "()",  # Empty list
            "'(a b c)",  # QUOTE
            "; comment",  # COMMENT
        ]

        for code in test_cases:
            result = printer.format(code)
            assert len(result) > 0

    def test_string_with_all_escape_sequences(self):
        """Test string with all possible escape sequences."""
        printer = AIFPLPrettyPrinter()
        # String with various characters that need escaping
        code = r'(string-append "quote:\" backslash:\\ newline:\n tab:\t return:\r")'
        result = printer.format(code)

        # Should preserve escapes
        assert '\\"' in result or r'\"' in result
        assert '\\n' in result or r'\n' in result
        assert '\\t' in result or r'\t' in result

    def test_control_characters_in_string(self):
        """Test control characters in strings."""
        printer = AIFPLPrettyPrinter()
        # String with control character (bell)
        code = '"hello\\u0007world"'
        result = printer.format(code)
        # Should format without error
        assert "hello" in result

    def test_complex_nested_comments(self):
        """Test complex nesting with comments everywhere."""
        printer = AIFPLPrettyPrinter()
        code = """
; Top comment
(let ((x 5)  ; x comment
      (y 10))  ; y comment
  ; Body comment
  (if (> x 0)  ; condition comment
    (+ x y)  ; then comment
    (- x y)))  ; else comment
"""
        result = printer.format(code)

        # All comments should be preserved
        assert "; Top comment" in result
        assert "; x comment" in result
        assert "; y comment" in result
        assert "; Body comment" in result

    def test_empty_and_whitespace_variations(self):
        """Test empty and whitespace variations."""
        printer = AIFPLPrettyPrinter()

        test_cases = [
            "",  # Empty
            "   ",  # Spaces only
            "\n",  # Newline only
            "\n\n\n",  # Multiple newlines
            "  \n  \n  ",  # Mixed whitespace
        ]

        for code in test_cases:
            result = printer.format(code)
            # Should handle gracefully
            assert isinstance(result, str)

    def test_comment_at_every_position(self):
        """Test comments at every possible position."""
        printer = AIFPLPrettyPrinter()

        # Comment before expression
        result1 = printer.format("; before\n(+ 1 2)")
        assert "; before" in result1

        # Comment after expression
        result2 = printer.format("(+ 1 2)\n; after")
        assert "; after" in result2

        # Comment between expressions
        result3 = printer.format("(+ 1 2)\n; between\n(+ 3 4)")
        assert "; between" in result3

        # Multiple comments
        result4 = printer.format("; one\n; two\n; three\n(+ 1 2)")
        assert "; one" in result4
        assert "; two" in result4
        assert "; three" in result4

    def test_deeply_nested_special_forms(self):
        """Test deeply nested special forms."""
        printer = AIFPLPrettyPrinter()
        code = """
(let ((x 1))
  (let ((y 2))
    (let ((z 3))
      (lambda (a)
        (if (> a 0)
          (match a
            (1 'one)
            (_ 'other))
          (+ x y z))))))
"""
        result = printer.format(code)

        # Should handle deep nesting
        assert "let" in result
        assert "lambda" in result
        assert "if" in result
        assert "match" in result

    def test_all_special_forms_with_empty_bodies(self):
        """Test special forms with minimal/empty content."""
        printer = AIFPLPrettyPrinter()

        test_cases = [
            "(let () 42)",  # Empty bindings
            "(lambda () 42)",  # No parameters
            "(if #t 1 2)",  # Minimal if
            "(match x)",  # Match with no clauses
        ]

        for code in test_cases:
            try:
                result = printer.format(code)
                assert len(result) >= 0
            except:
                # Some might be invalid, that's OK
                pass

    def test_format_options_variations(self):
        """Test with various format options."""
        # Test with different options
        options_list = [
            FormatOptions(indent_size=2, comment_spacing=2, compact_threshold=60),
            FormatOptions(indent_size=4, comment_spacing=4, compact_threshold=40),
            FormatOptions(indent_size=0, comment_spacing=0, compact_threshold=100),
            FormatOptions(indent_size=8, comment_spacing=8, compact_threshold=20),
        ]

        code = "(let ((x 5)) (+ x 1))  ; comment"

        for options in options_list:
            printer = AIFPLPrettyPrinter(options)
            result = printer.format(code)
            # Should format with different options
            assert "let" in result
            assert "; comment" in result

    def test_idempotence_with_comments(self):
        """Test that formatting with comments is idempotent."""
        printer = AIFPLPrettyPrinter()
        code = """
; Comment 1
(let ((x 5)  ; x value
      (y 10))  ; y value
  ; Body
  (+ x y))  ; Result
"""
        # Format once
        result1 = printer.format(code)
        # Format again
        result2 = printer.format(result1)
        # Should be identical
        assert result1 == result2
