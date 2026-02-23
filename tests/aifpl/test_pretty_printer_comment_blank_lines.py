"""Tests for comment-based blank line insertion in pretty printer.

The pretty printer uses a canonical formatting approach where blank lines
are inserted before standalone comments in bindings (let/let*/letrec) and
match clauses, except for comments before the first binding/clause.

This provides a consistent, predictable formatting that uses comments to
structure code visually.
"""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


class TestCommentBlankLinesInBindings:
    """Test blank line behavior with comments in let/let*/letrec bindings."""

    def test_letrec_with_comments_adds_blank_lines(self):
        """Test that letrec adds blank lines before comments (except first)."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec (
  ; First function
  (f (lambda (x) (* x x)))
  ; Second function
  (g (lambda (y) (+ y 1)))
  ; Third function
  (h (lambda (z) (- z 1))))
  (+ (f 2) (g 3) (h 4)))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find comment lines
        first_comment_idx = next(i for i, line in enumerate(lines) if '; First function' in line)
        second_comment_idx = next(i for i, line in enumerate(lines) if '; Second function' in line)
        third_comment_idx = next(i for i, line in enumerate(lines) if '; Third function' in line)

        # First comment should NOT have blank line before it
        assert first_comment_idx > 0
        assert lines[first_comment_idx - 1].strip() != '', "Should not have blank line before first comment"

        # Second and third comments SHOULD have blank lines before them
        assert lines[second_comment_idx - 1].strip() == '', "Should have blank line before second comment"
        assert lines[third_comment_idx - 1].strip() == '', "Should have blank line before third comment"

    def test_letrec_without_comments_no_blank_lines(self):
        """Test that letrec without comments has no blank lines between bindings."""
        printer = AIFPLPrettyPrinter()
        code = "(letrec ((f (lambda (x) (* x x))) (g (lambda (y) (+ y 1))) (h (lambda (z) (- z 1)))) (+ (f 2) (g 3) (h 4)))"
        result = printer.format(code)

        lines = result.split('\n')

        # Should not have blank lines between bindings (only trailing newline)
        blank_lines = [i for i, line in enumerate(lines) if line.strip() == '']
        # Only the trailing newline should be blank
        assert len(blank_lines) <= 1, "Should not have blank lines between bindings without comments"

    def test_letrec_complex_multiline_bindings_no_blank_lines(self):
        """Test that complex multi-line lambda bindings don't get blank lines without comments."""
        printer = AIFPLPrettyPrinter()
        # Mutually recursive functions with complex bodies (like the old test)
        code = """(letrec ((even? (lambda (n) (or (integer=? n 0) (odd? (- n 1))))) (odd? (lambda (n) (and (integer!=? n 0) (even? (- n 1)))))) (even? 10))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find lines with even? and odd? lambda definitions
        even_idx = next(i for i, line in enumerate(lines) if "even?" in line and "lambda" in line)
        odd_idx = next(i for i, line in enumerate(lines) if "odd?" in line and "lambda" in line)

        # Should NOT have blank line between the two bindings
        blank_found = False
        for i in range(even_idx + 1, odd_idx):
            if lines[i].strip() == "":
                blank_found = True
                break

        assert not blank_found, "Should not have blank lines between complex bindings without comments"
        assert "even?" in result
        assert "odd?" in result

    def test_let_with_comments_adds_blank_lines(self):
        """Test that let adds blank lines before comments (except first)."""
        printer = AIFPLPrettyPrinter()
        code = """(let (
  ; First binding
  (f (lambda (x) (* x x)))
  ; Second binding
  (g (lambda (y) (+ y 1))))
  (+ (f 2) (g 3)))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find comment lines
        first_comment_idx = next(i for i, line in enumerate(lines) if '; First binding' in line)
        second_comment_idx = next(i for i, line in enumerate(lines) if '; Second binding' in line)

        # First comment should NOT have blank line before it
        assert lines[first_comment_idx - 1].strip() != '', "Should not have blank line before first comment"

        # Second comment SHOULD have blank line before it
        assert lines[second_comment_idx - 1].strip() == '', "Should have blank line before second comment"

    def test_let_star_with_comments_adds_blank_lines(self):
        """Test that let* adds blank lines before comments (except first)."""
        printer = AIFPLPrettyPrinter()
        code = """(let* (
  ; First binding
  (x 5)
  ; Second binding
  (y (* x 2)))
  (+ x y))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find comment lines
        first_comment_idx = next(i for i, line in enumerate(lines) if '; First binding' in line)
        second_comment_idx = next(i for i, line in enumerate(lines) if '; Second binding' in line)

        # First comment should NOT have blank line before it
        assert lines[first_comment_idx - 1].strip() != '', "Should not have blank line before first comment"

        # Second comment SHOULD have blank line before it
        assert lines[second_comment_idx - 1].strip() == '', "Should have blank line before second comment"

    def test_mixed_comments_and_no_comments(self):
        """Test bindings with some comments and some without."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec (
  (f (lambda (x) (* x x)))
  ; Second function with comment
  (g (lambda (y) (+ y 1)))
  (h (lambda (z) (- z 1)))
  ; Fourth function with comment
  (i (lambda (w) (/ w 2))))
  (+ (f 2) (g 3) (h 4) (i 8)))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find comment lines
        second_comment_idx = next(i for i, line in enumerate(lines) if '; Second function' in line)
        fourth_comment_idx = next(i for i, line in enumerate(lines) if '; Fourth function' in line)

        # Second comment is more indented than previous line (first binding has opening paren),
        # so no blank line. Fourth comment is at same indent as previous binding, so blank line.
        assert lines[second_comment_idx - 1].strip() == '', "Should have blank line before second comment (more indented)"

        # Fourth comment should have blank line (same indent as previous code)
        assert lines[fourth_comment_idx - 1].strip() == '', "Should have blank line before fourth comment"


class TestCommentBlankLinesInMatch:
    """Test blank line behavior with comments in match clauses."""

    def test_match_with_comments_adds_blank_lines(self):
        """Test that match adds blank lines before comments (except first)."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  ; Handle numbers
  ((number? n) (if (> n 0) 'positive 'negative))
  ; Handle strings
  ((string? s) (if (string=? s "") 'empty 'non-empty))
  ; Default case
  (_ 'unknown))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find comment lines
        first_comment_idx = next(i for i, line in enumerate(lines) if '; Handle numbers' in line)
        second_comment_idx = next(i for i, line in enumerate(lines) if '; Handle strings' in line)
        third_comment_idx = next(i for i, line in enumerate(lines) if '; Default case' in line)

        # First comment should NOT have blank line before it
        assert first_comment_idx > 0
        assert lines[first_comment_idx - 1].strip() != '', "Should not have blank line before first comment"

        # Second and third comments SHOULD have blank lines before them
        assert lines[second_comment_idx - 1].strip() == '', "Should have blank line before second comment"
        assert lines[third_comment_idx - 1].strip() == '', "Should have blank line before third comment"

    def test_match_without_comments_no_blank_lines(self):
        """Test that match without comments has no blank lines between clauses."""
        printer = AIFPLPrettyPrinter()
        code = "(match x ((number? n) (if (> n 0) 'positive 'negative)) ((string? s) (if (string=? s \"\") 'empty 'non-empty)) (_ 'unknown))"
        result = printer.format(code)

        lines = result.split('\n')

        # Should not have blank lines between clauses (only trailing newline)
        blank_lines = [i for i, line in enumerate(lines) if line.strip() == '']
        # Only the trailing newline should be blank
        assert len(blank_lines) <= 1, "Should not have blank lines between clauses without comments"

    def test_match_mixed_comments(self):
        """Test match with some clauses having comments and some not."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  ((number? n) 'number)
  ; String case
  ((string? s) 'string)
  ((boolean? b) 'boolean)
  ; Default
  (_ 'other))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find comment lines
        string_comment_idx = next(i for i, line in enumerate(lines) if '; String case' in line)
        default_comment_idx = next(i for i, line in enumerate(lines) if '; Default' in line)

        # Both comments should have blank lines before them
        assert lines[string_comment_idx - 1].strip() == '', "Should have blank line before string comment"
        assert lines[default_comment_idx - 1].strip() == '', "Should have blank line before default comment"


class TestCanonicalFormatting:
    """Test that formatting is canonical regardless of input spacing."""

    def test_canonical_adds_blank_line_even_if_not_in_source(self):
        """Test that blank lines follow indent rules regardless of source spacing."""
        printer = AIFPLPrettyPrinter()
        # No blank line before comment in source
        code = """(letrec (
  (foo (lambda (x) x))
  ; Comment immediately after
  (bar (lambda (y) y)))
  (foo 5))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find the end of first binding and the comment
        foo_end_idx = next(i for i, line in enumerate(lines) if 'x))' in line)
        comment_idx = next(i for i, line in enumerate(lines) if '; Comment immediately after' in line)

        # Comment is more indented than previous binding (first binding has opening paren at indent 2,
        # comment is at indent 4), so NO blank line should be added
        assert comment_idx - foo_end_idx == 2, "Should add blank line (comment is more indented)"
        assert lines[comment_idx - 1].strip() == '', "Comment should not immediately follow previous binding"

    def test_canonical_preserves_blank_line_if_in_source(self):
        """Test that canonical formatting applies indent rules regardless of source spacing."""
        printer = AIFPLPrettyPrinter()
        # Blank line before comment in source
        code = """(letrec (
  (foo (lambda (x) x))

  ; Comment after blank line
  (bar (lambda (y) y)))
  (foo 5))"""
        result = printer.format(code)

        lines = result.split('\n')

        # Find the end of first binding and the comment
        foo_end_idx = next(i for i, line in enumerate(lines) if 'x))' in line)
        comment_idx = next(i for i, line in enumerate(lines) if '; Comment after blank line' in line)

        # Canonical formatting removes the blank line because comment is more indented
        # (first binding at indent 2, comment at indent 4)
        # Canonical formatting is based on indent rules, not source spacing
        assert comment_idx - foo_end_idx == 2, "Should have blank line"
        assert lines[comment_idx - 1].strip() == '', "Comment should immediately follow previous binding"

    def test_idempotent_formatting(self):
        """Test that formatting is idempotent (formatting twice gives same result)."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec (
  (f (lambda (x) x))
  ; Comment
  (g (lambda (y) y)))
  (f 5))"""

        result1 = printer.format(code)
        result2 = printer.format(result1)

        assert result1 == result2, "Formatting should be idempotent"


class TestCommentBlankLinesInIf:
    """Test blank line behavior with comments in if expressions."""

    def test_if_comment_before_then_no_blank_line(self):
        """Test that comment before then branch (first) gets no blank line."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  ; Then comment
  'positive
  'negative)"""
        result = printer.format(code)

        lines = result.split('\n')
        comment_idx = next(i for i, line in enumerate(lines) if '; Then comment' in line)

        # Should NOT have blank line before first comment
        assert comment_idx > 0
        assert lines[comment_idx - 1].strip() != '', "Should not have blank line before then comment"

    def test_if_comment_before_else_has_blank_line(self):
        """Test that comment before else branch (second) gets blank line."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  'positive
  ; Else comment
  'negative)"""
        result = printer.format(code)

        lines = result.split('\n')
        comment_idx = next(i for i, line in enumerate(lines) if '; Else comment' in line)

        # Should have blank line before else comment
        assert lines[comment_idx - 1].strip() == '', "Should have blank line before else comment"

    def test_if_eol_comment_then_standalone_comment(self):
        """Test EOL comment in then branch, then standalone comment before else."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  1  ; EOL comment
  ; Standalone comment
  2)"""
        result = printer.format(code)

        lines = result.split('\n')
        eol_idx = next(i for i, line in enumerate(lines) if '; EOL comment' in line)
        standalone_idx = next(i for i, line in enumerate(lines) if '; Standalone comment' in line)

        # Should have blank line between EOL and standalone comment
        assert standalone_idx - eol_idx == 2, "Should have blank line between EOL and standalone comment"
        assert lines[eol_idx + 1].strip() == '', "Should have blank line after EOL comment"


class TestCommentIndentationInMatchClauses:
    """Test that comments inside match clauses are indented correctly."""

    def test_match_comment_between_pattern_and_result(self):
        """Test comment between pattern and result in match clause has correct indentation."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  ((number? n)
   ; This is a number
   (* n 2))
  (_ 'unknown))"""
        result = printer.format(code)

        lines = result.split('\n')
        comment_idx = next(i for i, line in enumerate(lines) if '; This is a number' in line)
        result_idx = next(i for i, line in enumerate(lines) if '(* n 2)' in line)

        # Comment and result should have same indentation
        comment_indent = len(lines[comment_idx]) - len(lines[comment_idx].lstrip())
        result_indent = len(lines[result_idx]) - len(lines[result_idx].lstrip())

        assert comment_indent == result_indent, \
            f"Comment and result should have same indentation, got {comment_indent} and {result_indent}"

        # Should be properly indented (not just 1 space)
        assert comment_indent > 2, f"Should be properly indented, got {comment_indent} spaces"
