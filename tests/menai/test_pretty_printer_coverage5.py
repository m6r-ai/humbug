"""Test for preserving blank lines between code and comments."""

from menai.menai_pretty_printer import MenaiPrettyPrinter


def test_preserve_blank_line_between_code_and_comment():
    """Test that a blank line between code and a comment is preserved."""
    printer = MenaiPrettyPrinter()
    code = """(letrec (
  (foo (lambda (x) x))

  ; Comment after blank line
  (bar (lambda (y) y)))
  (foo 5))"""
    result = printer.format(code)

    lines = result.split('\n')

    # Find the end of first binding (the line with closing parens)
    foo_end_idx = next(i for i, line in enumerate(lines) if 'x))' in line)
    comment_idx = next(i for i, line in enumerate(lines) if '; Comment after blank line' in line)

    # Should have a blank line between them (difference of 2 means 1 blank line)
    assert comment_idx - foo_end_idx == 2, f"Expected 1 blank line between code and comment, got {comment_idx - foo_end_idx - 1}"


def test_no_extra_blank_after_eol_comment():
    """Test that EOL comments don't create extra blank lines."""
    printer = MenaiPrettyPrinter()
    code = "(letrec (  ; Comment after opening paren\n  (foo (lambda (x) x)))\n  (foo 5))"
    result = printer.format(code)

    lines = result.split('\n')

    # With new formatting, letrec is on line 0, bindings opening paren with comment on line 1
    assert lines[0] == '(letrec'
    assert '; Comment after opening paren' in lines[1]

    # First binding should be on next line (no blank line in between)
    assert '(foo' in lines[2], f"Expected binding on line 2, got: {repr(lines[2])}"
    assert lines[2].startswith('   '), "Binding should be indented (3 spaces)"


def test_blank_line_between_binding_and_comment_then_binding():
    """Test blank line between binding, comment, and next binding."""
    printer = MenaiPrettyPrinter()
    code = """(let ((x 1)

            ; Middle comment
            (y 2))
  (+ x y))"""
    result = printer.format(code)

    lines = result.split('\n')

    # Find the comment
    comment_idx = next(i for i, line in enumerate(lines) if '; Middle comment' in line)

    # Should have blank line before comment
    assert lines[comment_idx - 1] == '', "Expected blank line before comment"

    # Should NOT have blank line after comment (binding should be next)
    assert '(y' in lines[comment_idx + 1], "Expected binding right after comment"


def test_multiple_blank_lines_reduced_to_one_between_code_and_comment():
    """Test that multiple blank lines between code and comment are reduced to one."""
    printer = MenaiPrettyPrinter()
    code = """(letrec (
  (foo (lambda (x) x))
  (blah (lambda (z) z)))


  ; Comment after multiple blank lines
  (bar (lambda (y) y)))
  (foo 5))"""
    result = printer.format(code)

    lines = result.split('\n')

    # Find the end of first binding and the comment
    foo_end_idx = next(i for i, line in enumerate(lines) if 'x))' in line)
    comment_idx = next(i for i, line in enumerate(lines) if '; Comment after multiple blank lines' in line)

    # Should have exactly one blank line between them (boolean-not multiple)
    assert comment_idx - foo_end_idx == 2, f"Expected 1 blank line, got {comment_idx - foo_end_idx - 1}"
    assert lines[foo_end_idx + 1] == '', "Expected blank line after first binding"


def test_blank_line_always_added_before_comment():
    """Test that blank line is always added before comment (canonical formatting)."""
    printer = MenaiPrettyPrinter()
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

    # Canonical formatting: always add blank line before comment (except first)
    assert comment_idx - foo_end_idx == 2, f"Expected 1 blank line, got {comment_idx - foo_end_idx - 1} blank lines"
    assert lines[foo_end_idx + 1] == '', "Expected blank line before comment"
