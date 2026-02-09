"""Test for let form with EOL comment after opening paren."""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


def test_let_eol_comment_after_open_paren():
    """Test that bindings are indented after EOL comment on opening paren."""
    printer = AIFPLPrettyPrinter()
    code = "(letrec (  ; Comment after opening paren\n  (foo (lambda (x) x)))\n  (foo 5))"
    result = printer.format(code)

    lines = result.split('\n')

    # With new formatting, letrec is on line 0, bindings paren with comment on line 1
    assert lines[0] == '(letrec'
    assert '; Comment after opening paren' in lines[1]

    # First binding should be on line 2 and indented (3 spaces)
    assert lines[2].startswith('   '), f"Expected 3 spaces, got: {repr(lines[2])}"
    assert '(foo' in lines[2]

    # Should not have blank line between comment and binding
    assert lines[2] != ''
