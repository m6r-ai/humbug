"""Test for let form with EOL comment after opening paren."""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


def test_let_eol_comment_after_open_paren():
    """Test that bindings are indented after EOL comment on opening paren."""
    printer = AIFPLPrettyPrinter()
    code = "(letrec (  ; Comment after opening paren\n  (foo (lambda (x) x)))\n  (foo 5))"
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # EOL comment should be on first line
    assert '; Comment after opening paren' in lines[0]
    
    # First binding should be on next line and indented
    assert lines[1].startswith('         '), f"Expected 9 spaces, got: {repr(lines[1])}"
    assert '(foo' in lines[1]
    
    # Should not have blank line between comment and binding
    assert lines[1] != ''
