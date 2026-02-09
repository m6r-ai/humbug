"""Test for standalone comments appearing on their own line."""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


def test_standalone_comment_after_binding_list_open_paren():
    """Test that standalone comment after binding list opening paren is on its own line."""
    printer = AIFPLPrettyPrinter()
    code = """(letrec (
  ; This is a comment
  (foo (lambda (x) x)))
  (foo 5))"""
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # With new formatting, letrec is on line 0, bindings opening paren on line 1
    assert lines[0] == '(letrec'
    assert lines[1] == '  ('
    
    # Comment should be on its own line (line 2), indented to binding position
    assert '; This is a comment' in lines[2]
    assert lines[2].startswith('   '), f"Comment should be indented 3 spaces, got: {repr(lines[2])}"
    
    # Binding should follow on next line
    assert '(foo' in lines[3]
    assert lines[3].startswith('   ')


def test_multiple_standalone_comments_before_bindings():
    """Test multiple standalone comments before first binding."""
    printer = AIFPLPrettyPrinter()
    code = """(let (
  ; Comment 1
  ; Comment 2
  (x 1))
  x)"""
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # With new formatting, let is on line 0, bindings opening paren on line 1
    assert lines[0] == '(let'
    assert lines[1] == '  ('
    
    # Both comments should be on their own lines, indented
    assert '; Comment 1' in lines[2]
    assert lines[2].startswith('   ')  # 3 spaces (indent after bindings paren)
    assert '; Comment 2' in lines[3]
    assert lines[3].startswith('   ')
    
    # Binding should follow
    assert '(x 1)' in lines[4]


def test_standalone_comment_not_moved_to_previous_line():
    """Test that standalone comments are never moved to end of previous line."""
    printer = AIFPLPrettyPrinter()
    code = """(letrec (
  ; ============================================================================
  ; SECTION HEADER
  ; ============================================================================
  (func (lambda () 42)))
  (func))"""
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # With new formatting, first line is just letrec, second line is bindings paren
    assert lines[0] == '(letrec'
    assert lines[1] == '  ('
    assert ';' not in lines[0]
    
    # All three comment lines should be standalone
    comment_lines = [i for i, line in enumerate(lines) if '; =' in line or '; SECTION' in line]
    assert len(comment_lines) == 3
    
    # Each comment line should be properly indented and not have other content
    for i in comment_lines:
        assert lines[i].strip().startswith(';')
        assert not lines[i].strip().startswith('(')
