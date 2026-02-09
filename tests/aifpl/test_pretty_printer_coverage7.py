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
    
    # First line should be just the letrec opening
    assert lines[0] == '(letrec ('
    
    # Comment should be on its own line (line 1), indented to binding position
    assert '; This is a comment' in lines[1]
    assert lines[1].startswith('         '), f"Comment should be indented 9 spaces, got: {repr(lines[1])}"
    
    # Binding should follow on next line
    assert '(foo' in lines[2]
    assert lines[2].startswith('         ')


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
    
    # First line should be just the let opening
    assert lines[0] == '(let ('
    
    # Both comments should be on their own lines, indented
    assert '; Comment 1' in lines[1]
    assert lines[1].startswith('      ')  # 6 spaces for 'let'
    assert '; Comment 2' in lines[2]
    assert lines[2].startswith('      ')
    
    # Binding should follow
    assert '(x 1)' in lines[3]


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
    
    # First line should NOT have a comment on it
    assert lines[0] == '(letrec ('
    assert ';' not in lines[0]
    
    # All three comment lines should be standalone
    comment_lines = [i for i, line in enumerate(lines) if '; =' in line or '; SECTION' in line]
    assert len(comment_lines) == 3
    
    # Each comment line should be properly indented and not have other content
    for i in comment_lines:
        assert lines[i].strip().startswith(';')
        assert not lines[i].strip().startswith('(')
