"""Test for preserving blank lines between comments."""

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter


def test_preserve_single_blank_line_between_comments():
    """Test that a single blank line between comments is preserved."""
    printer = AIFPLPrettyPrinter()
    code = """(letrec (
  ; Comment 1
  
  ; Comment 2
  (foo (lambda (x) x)))
  (foo 5))"""
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # Find the two comment lines
    comment1_idx = next(i for i, line in enumerate(lines) if '; Comment 1' in line)
    comment2_idx = next(i for i, line in enumerate(lines) if '; Comment 2' in line)
    
    # There should be exactly one blank line between them
    assert comment2_idx - comment1_idx == 2, f"Expected 2 lines between comments (1 blank), got {comment2_idx - comment1_idx}"
    assert lines[comment1_idx + 1] == '', "Expected blank line after Comment 1"


def test_reduce_multiple_blank_lines_to_one():
    """Test that multiple blank lines between comments are reduced to one."""
    printer = AIFPLPrettyPrinter()
    code = """(letrec (
  ; Comment 1


  ; Comment 2
  (foo (lambda (x) x)))
  (foo 5))"""
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # Find the two comment lines
    comment1_idx = next(i for i, line in enumerate(lines) if '; Comment 1' in line)
    comment2_idx = next(i for i, line in enumerate(lines) if '; Comment 2' in line)
    
    # Multiple blank lines should be reduced to one
    assert comment2_idx - comment1_idx == 2, f"Expected 2 lines between comments (1 blank), got {comment2_idx - comment1_idx}"
    assert lines[comment1_idx + 1] == '', "Expected blank line after Comment 1"


def test_no_blank_line_added_when_comments_adjacent():
    """Test that no blank line is added when comments are adjacent in source."""
    printer = AIFPLPrettyPrinter()
    code = """(letrec (
  ; Comment 1
  ; Comment 2
  (foo (lambda (x) x)))
  (foo 5))"""
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # Find the two comment lines
    comment1_idx = next(i for i, line in enumerate(lines) if '; Comment 1' in line)
    comment2_idx = next(i for i, line in enumerate(lines) if '; Comment 2' in line)
    
    # Comments should be adjacent (no blank line)
    assert comment2_idx - comment1_idx == 1, f"Expected comments to be adjacent, got {comment2_idx - comment1_idx} lines apart"


def test_blank_lines_in_comment_blocks():
    """Test blank lines in multi-line comment blocks."""
    printer = AIFPLPrettyPrinter()
    code = """(letrec (
  ; ============================================================================
  ; SECTION HEADER
  ; ============================================================================
  
  ; Function description
  (foo (lambda (x) x)))
  (foo 5))"""
    result = printer.format(code)
    
    lines = result.split('\n')
    
    # Find all lines with equals signs (header lines)
    header_lines = [i for i, line in enumerate(lines) if '=' in line]
    # Last header line
    header_end_idx = header_lines[-1] if header_lines else -1
    
    # Find the description line
    description_idx = next(i for i, line in enumerate(lines) if '; Function description' in line)
    
    # Should have a blank line between the header block and the description
    assert description_idx - header_end_idx == 2, f"Expected blank line between comment blocks, got {description_idx - header_end_idx - 1} blank lines"
    assert lines[header_end_idx + 1] == '', "Expected blank line after header"
