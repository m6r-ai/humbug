"""Test for let form with EOL comment after opening paren."""
import sys
sys.path.insert(0, 'src')

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter

def test_let_eol_comment_after_open_paren():
    """Test that bindings are indented after EOL comment on opening paren."""
    printer = AIFPLPrettyPrinter()
    code = "(letrec (  ; Comment after opening paren\n  (foo (lambda (x) x)))\n  (foo 5))"
    result = printer.format(code)
    print("Result:")
    print(result)
    print("\nLines:")
    lines = result.split('\n')
    for i, line in enumerate(lines):
        print(f"{i}: {repr(line)}")
    
    # First binding should be indented
    assert lines[1].startswith('         '), f"Line 1 should start with 9 spaces, got: {repr(lines[1])}"
    assert '(foo' in lines[1]

if __name__ == '__main__':
    test_let_eol_comment_after_open_paren()
