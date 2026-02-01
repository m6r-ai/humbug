"""Test to cover the empty binding case in parser."""

from aifpl import AIFPLLexer, AIFPLParser


def test_empty_binding_in_let():
    """
    Test that empty binding () is parsed correctly.

    This covers the case where the elif condition on line 455 is False,
    because current_token.type == RPAREN (empty binding).
    """
    lexer = AIFPLLexer()
    tokens = lexer.lex("(let (() 5))")
    parser = AIFPLParser()

    # Should parse successfully (evaluator will complain about invalid binding)
    result = parser.parse(tokens, "(let (() 5))")

    # Verify structure: (let (() 5))
    assert result.length() == 2
    assert result.get(0).name == "let"

    # The bindings list should contain one empty binding
    bindings = result.get(1)
    assert bindings.length() == 2
    assert bindings.get(0).length() == 0  # Empty binding


def test_multiple_bindings_with_empty():
    """Test multiple bindings including an empty one."""
    lexer = AIFPLLexer()
    tokens = lexer.lex("(let ((x 5) () (y 10)) 42)")
    parser = AIFPLParser()

    result = parser.parse(tokens, "(let ((x 5) () (y 10)) 42)")

    # Verify structure
    bindings = result.get(1)
    assert bindings.length() == 3
    assert bindings.get(0).length() == 2  # (x 5)
    assert bindings.get(1).length() == 0  # ()
    assert bindings.get(2).length() == 2  # (y 10)
