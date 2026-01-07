"""Test to cover the empty binding case in parser."""

from aifpl import AIFPLTokenizer, AIFPLParser


def test_empty_binding_in_let():
    """
    Test that empty binding () is parsed correctly.

    This covers the case where the elif condition on line 455 is False,
    because current_token.type == RPAREN (empty binding).
    """
    tokenizer = AIFPLTokenizer()
    tokens = tokenizer.tokenize("(let (() 5))")
    parser = AIFPLParser(tokens, "(let (() 5))")

    # Should parse successfully (evaluator will complain about invalid binding)
    result = parser.parse()

    # Verify structure: (let (() 5))
    assert result.length() == 2
    assert result.get(0).name == "let"

    # The bindings list should contain one empty binding
    bindings = result.get(1)
    assert bindings.length() == 2
    assert bindings.get(0).length() == 0  # Empty binding


def test_multiple_bindings_with_empty():
    """Test multiple bindings including an empty one."""
    tokenizer = AIFPLTokenizer()
    tokens = tokenizer.tokenize("(let ((x 5) () (y 10)) 42)")
    parser = AIFPLParser(tokens, "(let ((x 5) () (y 10)) 42)")

    result = parser.parse()

    # Verify structure
    bindings = result.get(1)
    assert bindings.length() == 3
    assert bindings.get(0).length() == 2  # (x 5)
    assert bindings.get(1).length() == 0  # ()
    assert bindings.get(2).length() == 2  # (y 10)
