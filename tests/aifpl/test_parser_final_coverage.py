"""Final test to achieve 100% coverage - nested let with related_symbol in stack."""

import pytest
from aifpl import AIFPLTokenizer, AIFPLParser, AIFPLParseError


def test_nested_let_with_related_symbol_in_incomplete_bindings_stack():
    """
    Test nested let where incomplete bindings error shows related_symbol from outer binding.

    This covers line 523 in _create_incomplete_bindings_error where it adds
    the related_symbol to the stack trace line.
    """
    tokenizer = AIFPLTokenizer()
    # Outer let with binding 'x', inner let with binding 'y' that's incomplete
    code = "(let ((x (let ((y 5"
    tokens = tokenizer.tokenize(code)
    parser = AIFPLParser(tokens, code)

    with pytest.raises(AIFPLParseError) as exc_info:
        parser.parse()

    error = exc_info.value
    # Should show both 'x' and 'y' in the context
    # The 'x' binding frame should show its related_symbol in the stack trace
    assert "'x'" in error.context
    assert "'y'" in error.context
