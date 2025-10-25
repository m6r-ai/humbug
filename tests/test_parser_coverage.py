"""Tests to achieve 100% coverage of parser error handling."""

import pytest
from aifpl import AIFPLTokenizer, AIFPLParser, AIFPLParseError


class TestUnterminatedLetExpressions:
    """Test unterminated let expressions at various points."""

    def test_unterminated_let_after_keyword(self):
        """Test EOF immediately after 'let' keyword."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let")
        parser = AIFPLParser(tokens, "(let")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        assert "Unterminated list" in error.message
        assert "missing 1 closing parenthesis" in error.message
        assert "let binding" in error.context

    def test_unterminated_let_with_variable_name(self):
        """Test unterminated let binding with variable name (triggers related_symbol)."""
        tokenizer = AIFPLTokenizer()
        # EOF happens inside the binding, so we get unterminated error
        tokens = tokenizer.tokenize("(let ((x 5) (y")
        parser = AIFPLParser(tokens, "(let ((x 5) (y")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # This triggers unterminated error, not incomplete bindings
        assert "Unterminated list" in error.message
        # Should show the 'y' variable in the error context
        assert "'y'" in error.context

    def test_unterminated_let_inside_single_binding(self):
        """Test EOF while parsing a single binding."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let ((x")
        parser = AIFPLParser(tokens, "(let ((x")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should trigger the enhanced unterminated error
        assert "Unterminated list" in error.message
        assert "'x'" in error.context

    def test_unterminated_let_with_multiple_bindings(self):
        """Test unterminated let with multiple bindings showing related_symbol."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let ((x 5) (y 10) (z")
        parser = AIFPLParser(tokens, "(let ((x 5) (y 10) (z")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should show 'z' in the related_symbol field
        assert "'z'" in error.context


class TestInvalidBindingStructures:
    """Test invalid binding structures in let expressions."""

    def test_binding_not_a_list(self):
        """Test when a binding is not a list structure."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let (x")
        parser = AIFPLParser(tokens, "(let (x")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should show that 'x' is not a proper binding
        assert "Incomplete let bindings" in error.message
        assert "<not a list>" in error.context

    def test_binding_is_number(self):
        """Test when a binding is a number instead of a list."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let (42")
        parser = AIFPLParser(tokens, "(let (42")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        assert "Incomplete let bindings" in error.message
        assert "<not a list>" in error.context

    def test_binding_invalid_structure(self):
        """Test binding with invalid structure (not starting with symbol)."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let ((42 5)")
        parser = AIFPLParser(tokens, "(let ((42 5)")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # The binding (42 5) is parsed but should show as invalid
        assert "Incomplete let bindings" in error.message

    def test_binding_with_non_symbol_first_element(self):
        """Test binding where first element is not a symbol."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize('(let (("string" 5)')
        parser = AIFPLParser(tokens, '(let (("string" 5)')

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        assert "Incomplete let bindings" in error.message
        # Should show as invalid binding since first element is not a symbol
        assert "<invalid binding>" in error.context


class TestRelatedSymbolDisplay:
    """Test that related_symbol is properly displayed in error messages."""

    def test_related_symbol_in_unterminated_error(self):
        """Test that related_symbol appears in unterminated list errors."""
        tokenizer = AIFPLTokenizer()
        # Create a deeply nested structure where a binding is unterminated
        tokens = tokenizer.tokenize("(let ((myvar (+ 1 2")
        parser = AIFPLParser(tokens, "(let ((myvar (+ 1 2")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should show 'myvar' in the stack trace
        assert "'myvar'" in error.context

    def test_related_symbol_in_incomplete_bindings_error(self):
        """Test that related_symbol appears in incomplete bindings errors."""
        tokenizer = AIFPLTokenizer()
        # EOF after complete binding but before closing bindings list
        tokens = tokenizer.tokenize("(let ((alpha 1) (beta 2")
        parser = AIFPLParser(tokens, "(let ((alpha 1) (beta 2")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # This should show 'beta' in the error context (unterminated binding)
        assert "'beta'" in error.context

    def test_related_symbol_in_incomplete_bindings_error_stack(self):
        """Test that related_symbol appears in the stack trace of incomplete bindings errors."""
        tokenizer = AIFPLTokenizer()
        # This creates a situation where we have a complete binding with a symbol,
        # then an incomplete binding list (EOF in bindings list, not inside a binding)
        tokens = tokenizer.tokenize("(let ((x 5")
        parser = AIFPLParser(tokens, "(let ((x 5")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # This should show 'x' in the context
        assert "'x'" in error.context


class TestComplexNestedStructures:
    """Test complex nested structures to ensure all code paths are covered."""

    def test_nested_let_with_unterminated_binding(self):
        """Test nested let with unterminated binding."""
        tokenizer = AIFPLTokenizer()
        code = "(let ((x 5) (y (let ((z 10"
        tokens = tokenizer.tokenize(code)
        parser = AIFPLParser(tokens, code)

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should show nested structure in error
        assert "let binding" in error.context

    def test_let_with_empty_binding_list(self):
        """Test let with empty binding followed by EOF."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let (")
        parser = AIFPLParser(tokens, "(let (")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        assert "Incomplete let bindings" in error.message
        assert "(no complete bindings)" in error.context

    def test_multiple_invalid_bindings(self):
        """Test multiple invalid bindings to cover all error formatting."""
        tokenizer = AIFPLTokenizer()
        # Complete the bindings but EOF before body
        tokens = tokenizer.tokenize("(let (x 42")
        parser = AIFPLParser(tokens, "(let (x 42")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should show invalid bindings
        assert "Incomplete let bindings" in error.message
        assert "<not a list>" in error.context


class TestEdgeCases:
    """Test edge cases for complete coverage."""

    def test_single_binding_with_eof_after_variable(self):
        """Test EOF right after variable name in binding."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let ((x")
        parser = AIFPLParser(tokens, "(let ((x")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should trigger unterminated error for the binding
        assert "'x'" in error.context

    def test_binding_with_only_variable_no_value(self):
        """Test binding with only variable, no value, then EOF."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let ((myvar")
        parser = AIFPLParser(tokens, "(let ((myvar")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        assert "'myvar'" in error.context

    def test_let_eof_after_keyword_only(self):
        """Test EOF immediately after 'let' with no space."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let")
        parser = AIFPLParser(tokens, "(let")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        assert "Unterminated list" in error.message
        assert "let binding" in error.context


class TestBindingSummaryFormatting:
    """Test the binding summary formatting in error messages."""

    def test_binding_summary_with_valid_and_invalid(self):
        """Test binding summary shows both valid and invalid bindings."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let ((x 5) (y 10) z")
        parser = AIFPLParser(tokens, "(let ((x 5) (y 10) z")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should show x and y as valid (✓), z as invalid
        assert "Incomplete let bindings" in error.message
        # The summary should list the bindings
        assert "Bindings parsed:" in error.context

    def test_binding_summary_with_wrong_element_count(self):
        """Test binding with wrong number of elements."""
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize("(let ((x 5 6 7)")
        parser = AIFPLParser(tokens, "(let ((x 5 6 7)")

        with pytest.raises(AIFPLParseError) as exc_info:
            parser.parse()

        error = exc_info.value
        # Should show binding as invalid due to wrong count
        assert "Incomplete let bindings" in error.message
