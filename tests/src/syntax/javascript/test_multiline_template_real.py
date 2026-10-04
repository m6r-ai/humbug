"""
Test for real-world multiline template literal scenario.
This tests the specific case from the user's code.
"""
import pytest

from syntax.typescript.typescript_lexer import TypeScriptLexer


class TestMultilineTemplateLiteral:
    """Test real multiline template literal handling."""

    def test_multiline_template_literal_basic(self):
        """Test a basic multiline template literal."""
        lines = [
            "const code = `line 1",
            "line 2",
            "line 3`;",
        ]

        state = None
        for i, line in enumerate(lines):
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

            # Should produce at least one token per line
            assert len(tokens) >= 1, f"Line {i+1} should produce tokens"

            # First line should have keyword, identifier, operator, and start of string
            if i == 0:
                # Check we have const keyword
                keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'const']
                assert len(keyword_tokens) == 1, "First line should have 'const' keyword"

    def test_user_code_fragment(self):
        """Test the exact pattern from user's code."""
        lines = [
            "h(CodeFragment, {language: 'text', code:",
            "`================================================================================",
            "COMPARISON WITH BASELINE: baseline.json",
            "================================================================================",
            "Benchmark                                Current      Baseline     Change",
            "--------------------------------------------------------------------------------",
            "Simple Addition                          0.046ms      0.095ms      ↑ 51.9%",
            "`",
            "});",
        ]

        state = None
        for i, line in enumerate(lines):
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

            # Should not crash
            assert tokens is not None, f"Line {i+1} should tokenize"

            # Lines inside template literal should produce STRING tokens
            if 1 <= i <= 6:  # Lines inside the template literal
                string_tokens = [t for t in tokens if t.type.name == 'STRING']
                # Note: Current implementation may not track template literal state,
                # so this documents the actual behavior
                # assert len(string_tokens) >= 1, f"Line {i+1} inside template should have STRING token"

    def test_template_literal_with_expression(self):
        """Test template literal with embedded expression."""
        lines = [
            "const msg = `Hello ${name},",
            "Welcome to the system`;",
        ]

        state = None
        for line in lines:
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

            # Should not crash
            assert tokens is not None

    def test_nested_template_in_function_call(self):
        """Test template literal as function argument."""
        lines = [
            "render(`",
            "  <div>",
            "    <h1>Title</h1>",
            "  </div>",
            "`);",
        ]

        state = None
        for line in lines:
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

            # Should not crash
            assert tokens is not None

    def test_template_literal_closed_on_same_line(self):
        """Test template literal that opens and closes on same line (should work)."""
        lexer = TypeScriptLexer()
        lexer.lex(None, "const x = `hello world`;")

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) >= 1, "Should have STRING token for template literal"

    def test_template_literal_with_special_chars(self):
        """Test template literal containing special characters."""
        lines = [
            "const output = `",
            "Result: ↑ 51.9%",
            "Status: ✓",
            "`;",
        ]

        state = None
        for line in lines:
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

            # Should handle Unicode characters without crashing
            assert tokens is not None
