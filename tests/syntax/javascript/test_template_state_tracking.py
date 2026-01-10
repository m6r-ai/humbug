"""
Test to verify template literal state tracking (or lack thereof).
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestTemplateLiteralStateTracking:
    """Test whether template literals maintain state across lines."""

    def test_unclosed_template_literal_state(self):
        """Test if lexer tracks unclosed template literal state."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, "const x = `unclosed template")

        # Check if state has any template literal tracking
        print(f"\nState attributes: {dir(state)}")
        print(f"State dict: {vars(state)}")

        # The state should indicate we're in a template literal
        has_template_state = hasattr(state, 'in_template_literal')
        print(f"Has template literal state tracking: {has_template_state}")

        # Now we should have template literal state tracking
        assert has_template_state, "Should have template literal state tracking"
        assert state.in_template_literal == True, "Should be in template literal state"

    def test_multiline_template_tokenization(self):
        """Test how multiline template literals are actually tokenized."""
        lines = [
            "const code = `line 1",
            "line 2",
            "line 3`;",
        ]

        state = None
        all_tokens = []
        for i, line in enumerate(lines):
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            all_tokens.append((i+1, line, tokens))

            print(f"\nLine {i+1}: '{line}'")
            for token in tokens:
                print(f"  {token.type.name}: {repr(token.value[:50])}")

        # Line 1: Should have tokens up to and including the backtick
        line1_tokens = all_tokens[0][2]
        backtick_tokens = [t for t in line1_tokens if '`' in t.value]
        print(f"\nLine 1 backtick tokens: {len(backtick_tokens)}")

        # Line 2: This is inside the template, but without state tracking,
        # it will be tokenized as if it's standalone code
        line2_tokens = all_tokens[1][2]
        print(f"Line 2 tokens: {[t.type.name for t in line2_tokens]}")

        # Line 3: Starts with content, ends with backtick and semicolon
        line3_tokens = all_tokens[2][2]
        print(f"Line 3 tokens: {[t.type.name for t in line3_tokens]}")

        # Document the actual behavior
        # Without proper state tracking, each line is tokenized independently

    def test_compare_single_vs_multiline_template(self):
        """Compare single-line vs multi-line template literal tokenization."""
        # Single line (should work correctly)
        lexer1 = JavaScriptLexer()
        lexer1.lex(None, "const x = `hello world`;")
        single_line_tokens = list(lexer1._tokens)

        print("\nSingle-line template:")
        for token in single_line_tokens:
            print(f"  {token.type.name}: {repr(token.value)}")

        # Multi-line (may not work correctly)
        lines = ["const x = `hello", "world`;"]
        state = None
        multiline_tokens = []
        for line in lines:
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)
            multiline_tokens.extend(list(lexer._tokens))

        print("\nMulti-line template:")
        for token in multiline_tokens:
            print(f"  {token.type.name}: {repr(token.value)}")

        # The tokenization will likely be different
        # This test documents the difference

    def test_multiline_template_with_escapes(self):
        """Test multiline template literal with escape sequences."""
        lines = [
            "const text = `line with \\n newline",
            "and \\t tab",
            "and \\\\ backslash`;",
        ]

        state = None
        for line in lines:
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

        # Should handle escapes correctly
        assert state.in_template_literal == False, "Template should be closed"

    def test_multiline_template_with_escaped_backtick(self):
        """Test multiline template literal with escaped backtick."""
        lines = [
            "const code = `first line with \\` escaped backtick",
            "second line",
            "third line`;",
        ]

        state = None
        for i, line in enumerate(lines):
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

            print(f"\nLine {i+1}: '{line}'")
            print(f"  Tokens: {[t.type.name for t in tokens]}")
            print(f"  In template: {state.in_template_literal}")

        # Should still be in template after line 1 (backtick was escaped)
        # Should close on line 3
        assert state.in_template_literal == False, "Template should be closed at end"

    def test_multiline_template_with_backslash_continuation(self):
        """Test multiline template with backslash at end of line."""
        lines = [
            "const x = `line ending with backslash\\",
            "continues here`;",
        ]

        state = None
        for line in lines:
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)

        assert state.in_template_literal == False, "Template should be closed"
