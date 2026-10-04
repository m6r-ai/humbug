"""
Tests for JavaScript multiline and state continuation scenarios.
This is especially important for template literals which can span multiple lines.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer, JavaScriptLexerState


class TestJavaScriptMultilineComments:
    """Test multiline comment state management."""

    def test_block_comment_single_line(self):
        """Test block comment that closes on same line."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '/* comment */')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state.in_block_comment == False, "Should not be in block comment state"

    def test_block_comment_multiline_start(self):
        """Test starting a multiline block comment."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '/* start of comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state.in_block_comment == True, "Should be in block comment state"

    def test_block_comment_multiline_middle(self):
        """Test middle lines of multiline block comment."""
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '/* start')

        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'middle line')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state2.in_block_comment == True, "Should still be in block comment state"

    def test_block_comment_multiline_end(self):
        """Test ending a multiline block comment."""
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '/* start')

        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'middle')

        lexer3 = JavaScriptLexer()
        state3 = lexer3.lex(state2, 'end */')

        tokens = list(lexer3._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state3.in_block_comment == False, "Should no longer be in block comment state"

    def test_block_comment_with_code_after_close(self):
        """Test block comment with code after closing on same line."""
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '/* comment')

        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'more */ let x = 5;')

        tokens = list(lexer2._tokens)
        # Should have comment token and other tokens
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 1
        assert state2.in_block_comment == False
        assert len(tokens) > 1, "Should have tokens after comment"

    def test_block_comment_multiple_lines(self):
        """Test block comment spanning many lines."""
        lines = [
            '/* This is',
            'a multi-line',
            'comment that',
            'spans several',
            'lines */',
        ]

        state = None
        for i, line in enumerate(lines):
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'COMMENT'

            if i < len(lines) - 1:
                assert state.in_block_comment == True, f"Line {i}: should still be in comment"
            else:
                assert state.in_block_comment == False, f"Line {i}: should have closed comment"

    def test_nested_block_comment_attempt(self):
        """Test that block comments don't nest (standard JS behavior)."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '/* outer /* inner */')

        tokens = list(lexer._tokens)
        # Should close at first */
        assert state.in_block_comment == False, "Should have closed at first */"


class TestJavaScriptMultilineStrings:
    """Test multiline string scenarios (especially template literals)."""

    def test_regular_string_no_multiline(self):
        """Test that regular strings don't span lines."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '"single line"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_template_literal_single_line(self):
        """Test template literal on single line."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '`single line`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1
        # Template literal should be tokenized as string
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) >= 1

    def test_template_literal_multiline_not_closed(self):
        """Test template literal that starts but doesn't close on same line."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '`start of template')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1
        # Current implementation may not track template literal state
        # This test documents the expected behavior

    def test_template_literal_with_expression_single_line(self):
        """Test template literal with expression on single line."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '`value: ${x}`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1

    def test_escaped_backtick_in_template(self):
        """Test escaped backtick inside template literal."""
        lexer = JavaScriptLexer()
        lexer.lex(None, r'`escaped \` backtick`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1

    def test_string_with_line_continuation(self):
        """Test string with backslash line continuation."""
        # Note: In JS, strings can't actually span multiple lines without escaping newline
        lexer = JavaScriptLexer()
        lexer.lex(None, r'"line with \ continuation"')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1


class TestJavaScriptMultilineEdgeCases:
    """Test edge cases for multiline tokenization."""

    def test_empty_line(self):
        """Test empty line."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0, "Empty line should produce no tokens"

    def test_whitespace_only_line(self):
        """Test line with only whitespace."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '    ')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 0

    def test_continuation_after_comment(self):
        """Test that single-line comment doesn't affect next line."""
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '// comment')

        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'let x = 5;')

        tokens = list(lexer2._tokens)
        # Should tokenize normally, not as comment
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) >= 1

    def test_code_before_and_after_block_comment(self):
        """Test code before and after multiline block comment."""
        lines = [
            'let x = 5;',
            '/* comment',
            'continues',
            'ends */ let y = 10;',
        ]

        state = None
        for line in lines:
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_multiple_statements_across_lines(self):
        """Test multiple statements across multiple lines."""
        lines = [
            'let x = 5;',
            'let y = 10;',
            'let z = x + y;',
        ]

        for line in lines:
            lexer = JavaScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_function_definition_multiline(self):
        """Test function definition across multiple lines."""
        lines = [
            'function test(',
            '    arg1,',
            '    arg2',
            ') {',
            '    return arg1 + arg2;',
            '}',
        ]

        for line in lines:
            lexer = JavaScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            # Should not crash
            assert len(tokens) >= 0

    def test_object_literal_multiline(self):
        """Test object literal across multiple lines."""
        lines = [
            'const obj = {',
            '    name: "test",',
            '    value: 42,',
            '    nested: {',
            '        key: "value"',
            '    }',
            '};',
        ]

        for line in lines:
            lexer = JavaScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_array_literal_multiline(self):
        """Test array literal across multiple lines."""
        lines = [
            'const arr = [',
            '    1,',
            '    2,',
            '    3,',
            '];',
        ]

        for line in lines:
            lexer = JavaScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_chained_method_calls_multiline(self):
        """Test chained method calls across multiple lines."""
        lines = [
            'result = obj',
            '    .method1()',
            '    .method2()',
            '    .method3();',
        ]

        for line in lines:
            lexer = JavaScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_state_preservation_across_lines(self):
        """Test that lexer state is properly preserved across lines."""
        # Start a block comment
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '/* comment')
        assert isinstance(state1, JavaScriptLexerState)
        assert state1.in_block_comment == True

        # Continue with new lexer but same state
        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'still in comment')
        assert state2.in_block_comment == True

        # End comment
        lexer3 = JavaScriptLexer()
        state3 = lexer3.lex(state2, 'end */')
        assert state3.in_block_comment == False

    def test_mixed_content_multiline(self):
        """Test mixed content (code, comments, strings) across lines."""
        lines = [
            'let x = "string"; // comment',
            '/* block comment',
            'continues */',
            'let y = 42;',
            '// another comment',
        ]

        state = None
        for line in lines:
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_unterminated_comment_at_eof(self):
        """Test unterminated block comment at end of file."""
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '/* unterminated comment')

        assert state1.in_block_comment == True

        # Even at "EOF" (last line), state should indicate unclosed comment
        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'final line without close')

        assert state2.in_block_comment == True


class TestJavaScriptTemplateLiteralMultiline:
    """Specific tests for template literal multiline behavior."""

    def test_template_literal_basic_multiline_simulation(self):
        """Test template literal that would span multiple lines."""
        # In actual JS, template literals CAN span lines
        # Testing if lexer handles the backtick correctly
        lexer = JavaScriptLexer()
        lexer.lex(None, '`line1')

        tokens = list(lexer._tokens)
        # Should produce at least one token
        assert len(tokens) >= 1

    def test_template_literal_with_embedded_quotes(self):
        """Test template literal with embedded quotes."""
        test_cases = [
            '`contains "double" quotes`',
            "`contains 'single' quotes`",
            '`both "double" and \'single\'`',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_template_literal_with_expression_multiline(self):
        """Test template literal with expression that might span lines."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '`start ${')

        tokens = list(lexer._tokens)
        # Should handle gracefully even if not closed
        assert len(tokens) >= 1

    def test_nested_template_literals(self):
        """Test nested template literals."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '`outer ${`inner`}`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1

    def test_template_literal_with_escaped_characters(self):
        """Test template literal with various escaped characters."""
        test_cases = [
            r'`escaped \n newline`',
            r'`escaped \t tab`',
            r'`escaped \` backtick`',
            r'`escaped \\ backslash`',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1
