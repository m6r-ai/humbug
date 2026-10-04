"""
Tests for JavaScript comment tokenization.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestJavaScriptComments:
    """Test JavaScript comment tokenization."""

    def test_single_line_comments(self):
        """Test single-line // comments."""
        test_cases = [
            '// this is a comment',
            '//comment without space',
            '// comment with symbols !@#$%',
            '//',  # empty comment
            '// comment with "quotes" and \'apostrophes\'',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"'{code}' should be COMMENT type"
            assert tokens[0].value == code

    def test_block_comments_single_line(self):
        """Test block comments on a single line."""
        test_cases = [
            '/* comment */',
            '/* another comment */',
            '/**/',  # empty block comment
            '/* comment with symbols !@#$% */',
            '/* comment with "quotes" */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"'{code}' should be COMMENT type"
            assert tokens[0].value == code

    def test_multiline_block_comment_start(self):
        """Test starting a multiline block comment."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '/* start of comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'COMMENT'
        assert state.in_block_comment == True, "Should be in block comment state"

    def test_multiline_block_comment_middle(self):
        """Test middle lines of multiline block comment."""
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '/* start')

        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'middle line')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'COMMENT'
        assert state2.in_block_comment == True, "Should still be in block comment state"

    def test_multiline_block_comment_end(self):
        """Test ending a multiline block comment."""
        lexer1 = JavaScriptLexer()
        state1 = lexer1.lex(None, '/* start')

        lexer2 = JavaScriptLexer()
        state2 = lexer2.lex(state1, 'middle')

        lexer3 = JavaScriptLexer()
        state3 = lexer3.lex(state2, 'end */')

        tokens = list(lexer3._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'COMMENT'
        assert state3.in_block_comment == False, "Should no longer be in block comment state"

    def test_block_comment_with_code_after(self):
        """Test block comment with code on same line."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '/* comment */ let x = 5;')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 1, "Should have one comment token"

        # Should have other tokens after the comment
        assert len(tokens) > 1, "Should have tokens after comment"

    def test_comment_with_asterisks(self):
        """Test block comment containing asterisks."""
        test_cases = [
            '/* * */',
            '/* ** */',
            '/* *** */',
            '/* comment * with * asterisks */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_comment_with_slashes(self):
        """Test block comment containing slashes."""
        test_cases = [
            '/* / */',
            '/* // */',
            '/* comment / with / slashes */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_unterminated_block_comment(self):
        """Test unterminated block comment."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '/* unterminated comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'COMMENT'
        assert state.in_block_comment == True, "Should still be in block comment state"

    def test_jsdoc_comments(self):
        """Test JSDoc-style comments."""
        test_cases = [
            '/** JSDoc comment */',
            '/**\n * @param {string} name\n */',
            '/** @returns {number} */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
            assert len(comment_tokens) >= 1, f"'{code}' should have comment tokens"


class TestJavaScriptCommentEdgeCases:
    """Test edge cases for JavaScript comment tokenization."""

    def test_comment_after_code(self):
        """Test comment appearing after code on same line."""
        test_cases = [
            'let x = 5; // comment',
            'const y = 10; /* comment */',
            'function test() {} // end',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
            assert len(comment_tokens) == 1, f"'{code}' should have one comment token"

    def test_comment_like_strings(self):
        """Test strings that look like comments."""
        test_cases = [
            '"// not a comment"',
            '"/* not a comment */"',
            "'// also not a comment'",
            "'/* also not a comment */'",
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'STRING', "Should be STRING, not COMMENT"

    def test_regex_that_looks_like_comment(self):
        """Test regex that might look like a comment."""
        # Note: This is tricky - context determines if // is regex or comment
        test_cases = [
            '//',  # Could be empty regex or comment - should be comment
            '/* */',  # Block comment
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_division_operator_not_comment(self):
        """Test that division operator is not confused with comment."""
        test_cases = [
            'x / y',
            'a /= b',
            'result = 10 / 2',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
            assert len(comment_tokens) == 0, f"'{code}' should not have comment tokens"

    def test_nested_block_comments(self):
        """Test that block comments don't nest (they shouldn't in JS)."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '/* outer /* inner */ still in comment? */')

        tokens = list(lexer._tokens)
        # First comment should end at first */
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) >= 1, "Should have comment token(s)"

    def test_block_comment_with_newlines(self):
        """Test block comment containing newlines."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '/* line1\nline2\nline3 */')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should produce tokens"

    def test_multiple_comments_on_line(self):
        """Test multiple comments on same line."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '/* comment1 */ /* comment2 */')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 2, "Should have two comment tokens"

    def test_comment_at_end_of_file(self):
        """Test comment at the very end."""
        test_cases = [
            '// comment at end',
            '/* comment at end */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_almost_comment(self):
        """Test sequences that almost look like comments."""
        test_cases = [
            '/ /',  # division with spaces
            '/ * x',  # division and multiplication
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
            assert len(comment_tokens) == 0, f"'{code}' should not have comment tokens"
