"""
Tests for Lua comment tokenization.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer


class TestLuaComments:
    """Test Lua comment tokenization."""

    def test_single_line_comments(self):
        """Test single-line comments."""
        test_cases = [
            '-- comment',
            '-- comment with more text',
            '-- trailing spaces   ',
            '--42',
            '--',
        ]
        for comment in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment '{comment}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"Comment should be COMMENT type"
            assert tokens[0].value == comment, f"Comment should capture full line"

    def test_single_line_comment_at_start(self):
        """Test that lexer handles comment-only lines."""
        lexer = LuaLexer()
        lexer.lex(None, '--comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should produce one comment token"
        assert tokens[0].type.name == 'COMMENT'
        assert tokens[0].value == '--comment'

    def test_single_line_comment_in_middle(self):
        """Test comments at end of code line."""
        lexer = LuaLexer()
        lexer.lex(None, 'local x = 42 -- comment')

        tokens = list(lexer._tokens)
        assert tokens[0].type.name == 'KEYWORD', "First should be 'local' keyword"
        assert tokens[1].type.name == 'IDENTIFIER', "Second should be identifier"
        assert tokens[2].type.name == 'OPERATOR', "Third should be equals"
        assert tokens[3].type.name == 'NUMBER', "Fourth should be number"
        assert tokens[4].type.name == 'COMMENT', "Fifth should be comment"

    def test_block_comments(self):
        """Test multi-line block comments."""
        test_cases = [
            '--[[single line]]',
        ]
        for comment in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Block comment '{comment}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"Block comment should be COMMENT type"

    def test_unclosed_block_comment(self):
        """Test unclosed block comment."""
        lexer = LuaLexer()
        state = lexer.lex(None, '--[[start comment')

        assert state.in_block_comment == True, "Should be in block comment state"

    def test_block_comment_after_code(self):
        """Test block comment on same line as code."""
        lexer = LuaLexer()
        lexer.lex(None, 'local x = 42 --[[comment]]')

        tokens = list(lexer._tokens)
        assert len(tokens) == 5, "Should have keyword, identifier, operator, number, and comment"
        assert tokens[4].type.name == 'COMMENT'

    def test_comment_adjacent_to_code(self):
        """Test comment adjacent to code."""
        lexer = LuaLexer()
        lexer.lex(None, 'x=1 --comment y=2')

        tokens = list(lexer._tokens)
        assert tokens[0].type.name == 'IDENTIFIER', "First should be identifier"
        assert tokens[1].type.name == 'OPERATOR', "Second should be equals"
        assert tokens[2].type.name == 'NUMBER', "Third should be number"
        assert tokens[3].type.name == 'COMMENT', "Fourth should be comment"
        assert tokens[3].value == '--comment y=2', "Comment should include rest of line"
        assert len(tokens) == 4, "Should only have 4 tokens (comment consumes rest)"
