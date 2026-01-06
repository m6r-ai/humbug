"""
Tests for XML comment tokenization.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.lexer import TokenType


class TestXMLComments:
    """Test XML comment tokenization."""

    def test_simple_comment(self):
        """Test simple single-line comment."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- This is a comment -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == '<!-- This is a comment -->'

    def test_empty_comment(self):
        """Test empty comment."""
        lexer = XMLLexer()
        lexer.lex(None, '<!---->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_text(self):
        """Test comment with various text content."""
        test_cases = [
            '<!-- Comment with numbers 123 -->',
            '<!-- Comment with symbols !@#$% -->',
            '<!-- Comment with "quotes" -->',
            "<!-- Comment with 'single quotes' -->",
        ]
        for comment in test_cases:
            lexer = XMLLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment '{comment}' should produce one token"
            assert tokens[0].type == TokenType.COMMENT

    def test_comment_after_tag(self):
        """Test comment appearing after a tag."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>content</tag><!-- comment -->')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == '<!-- comment -->'

    def test_comment_before_tag(self):
        """Test comment appearing before a tag."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- comment --><tag>')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_comment_between_tags(self):
        """Test comment between tags."""
        lexer = XMLLexer()
        lexer.lex(None, '<outer><!-- comment --><inner /></outer>')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

    def test_comment_with_hyphens(self):
        """Test comment containing hyphens (but not --)."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- Comment with - single - hyphens -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_xml_like_content(self):
        """Test comment containing XML-like content."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- <tag attr="value"> -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        # Content inside comment should not be parsed as tags
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 0

    def test_comment_with_special_chars(self):
        """Test comment with special characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- Comment with &lt; &gt; &amp; entities -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_unicode(self):
        """Test comment with Unicode characters."""
        test_cases = [
            '<!-- Comment with emoji 😀 -->',
            '<!-- Comment with Chinese 中文 -->',
            '<!-- Comment with accents café -->',
        ]
        for comment in test_cases:
            lexer = XMLLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.COMMENT

    def test_multiline_comment_start(self):
        """Test starting a multiline comment."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<!-- Start of comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert state.in_comment, "Should be in comment state"

    def test_multiline_comment_middle(self):
        """Test middle line of multiline comment."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<!-- Start')

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, 'Middle line')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert state2.in_comment, "Should still be in comment state"

    def test_multiline_comment_end(self):
        """Test ending a multiline comment."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<!-- Start')

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, 'Middle')

        lexer3 = XMLLexer()
        state3 = lexer3.lex(state2, 'End -->')

        tokens = list(lexer3._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert not state3.in_comment, "Should no longer be in comment state"

    def test_comment_with_newlines_in_content(self):
        """Test multiline comment across several lines."""
        lines = [
            '<!-- This is a',
            'multiline',
            'comment -->',
        ]

        state = None
        for line in lines:
            lexer = XMLLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.COMMENT

        assert not state.in_comment, "Comment should be closed"

    def test_multiple_comments(self):
        """Test multiple comments on different lines."""
        lines = [
            '<!-- First comment -->',
            '<!-- Second comment -->',
            '<!-- Third comment -->',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.COMMENT

    def test_comment_not_confused_with_tag(self):
        """Test that comment start is not confused with tag."""
        lexer = XMLLexer()
        lexer.lex(None, '<!--- Not a tag --->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_comment_with_url(self):
        """Test comment containing URL."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- See https://example.com for details -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_todo_comment(self):
        """Test TODO/FIXME style comments."""
        test_cases = [
            '<!-- TODO: Fix this -->',
            '<!-- FIXME: Bug here -->',
            '<!-- NOTE: Important -->',
        ]
        for comment in test_cases:
            lexer = XMLLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.COMMENT

    def test_commented_out_xml(self):
        """Test commented-out XML code."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- <disabled-tag attr="value">content</disabled-tag> -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        # Should not parse the content as actual XML
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 0

    def test_comment_whitespace_variations(self):
        """Test comments with various whitespace patterns."""
        test_cases = [
            '<!--comment-->',
            '<!-- comment-->',
            '<!--comment -->',
            '<!--  comment  -->',
        ]
        for comment in test_cases:
            lexer = XMLLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.COMMENT

    def test_comment_at_line_end(self):
        """Test comment that ends exactly at line boundary."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<!-- Comment without closing')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert state.in_comment, "Should remain in comment state"
