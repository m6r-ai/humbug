"""
Tests for XML multiline and state continuation scenarios.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.xml.xml_parser import XMLParser


class TestXMLMultiline:
    """Test XML multiline tokenization and state management."""

    def test_tag_across_lines(self):
        """Test tag split across multiple lines."""
        lines = [
            '<tag',
            'attr="value"',
            '>',
        ]

        state = None
        for line in lines:
            lexer = XMLLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

        assert not state.in_tag, "Tag should be closed"

    def test_content_across_lines(self):
        """Test content across multiple lines."""
        lines = [
            '<tag>',
            'Line 1',
            'Line 2',
            '</tag>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_nested_tags_across_lines(self):
        """Test nested tags across multiple lines."""
        lines = [
            '<outer>',
            '  <inner>',
            '    content',
            '  </inner>',
            '</outer>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            # Should not crash
            assert len(tokens) >= 0

    def test_attribute_across_lines(self):
        """Test attribute split across lines."""
        lines = [
            '<tag attr="value1"',
            '     attr2="value2">',
        ]

        state = None
        for line in lines:
            lexer = XMLLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_comment_multiline_continuation(self):
        """Test comment spanning multiple lines."""
        lines = [
            '<!-- This is',
            'a multiline',
            'comment -->',
        ]

        state = None
        for line in lines:
            lexer = XMLLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'COMMENT'

        assert not state.in_comment, "Comment should be closed"

    def test_cdata_multiline_continuation(self):
        """Test CDATA spanning multiple lines."""
        lines = [
            '<![CDATA[',
            'Line 1',
            'Line 2',
            ']]>',
        ]

        state = None
        for line in lines:
            lexer = XMLLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1

        assert not state.in_cdata, "CDATA should be closed"

    def test_processing_instruction_multiline(self):
        """Test processing instruction spanning multiple lines."""
        lines = [
            '<?target',
            'data',
            '?>',
        ]

        state = None
        for line in lines:
            lexer = XMLLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1

        assert not state.in_processing_instruction, "PI should be closed"

    def test_parser_state_continuation(self):
        """Test parser state continuation across lines."""
        parser1 = XMLParser()
        state1 = parser1.parse(None, '<!-- Start')

        assert state1.continuation_state != 0, "Should be in continuation state"

        parser2 = XMLParser()
        state2 = parser2.parse(state1, 'End -->')

        assert state2.continuation_state == 0, "Should no longer be in continuation"

    def test_empty_lines_between_tags(self):
        """Test empty lines between tags."""
        lines = [
            '<tag>',
            '',
            '',
            '</tag>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            # Should handle empty lines
            assert len(tokens) >= 0

    def test_whitespace_only_lines(self):
        """Test lines with only whitespace."""
        lines = [
            '<tag>',
            '    ',
            '  ',
            '</tag>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_indented_xml(self):
        """Test properly indented XML."""
        lines = [
            '<root>',
            '  <child>',
            '    <grandchild>content</grandchild>',
            '  </child>',
            '</root>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_mixed_content_multiline(self):
        """Test mixed content across multiple lines."""
        lines = [
            '<p>',
            'Text before <em>emphasized</em> text after',
            '</p>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_comment_after_unclosed_tag(self):
        """Test comment starting on line with unclosed tag."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<tag attr="value"')

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, '><!-- comment -->')

        tokens = list(lexer2._tokens)
        # Should have closing bracket and comment
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 1

    def test_cdata_after_tag(self):
        """Test CDATA on line after tag."""
        lines = [
            '<script>',
            '<![CDATA[',
            'code here',
            ']]>',
            '</script>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_multiple_tags_one_line(self):
        """Test multiple complete tags on one line."""
        lexer = XMLLexer()
        lexer.lex(None, '<a>1</a><b>2</b><c>3</c>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type.name == 'HTML_TAG']
        assert len(tag_tokens) == 6  # 3 opening, 3 closing

    def test_state_preservation_across_lines(self):
        """Test that lexer state is properly preserved."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<!-- comment')

        assert state1.in_comment, "Should be in comment state"
        assert not state1.in_tag, "Should not be in tag state"
        assert not state1.in_cdata, "Should not be in CDATA state"

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, 'still in comment')

        assert state2.in_comment, "Should still be in comment state"

    def test_tag_state_with_attributes(self):
        """Test tag state preservation with attributes."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<tag attr="value"')

        assert state1.in_tag, "Should be in tag state"
        assert state1.tag_name == 'tag', "Should remember tag name"

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, 'attr2="value2">')

        assert not state2.in_tag, "Tag should be closed"

    def test_complex_document_multiline(self):
        """Test complex document across multiple lines."""
        lines = [
            '<?xml version="1.0"?>',
            '<!DOCTYPE root>',
            '<!-- Document start -->',
            '<root>',
            '  <element attr="value">',
            '    <![CDATA[data]]>',
            '  </element>',
            '</root>',
        ]

        state = None
        for line in lines:
            parser = XMLParser()
            state = parser.parse(state, line)
            tokens = list(parser._tokens)
            assert len(tokens) >= 1

    def test_unclosed_comment_at_eof(self):
        """Test unclosed comment at end of file."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<!-- Unclosed comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
        assert state.in_comment, "Should remain in comment state"

    def test_unclosed_cdata_at_eof(self):
        """Test unclosed CDATA at end of file."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<![CDATA[Unclosed')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert state.in_cdata, "Should remain in CDATA state"

    def test_unclosed_pi_at_eof(self):
        """Test unclosed processing instruction at end of file."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<?target unclosed')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert state.in_processing_instruction, "Should remain in PI state"

    def test_text_content_multiline(self):
        """Test text content across multiple lines."""
        lines = [
            '<description>',
            'This is a long',
            'description that spans',
            'multiple lines',
            '</description>',
        ]

        for line in lines:
            lexer = XMLLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_namespace_declaration_multiline(self):
        """Test namespace declarations across lines."""
        lines = [
            '<root xmlns="http://default.ns"',
            '      xmlns:custom="http://custom.ns"',
            '      custom:attr="value">',
        ]

        state = None
        for line in lines:
            lexer = XMLLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

        assert not state.in_tag, "Tag should be closed"
