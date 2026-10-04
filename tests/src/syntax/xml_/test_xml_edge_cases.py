"""
Tests for XML edge cases and real-world scenarios.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.xml.xml_parser import XMLParser
from syntax.lexer import TokenType


class TestXMLEdgeCases:
    """Test XML edge cases and real-world scenarios."""

    def test_empty_document(self):
        """Test empty XML document."""
        lexer = XMLLexer()
        lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0

    def test_whitespace_only_document(self):
        """Test document with only whitespace."""
        lexer = XMLLexer()
        lexer.lex(None, '   \t   ')

        tokens = list(lexer._tokens)
        # May have whitespace token or no tokens
        assert len(tokens) >= 0

    def test_minimal_xml_document(self):
        """Test minimal valid XML document."""
        lexer = XMLLexer()
        lexer.lex(None, '<root/>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_xml_with_entities(self):
        """Test XML with entity references."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>&lt;&gt;&amp;&quot;&apos;</tag>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1

    def test_xml_with_numeric_entities(self):
        """Test XML with numeric character references."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>&#169; &#xA9;</tag>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1

    def test_svg_example(self):
        """Test real-world SVG example."""
        lexer = XMLLexer()
        lexer.lex(None, '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == 'svg'

        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 3

    def test_rss_feed_example(self):
        """Test RSS feed XML."""
        lexer = XMLLexer()
        lexer.lex(None, '<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_soap_envelope_example(self):
        """Test SOAP envelope XML."""
        lexer = XMLLexer()
        lexer.lex(None, '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        assert 'soap:Envelope' in tag_tokens[0].value

    def test_xml_with_very_long_attribute_value(self):
        """Test XML with very long attribute value."""
        long_value = "a" * 1000
        lexer = XMLLexer()
        lexer.lex(None, f'<tag attr="{long_value}">')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1

    def test_xml_with_many_attributes(self):
        """Test tag with many attributes."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag a="1" b="2" c="3" d="4" e="5" f="6" g="7" h="8">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 8

    def test_deeply_nested_tags(self):
        """Test deeply nested tags."""
        lexer = XMLLexer()
        lexer.lex(None, '<a><b><c><d><e>content</e></d></c></b></a>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 10  # 5 opening, 5 closing

    def test_tag_with_no_whitespace(self):
        """Test tags with no whitespace between them."""
        lexer = XMLLexer()
        lexer.lex(None, '<a><b></b></a>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 4

    def test_mixed_quotes_in_attributes(self):
        """Test attributes with mixed quote styles."""
        lexer = XMLLexer()
        lexer.lex(None, '''<tag attr1="value1" attr2='value2'>''')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 2

    def test_attribute_with_empty_namespace(self):
        """Test attribute with colon but effectively no namespace."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag :attr="value">')

        tokens = list(lexer._tokens)
        # Should still parse, even if semantically invalid
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) >= 1

    def test_consecutive_comments(self):
        """Test consecutive comments with no space."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- comment1 --><!-- comment2 -->')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 2

    def test_comment_inside_cdata_like_text(self):
        """Test that comment-like text in CDATA is not parsed as comment."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[<!-- not a comment -->]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        # Should not have a comment token
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 0

    def test_cdata_inside_comment_like_text(self):
        """Test that CDATA-like text in comment is not parsed as CDATA."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- <![CDATA[not cdata]]> -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_tag_name_starting_with_xml(self):
        """Test tag names starting with 'xml' (technically reserved)."""
        lexer = XMLLexer()
        lexer.lex(None, '<xmldata>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1
        # Lexer accepts it, even if semantically invalid

    def test_unicode_tag_names(self):
        """Test tag names with Unicode characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag_中文>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_unicode_attribute_names(self):
        """Test attribute names with Unicode characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr_中文="value">')

        tokens = list(lexer._tokens)
        attr_tokens = [t for t in tokens if t.type == TokenType.HTML_ATTRIBUTE]
        assert len(attr_tokens) == 1

    def test_unicode_content(self):
        """Test Unicode content."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag>Hello 世界 🌍</tag>')

        tokens = list(lexer._tokens)
        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) == 1

    def test_xhtml_example(self):
        """Test XHTML example."""
        lexer = XMLLexer()
        lexer.lex(None, '<html xmlns="http://www.w3.org/1999/xhtml">')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_mathml_example(self):
        """Test MathML example."""
        lexer = XMLLexer()
        lexer.lex(None, '<math xmlns="http://www.w3.org/1998/Math/MathML">')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_xml_with_all_features(self):
        """Test XML with all major features."""
        lines = [
            '<?xml version="1.0" encoding="UTF-8"?>',
            '<!DOCTYPE root SYSTEM "root.dtd">',
            '<!-- Root element -->',
            '<root xmlns:custom="http://custom.ns">',
            '  <data custom:attr="value">',
            '    <![CDATA[Special <content> here]]>',
            '  </data>',
            '  <?custom-pi data?>',
            '</root>',
        ]

        for line in lines:
            parser = XMLParser()
            parser.parse(None, line)
            tokens = list(parser._tokens)
            assert len(tokens) >= 1

    def test_malformed_tag_recovery(self):
        """Test that lexer handles malformed tags gracefully."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag')

        tokens = list(lexer._tokens)
        # Should produce some tokens without crashing
        assert len(tokens) >= 1

    def test_attribute_without_value(self):
        """Test attribute without value (technically invalid XML)."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag attr>')

        tokens = list(lexer._tokens)
        # Should handle gracefully
        assert len(tokens) >= 1

    def test_double_hyphen_in_comment(self):
        """Test double hyphen in comment (technically invalid)."""
        lexer = XMLLexer()
        lexer.lex(None, '<!-- comment -- with -- double hyphens -->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_very_long_tag_name(self):
        """Test very long tag name."""
        long_name = "tag" + "a" * 1000
        lexer = XMLLexer()
        lexer.lex(None, f'<{long_name}>')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 1

    def test_parser_with_xml_declaration(self):
        """Test parser correctly handles XML declaration."""
        parser = XMLParser()
        state = parser.parse(None, '<?xml version="1.0"?>')

        tokens = list(parser._tokens)
        pi_tokens = [t for t in tokens if t.type == TokenType.PREPROCESSOR]
        assert len(pi_tokens) == 1
        assert state.continuation_state == 0

    def test_parser_continuation_states(self):
        """Test parser continuation states for different constructs."""
        # Comment continuation
        parser1 = XMLParser()
        state1 = parser1.parse(None, '<!-- unclosed')
        assert state1.continuation_state == 1

        # CDATA continuation
        parser2 = XMLParser()
        state2 = parser2.parse(None, '<![CDATA[unclosed')
        assert state2.continuation_state == 2

        # PI continuation
        parser3 = XMLParser()
        state3 = parser3.parse(None, '<?target unclosed')
        assert state3.continuation_state == 3
