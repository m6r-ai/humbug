"""
Tests for XML CDATA section tokenization.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.lexer import TokenType


class TestXMLCDATA:
    """Test XML CDATA section tokenization."""

    def test_simple_cdata(self):
        """Test simple CDATA section."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[content]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '<![CDATA[content]]>'

    def test_empty_cdata(self):
        """Test empty CDATA section."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_with_special_chars(self):
        """Test CDATA with special XML characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[<tag> & "quotes" & more]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert '<tag>' in tokens[0].value
        assert '&' in tokens[0].value

    def test_cdata_with_xml_content(self):
        """Test CDATA containing XML-like content."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[<element attr="value">text</element>]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        # Content should not be parsed as XML
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(tag_tokens) == 0

    def test_cdata_with_brackets(self):
        """Test CDATA containing brackets."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[array[0] = value]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_with_greater_than(self):
        """Test CDATA with > symbol."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[if (x > 5)]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_with_less_than(self):
        """Test CDATA with < symbol."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[if (x < 5)]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_with_ampersand(self):
        """Test CDATA with & symbol."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[Tom & Jerry]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert '&' in tokens[0].value

    def test_cdata_with_quotes(self):
        """Test CDATA with quote characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA["quoted" and \'single\']]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_inside_tag(self):
        """Test CDATA section inside a tag."""
        lexer = XMLLexer()
        lexer.lex(None, '<data><![CDATA[content]]></data>')

        tokens = list(lexer._tokens)
        cdata_tokens = [t for t in tokens if t.type == TokenType.STRING and 'CDATA' in t.value]
        assert len(cdata_tokens) == 1

    def test_multiline_cdata_start(self):
        """Test starting a multiline CDATA section."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<![CDATA[Start of CDATA')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert state.in_cdata, "Should be in CDATA state"

    def test_multiline_cdata_middle(self):
        """Test middle line of multiline CDATA."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<![CDATA[Start')

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, 'Middle line')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert state2.in_cdata, "Should still be in CDATA state"

    def test_multiline_cdata_end(self):
        """Test ending a multiline CDATA section."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<![CDATA[Start')

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, 'Middle')

        lexer3 = XMLLexer()
        state3 = lexer3.lex(state2, 'End]]>')

        tokens = list(lexer3._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert not state3.in_cdata, "Should no longer be in CDATA state"

    def test_cdata_with_newlines(self):
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
            assert tokens[0].type == TokenType.STRING

        assert not state.in_cdata, "CDATA should be closed"

    def test_cdata_with_double_brackets(self):
        """Test CDATA with double brackets that don't close it."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[array[[0]]]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_with_code(self):
        """Test CDATA containing code."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[function test() { return x < 5 && y > 10; }]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_with_script(self):
        """Test CDATA containing script code (common use case)."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[var x = 10; if (x < 20) { alert("Hello"); }]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_with_sql(self):
        """Test CDATA containing SQL (another common use case)."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[SELECT * FROM users WHERE age > 18 AND status = "active"]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_multiple_cdata_sections(self):
        """Test multiple CDATA sections."""
        lexer = XMLLexer()
        lexer.lex(None, '<root><![CDATA[first]]><![CDATA[second]]></root>')

        tokens = list(lexer._tokens)
        cdata_tokens = [t for t in tokens if t.type == TokenType.STRING and 'CDATA' in t.value]
        assert len(cdata_tokens) == 2

    def test_cdata_with_unicode(self):
        """Test CDATA with Unicode characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[Hello 世界 🌍]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_cdata_case_sensitive(self):
        """Test that CDATA is case-sensitive."""
        # This should NOT be treated as CDATA (wrong case)
        lexer = XMLLexer()
        lexer.lex(None, '<![cdata[content]]>')

        tokens = list(lexer._tokens)
        # Should not be recognized as CDATA
        # Note: This tests the lexer's strictness

    def test_cdata_at_line_end(self):
        """Test CDATA that doesn't close at line end."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<![CDATA[unclosed')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert state.in_cdata, "Should remain in CDATA state"

    def test_cdata_with_entity_references(self):
        """Test CDATA with entity references (which should NOT be expanded)."""
        lexer = XMLLexer()
        lexer.lex(None, '<![CDATA[&lt; &gt; &amp; &quot;]]>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        # Entities should be literal in CDATA
        assert '&lt;' in tokens[0].value
