"""
Tests for XML processing instruction tokenization.
"""
import pytest

from syntax.xml.xml_lexer import XMLLexer
from syntax.lexer import TokenType


class TestXMLProcessingInstructions:
    """Test XML processing instruction tokenization."""

    def test_xml_declaration(self):
        """Test standard XML declaration."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml version="1.0"?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR
        assert tokens[0].value == '<?xml version="1.0"?>'

    def test_xml_declaration_with_encoding(self):
        """Test XML declaration with encoding."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml version="1.0" encoding="UTF-8"?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR
        assert 'encoding="UTF-8"' in tokens[0].value

    def test_xml_declaration_full(self):
        """Test full XML declaration with all attributes."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_simple_processing_instruction(self):
        """Test simple processing instruction."""
        lexer = XMLLexer()
        lexer.lex(None, '<?target instruction?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_processing_instruction_with_data(self):
        """Test processing instruction with data."""
        lexer = XMLLexer()
        lexer.lex(None, '<?php echo "Hello"; ?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_processing_instruction_stylesheet(self):
        """Test xml-stylesheet processing instruction."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml-stylesheet type="text/xsl" href="style.xsl"?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_empty_processing_instruction(self):
        """Test empty processing instruction."""
        lexer = XMLLexer()
        lexer.lex(None, '<?target?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_processing_instruction_with_special_chars(self):
        """Test processing instruction with special characters."""
        lexer = XMLLexer()
        lexer.lex(None, '<?target data="value" attr=\'test\'?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_processing_instruction_after_tag(self):
        """Test processing instruction after a tag."""
        lexer = XMLLexer()
        lexer.lex(None, '<tag><?target data?></tag>')

        tokens = list(lexer._tokens)
        pi_tokens = [t for t in tokens if t.type == TokenType.PREPROCESSOR]
        assert len(pi_tokens) == 1

    def test_processing_instruction_before_root(self):
        """Test processing instruction before root element."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml version="1.0"?><root />')

        tokens = list(lexer._tokens)
        pi_tokens = [t for t in tokens if t.type == TokenType.PREPROCESSOR]
        assert len(pi_tokens) == 1

    def test_multiline_processing_instruction_start(self):
        """Test starting a multiline processing instruction."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<?target unclosed')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR
        assert state.in_processing_instruction, "Should be in PI state"

    def test_multiline_processing_instruction_end(self):
        """Test ending a multiline processing instruction."""
        lexer1 = XMLLexer()
        state1 = lexer1.lex(None, '<?target start')

        lexer2 = XMLLexer()
        state2 = lexer2.lex(state1, 'end?>')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR
        assert not state2.in_processing_instruction, "Should no longer be in PI state"

    def test_processing_instruction_with_question_mark(self):
        """Test processing instruction containing ? (but not ?> to close)."""
        lexer = XMLLexer()
        lexer.lex(None, '<?target is this a question??>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_multiple_processing_instructions(self):
        """Test multiple processing instructions."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml version="1.0"?><?xml-stylesheet href="style.css"?>')

        tokens = list(lexer._tokens)
        pi_tokens = [t for t in tokens if t.type == TokenType.PREPROCESSOR]
        assert len(pi_tokens) == 2

    def test_processing_instruction_case_sensitive(self):
        """Test that processing instruction targets are case-sensitive."""
        lexer = XMLLexer()
        lexer.lex(None, '<?XML version="1.0"?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR
        # Note: <?XML is technically illegal (should be lowercase), but lexer accepts it

    def test_processing_instruction_with_whitespace(self):
        """Test processing instruction with various whitespace."""
        lexer = XMLLexer()
        lexer.lex(None, '<?  target   data  ?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_processing_instruction_not_confused_with_tag(self):
        """Test that PI is not confused with tags."""
        lexer = XMLLexer()
        lexer.lex(None, '<?target?><tag />')

        tokens = list(lexer._tokens)
        pi_tokens = [t for t in tokens if t.type == TokenType.PREPROCESSOR]
        tag_tokens = [t for t in tokens if t.type == TokenType.HTML_TAG]
        assert len(pi_tokens) == 1
        assert len(tag_tokens) == 1

    def test_processing_instruction_at_line_end(self):
        """Test processing instruction that doesn't close at line end."""
        lexer = XMLLexer()
        state = lexer.lex(None, '<?target unclosed')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR
        assert state.in_processing_instruction, "Should remain in PI state"

    def test_xml_declaration_version_11(self):
        """Test XML 1.1 declaration."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml version="1.1"?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR

    def test_processing_instruction_with_url(self):
        """Test processing instruction with URL."""
        lexer = XMLLexer()
        lexer.lex(None, '<?xml-stylesheet href="https://example.com/style.xsl"?>')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.PREPROCESSOR
