"""
Tests for YAML anchors, aliases, tags, and document markers.
"""
from syntax.yaml.yaml_lexer import YAMLLexer


class TestYAMLAnchors:
    """Test YAML anchor tokenization."""

    def test_anchor(self):
        """Test basic anchor."""
        lexer = YAMLLexer()
        lexer.lex(None, '&anchor')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'HASH'
        assert tokens[0].value == '&anchor'

    def test_anchor_with_value(self):
        """Test anchor with a value on the same line."""
        lexer = YAMLLexer()
        lexer.lex(None, '&defaults 30')

        tokens = list(lexer._tokens)
        anchor_tokens = [t for t in tokens if t.type.name == 'HASH']
        assert len(anchor_tokens) == 1
        assert anchor_tokens[0].value == '&defaults'

    def test_anchor_with_underscore_and_dash(self):
        """Test anchor names with underscores and dashes."""
        for name in ['&my_anchor', '&my-anchor', '&anchor123']:
            lexer = YAMLLexer()
            lexer.lex(None, name)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'HASH'
            assert tokens[0].value == name


class TestYAMLAliases:
    """Test YAML alias tokenization."""

    def test_alias(self):
        """Test basic alias."""
        lexer = YAMLLexer()
        lexer.lex(None, '*anchor')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'HASH'
        assert tokens[0].value == '*anchor'

    def test_alias_merge_key(self):
        """Test merge key syntax with alias."""
        lexer = YAMLLexer()
        lexer.lex(None, '<<: *defaults')

        tokens = list(lexer._tokens)
        alias_tokens = [t for t in tokens if t.type.name == 'HASH']
        assert len(alias_tokens) == 1
        assert alias_tokens[0].value == '*defaults'


class TestYAMLTags:
    """Test YAML tag tokenization."""

    def test_double_bang_tag(self):
        """Test built-in tag (!!str, !!int, etc.)."""
        lexer = YAMLLexer()
        lexer.lex(None, '!!str')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'DIRECTIVE'
        assert tokens[0].value == '!!str'

    def test_single_bang_tag(self):
        """Test custom tag (!my_tag)."""
        lexer = YAMLLexer()
        lexer.lex(None, '!my_tag')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'DIRECTIVE'
        assert tokens[0].value == '!my_tag'

    def test_tag_with_value(self):
        """Test tag followed by a value."""
        lexer = YAMLLexer()
        lexer.lex(None, '!!str 42')

        tokens = list(lexer._tokens)
        tag_tokens = [t for t in tokens if t.type.name == 'DIRECTIVE']
        assert len(tag_tokens) == 1
        assert tag_tokens[0].value == '!!str'


class TestYAMLDocumentMarkers:
    """Test YAML document start/end markers."""

    def test_document_start(self):
        """Test --- document start marker."""
        lexer = YAMLLexer()
        lexer.lex(None, '---')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'DOC_COMMENT'
        assert tokens[0].value == '---'

    def test_document_end(self):
        """Test ... document end marker."""
        lexer = YAMLLexer()
        lexer.lex(None, '...')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'DOC_COMMENT'
        assert tokens[0].value == '...'

    def test_document_start_with_content(self):
        """Test --- followed by content on same line."""
        lexer = YAMLLexer()
        lexer.lex(None, '--- key: value')

        tokens = list(lexer._tokens)
        doc_tokens = [t for t in tokens if t.type.name == 'DOC_COMMENT']
        assert len(doc_tokens) == 1
        assert doc_tokens[0].value == '--- key: value'
