"""
Tests for YAML comment tokenization.
"""
from syntax.yaml.yaml_lexer import YAMLLexer


class TestYAMLComments:
    """Test YAML comment tokenization."""

    def test_simple_comment(self):
        """Test simple single-line comments."""
        for comment in [
            '# This is a comment',
            '# Comment with numbers 123',
            '# Comment with symbols !@#$%',
            '#NoSpaceComment',
        ]:
            lexer = YAMLLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment '{comment}' should produce one token"
            assert tokens[0].type.name == 'COMMENT'
            assert tokens[0].value == comment

    def test_empty_comment(self):
        """Test empty comment (just the # symbol)."""
        lexer = YAMLLexer()
        lexer.lex(None, '#')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'

    def test_comment_after_key_value(self):
        """Test comments appearing after a key-value pair."""
        lexer = YAMLLexer()
        lexer.lex(None, 'key: value  # trailing comment')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == '# trailing comment'

    def test_comment_with_quotes(self):
        """Test comments containing quote characters."""
        for comment in [
            '# Comment with "double quotes"',
            "# Comment with 'single quotes'",
        ]:
            lexer = YAMLLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'COMMENT'

    def test_comment_consumes_rest_of_line(self):
        """Test that a comment consumes everything to end of line."""
        lexer = YAMLLexer()
        lexer.lex(None, '# key: value [a, b] {c: d}')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'

    def test_comment_inside_flow_context(self):
        """Test comments inside flow context are still recognized."""
        lexer = YAMLLexer()
        lexer.lex(None, '[a, b  # not a comment in YAML flow')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == '# not a comment in YAML flow'
