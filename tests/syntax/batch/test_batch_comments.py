"""
Tests for Batch lexer comments and labels.
"""
from syntax.batch.batch_lexer import BatchLexer
from syntax.batch.batch_parser import BatchParser


class TestBatchComments:
    """Test Batch comment tokenization."""

    def test_rem_comment_via_parser(self):
        """Test REM comments are produced by the parser."""
        for comment in [
            'REM This is a comment',
            'rem lowercase rem comment',
            'REM Comment with numbers 123',
            'Rem Mixed Case Comment',
        ]:
            parser = BatchParser()
            parser.parse(None, comment)

            tokens = []
            while True:
                token = parser.get_next_token()
                if token is None:
                    break
                tokens.append(token)

            comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
            assert len(comment_tokens) == 1, f"'{comment}' should produce one COMMENT"
            assert comment_tokens[0].value.startswith('REM') or comment_tokens[0].value.startswith('rem') or \
                comment_tokens[0].value.startswith('Rem')

    def test_rem_is_identifier_in_lexer(self):
        """Test that REM is just an IDENTIFIER from the lexer (parser handles it)."""
        lexer = BatchLexer()
        lexer.lex(None, 'REM some text')

        tokens = list(lexer._tokens)
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'REM'

    def test_double_colon_comment(self):
        """Test :: comments."""
        for comment in [
            ':: This is a comment',
            '::Comment with no space',
        ]:
            lexer = BatchLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'COMMENT'
            assert tokens[0].value == comment

    def test_rem_after_command(self):
        """Test REM is not treated as comment mid-line."""
        parser = BatchParser()
        parser.parse(None, 'echo REM this is printed')

        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)

        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 0


class TestBatchLabels:
    """Test Batch label tokenization."""

    def test_simple_label(self):
        """Test simple labels."""
        for label in [
            ':start',
            ':error_handler',
            ':MyLabel',
        ]:
            lexer = BatchLexer()
            lexer.lex(None, label)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'DIRECTIVE'
            assert tokens[0].value == label

    def test_colon_not_at_line_start(self):
        """Test that : is an operator when not at line start."""
        lexer = BatchLexer()
        lexer.lex(None, 'echo hello:world')

        tokens = list(lexer._tokens)
        label_tokens = [t for t in tokens if t.type.name == 'DIRECTIVE']
        assert len(label_tokens) == 0
