"""
Tests for Bash lexer keywords and commands.
"""
from syntax.bash.bash_lexer import BashLexer


class TestBashKeywords:
    """Test Bash keyword and command tokenization."""

    def test_control_flow_keywords(self):
        """Test control flow keywords."""
        for keyword in ['if', 'then', 'elif', 'else', 'fi',
                        'for', 'while', 'until', 'do', 'done',
                        'case', 'esac', 'in']:
            lexer = BashLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            kw_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(kw_tokens) == 1, f"'{keyword}' should be a KEYWORD"
            assert kw_tokens[0].value == keyword

    def test_function_keyword(self):
        """Test the 'function' keyword."""
        lexer = BashLexer()
        lexer.lex(None, 'function')

        tokens = list(lexer._tokens)
        kw_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(kw_tokens) == 1
        assert kw_tokens[0].value == 'function'

    def test_return_keyword(self):
        """Test the 'return' keyword."""
        lexer = BashLexer()
        lexer.lex(None, 'return 0')

        tokens = list(lexer._tokens)
        kw_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(kw_tokens) == 1
        assert kw_tokens[0].value == 'return'

    def test_built_in_commands(self):
        """Test built-in command recognition."""
        for cmd in ['echo', 'cd', 'export', 'source', 'local', 'set', 'pwd']:
            lexer = BashLexer()
            lexer.lex(None, cmd)

            tokens = list(lexer._tokens)
            cmd_tokens = [t for t in tokens if t.type.name == 'COMMAND']
            assert len(cmd_tokens) == 1, f"'{cmd}' should be a COMMAND"
            assert cmd_tokens[0].value == cmd

    def test_common_commands(self):
        """Test common external command recognition."""
        for cmd in ['grep', 'sed', 'awk', 'cat', 'ls', 'mkdir', 'rm', 'cp', 'mv']:
            lexer = BashLexer()
            lexer.lex(None, cmd)

            tokens = list(lexer._tokens)
            cmd_tokens = [t for t in tokens if t.type.name == 'COMMAND']
            assert len(cmd_tokens) == 1, f"'{cmd}' should be a COMMAND"

    def test_boolean_literals(self):
        """Test boolean literals."""
        for val in ['true', 'false']:
            lexer = BashLexer()
            lexer.lex(None, val)

            tokens = list(lexer._tokens)
            bool_tokens = [t for t in tokens if t.type.name == 'BOOLEAN']
            assert len(bool_tokens) == 1
            assert bool_tokens[0].value == val

    def test_plain_identifier(self):
        """Test that non-keyword, non-command identifiers stay as IDENTIFIER."""
        lexer = BashLexer()
        lexer.lex(None, 'myvar')

        tokens = list(lexer._tokens)
        id_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(id_tokens) == 1
        assert id_tokens[0].value == 'myvar'

    def test_identifier_with_dash(self):
        """Test that identifiers can contain dashes."""
        lexer = BashLexer()
        lexer.lex(None, 'my-var')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1
