"""
Tests for Batch lexer variables.
"""
from syntax.batch.batch_lexer import BatchLexer


class TestBatchVariables:
    """Test Batch variable tokenization."""

    def test_percent_variable(self):
        """Test %VAR% variables."""
        lexer = BatchLexer()
        lexer.lex(None, '%PATH%')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '%PATH%'

    def test_positional_parameter(self):
        """Test %0-%9 positional parameters."""
        for i in range(10):
            lexer = BatchLexer()
            lexer.lex(None, f'%{i}')

            tokens = list(lexer._tokens)
            var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
            assert len(var_tokens) == 1, f"%{i} should be an IDENTIFIER"
            assert var_tokens[0].value == f'%{i}'

    def test_double_percent_for_var(self):
        """Test %%a (FOR loop variable)."""
        lexer = BatchLexer()
        lexer.lex(None, '%%a')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '%%a'

    def test_delayed_expansion(self):
        """Test !VAR! delayed expansion variables."""
        lexer = BatchLexer()
        lexer.lex(None, '!MYVAR!')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '!MYVAR!'

    def test_variable_in_echo(self):
        """Test variable in echo context."""
        lexer = BatchLexer()
        lexer.lex(None, 'echo %USERNAME%')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert any(t.value == '%USERNAME%' for t in var_tokens)
