"""
Tests for Bash lexer variables and dollar constructs.
"""
from syntax.bash.bash_lexer import BashLexer


class TestBashVariables:
    """Test Bash variable tokenization."""

    def test_simple_variable(self):
        """Test simple $VAR references."""
        lexer = BashLexer()
        lexer.lex(None, '$HOME')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '$HOME'

    def test_braced_variable(self):
        """Test ${VAR} references."""
        lexer = BashLexer()
        lexer.lex(None, '${HOME}/bin')

        tokens = list(lexer._tokens)
        home_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER' and t.value == '${HOME}']
        assert len(home_tokens) == 1
        assert home_tokens[0].value == '${HOME}'

    def test_special_parameter_question(self):
        """Test $? (exit code)."""
        lexer = BashLexer()
        lexer.lex(None, '$?')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '$?'

    def test_special_parameter_at(self):
        """Test $@ (all arguments)."""
        lexer = BashLexer()
        lexer.lex(None, '$@')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '$@'

    def test_special_parameter_hash(self):
        """Test $# (argument count)."""
        lexer = BashLexer()
        lexer.lex(None, '$#')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '$#'

    def test_special_parameter_dollar(self):
        """Test $$ (process ID)."""
        lexer = BashLexer()
        lexer.lex(None, '$$')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(var_tokens) == 1
        assert var_tokens[0].value == '$$'

    def test_dollar_command_substitution(self):
        """Test $(...) command substitution."""
        lexer = BashLexer()
        lexer.lex(None, '$(echo hello)')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 1
        assert string_tokens[0].value == '$(echo hello)'

    def test_dollar_arithmetic(self):
        """Test $((...)) arithmetic expansion."""
        lexer = BashLexer()
        lexer.lex(None, '$((1 + 2))')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 1
        assert string_tokens[0].value == '$((1 + 2))'

    def test_bare_dollar(self):
        """Test a bare $ with no following identifier."""
        lexer = BashLexer()
        lexer.lex(None, 'echo $')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 2

    def test_variable_in_assignment(self):
        """Test variable in assignment context."""
        lexer = BashLexer()
        lexer.lex(None, 'PATH=$PATH:/usr/bin')

        tokens = list(lexer._tokens)
        var_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert any(t.value == '$PATH' for t in var_tokens)
