"""
Tests for Bash lexer operators and numbers.
"""
from syntax.bash.bash_lexer import BashLexer


class TestBashOperators:
    """Test Bash operator tokenization."""

    def test_pipe(self):
        """Test pipe operator."""
        lexer = BashLexer()
        lexer.lex(None, '|')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '|'

    def test_logical_and(self):
        """Test && operator."""
        lexer = BashLexer()
        lexer.lex(None, '&&')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '&&'

    def test_logical_or(self):
        """Test || operator."""
        lexer = BashLexer()
        lexer.lex(None, '||')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '||'

    def test_redirect_output(self):
        """Test > redirect operator."""
        lexer = BashLexer()
        lexer.lex(None, '>')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '>'

    def test_redirect_append(self):
        """Test >> redirect operator."""
        lexer = BashLexer()
        lexer.lex(None, '>>')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '>>'

    def test_semicolon(self):
        """Test semicolon operator."""
        lexer = BashLexer()
        lexer.lex(None, ';')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == ';'

    def test_case_terminator(self):
        """Test ;; case terminator."""
        lexer = BashLexer()
        lexer.lex(None, ';;')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == ';;'

    def test_assignment_equals(self):
        """Test = assignment operator."""
        lexer = BashLexer()
        lexer.lex(None, 'VAR=value')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert any(op.value == '=' for op in op_tokens)

    def test_command_pipeline(self):
        """Test a full pipeline."""
        lexer = BashLexer()
        lexer.lex(None, 'cat file | grep pattern')

        tokens = list(lexer._tokens)
        pipe_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '|']
        assert len(pipe_tokens) == 1

    def test_escape_sequence(self):
        """Test backslash escape."""
        lexer = BashLexer()
        lexer.lex(None, r'echo \n')

        tokens = list(lexer._tokens)
        escape_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value.startswith('\\')]
        assert len(escape_tokens) == 1
        assert escape_tokens[0].value == r'\n'


class TestBashNumbers:
    """Test Bash number tokenization."""

    def test_simple_number(self):
        """Test simple decimal numbers."""
        lexer = BashLexer()
        lexer.lex(None, '42')

        tokens = list(lexer._tokens)
        num_tokens = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(num_tokens) == 1
        assert num_tokens[0].value == '42'

    def test_zero(self):
        """Test zero."""
        lexer = BashLexer()
        lexer.lex(None, '0')

        tokens = list(lexer._tokens)
        num_tokens = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(num_tokens) == 1
        assert num_tokens[0].value == '0'

    def test_number_in_context(self):
        """Test number in an argument context."""
        lexer = BashLexer()
        lexer.lex(None, 'exit 1')

        tokens = list(lexer._tokens)
        num_tokens = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(num_tokens) == 1
        assert num_tokens[0].value == '1'
