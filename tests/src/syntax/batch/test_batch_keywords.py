"""
Tests for Batch lexer keywords, commands, and operators.
"""
from syntax.batch.batch_lexer import BatchLexer


class TestBatchKeywords:
    """Test Batch keyword tokenization."""

    def test_control_flow_keywords(self):
        """Test control flow keywords (case-insensitive)."""
        for keyword in ['if', 'else', 'for', 'in', 'do', 'goto', 'call', 'exit']:
            lexer = BatchLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            kw_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(kw_tokens) == 1, f"'{keyword}' should be a KEYWORD"
            assert kw_tokens[0].value == keyword

    def test_case_insensitive_keywords(self):
        """Test that keywords work in any case."""
        for keyword in ['IF', 'If', 'iF', 'if', 'FOR', 'For', 'GOTO', 'Goto']:
            lexer = BatchLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            kw_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(kw_tokens) == 1, f"'{keyword}' should be a KEYWORD"
            assert kw_tokens[0].value == keyword

    def test_comparison_operators(self):
        """Test IF comparison keywords."""
        for op in ['equ', 'neq', 'lss', 'leq', 'gtr', 'geq']:
            lexer = BatchLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            kw_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(kw_tokens) == 1, f"'{op}' should be a KEYWORD"

    def test_commands(self):
        """Test built-in command recognition (case-insensitive)."""
        for cmd in ['echo', 'set', 'cd', 'dir', 'copy', 'move', 'del', 'cls',
                     'ECHO', 'SET', 'CD', 'DIR']:
            lexer = BatchLexer()
            lexer.lex(None, cmd)

            tokens = list(lexer._tokens)
            cmd_tokens = [t for t in tokens if t.type.name == 'COMMAND']
            assert len(cmd_tokens) == 1, f"'{cmd}' should be a COMMAND"
            assert cmd_tokens[0].value == cmd

    def test_at_prefix(self):
        """Test @ prefix operator."""
        lexer = BatchLexer()
        lexer.lex(None, '@echo off')

        tokens = list(lexer._tokens)
        at_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '@']
        assert len(at_tokens) == 1

    def test_command_line_switches(self):
        """Test /S style command-line switches."""
        for switch in ['/S', '/Q', '/F']:
            lexer = BatchLexer()
            lexer.lex(None, switch)

            tokens = list(lexer._tokens)
            opt_tokens = [t for t in tokens if t.type.name == 'OPTION']
            assert len(opt_tokens) == 1, f"'{switch}' should be an OPTION"
            assert opt_tokens[0].value == switch


class TestBatchOperators:
    """Test Batch operator tokenization."""

    def test_redirect_output(self):
        """Test > redirect."""
        lexer = BatchLexer()
        lexer.lex(None, '>')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '>'

    def test_redirect_append(self):
        """Test >> redirect."""
        lexer = BatchLexer()
        lexer.lex(None, '>>')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '>>'

    def test_pipe(self):
        """Test | pipe."""
        lexer = BatchLexer()
        lexer.lex(None, '|')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '|'

    def test_conditional_and(self):
        """Test && operator."""
        lexer = BatchLexer()
        lexer.lex(None, '&&')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '&&'

    def test_assignment(self):
        """Test = assignment."""
        lexer = BatchLexer()
        lexer.lex(None, 'VAR=value')

        tokens = list(lexer._tokens)
        op_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert any(op.value == '=' for op in op_tokens)


class TestBatchStrings:
    """Test Batch string tokenization."""

    def test_double_quoted_string(self):
        """Test double-quoted strings."""
        lexer = BatchLexer()
        lexer.lex(None, '"hello world"')

        tokens = list(lexer._tokens)
        str_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(str_tokens) == 1
        assert str_tokens[0].value == '"hello world"'

    def test_string_in_echo(self):
        """Test string in echo context."""
        lexer = BatchLexer()
        lexer.lex(None, 'echo "hello"')

        tokens = list(lexer._tokens)
        str_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(str_tokens) == 1
        assert str_tokens[0].value == '"hello"'

    def test_unterminated_string(self):
        """Test that unterminated strings consume to end of line."""
        lexer = BatchLexer()
        lexer.lex(None, '"unterminated')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
