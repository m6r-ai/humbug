"""
Tests for Python operator tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer


class TestPythonOperators:
    """Test Python operator tokenization."""

    def test_arithmetic_operators(self):
        """Test basic arithmetic operators."""
        operators = ['+', '-', '*', '/', '%', '**', '//']
        for op in operators:
            lexer = PythonLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_comparison_operators(self):
        """Test comparison operators."""
        operators = ['==', '!=', '<', '>', '<=', '>=']
        for op in operators:
            lexer = PythonLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_assignment_operators(self):
        """Test assignment operators."""
        operators = ['=', '+=', '-=', '*=', '/=', '%=', '**=', '//=', '&=', '|=', '^=', '>>=', '<<=']
        for op in operators:
            lexer = PythonLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_bitwise_operators(self):
        """Test bitwise operators."""
        operators = ['&', '|', '^', '~', '<<', '>>']
        for op in operators:
            lexer = PythonLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_walrus_operator(self):
        """Test walrus operator (assignment expression)."""
        lexer = PythonLexer()
        lexer.lex(None, ':=')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Walrus operator should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == ':='

    def test_matrix_multiplication_operator(self):
        """Test matrix multiplication operator."""
        operators = ['@', '@=']
        for op in operators:
            lexer = PythonLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_ellipsis_operator(self):
        """Test ellipsis operator."""
        lexer = PythonLexer()
        lexer.lex(None, '...')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Ellipsis should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '...'

    def test_arrow_operator(self):
        """Test arrow operator (function annotations)."""
        lexer = PythonLexer()
        lexer.lex(None, '->')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Arrow operator should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '->'

    def test_delimiters(self):
        """Test delimiter operators."""
        delimiters = ['(', ')', '[', ']', '{', '}', ',', ':', '.']
        for delim in delimiters:
            lexer = PythonLexer()
            lexer.lex(None, delim)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Delimiter '{delim}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{delim}' should be OPERATOR type"
            assert tokens[0].value == delim

    def test_backslash_line_continuation(self):
        """Test backslash as line continuation."""
        lexer = PythonLexer()
        lexer.lex(None, '\\')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Backslash should produce one token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '\\'

    def test_operators_in_expression(self):
        """Test multiple operators in an expression."""
        lexer = PythonLexer()
        lexer.lex(None, 'x = 1 + 2 * 3')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        operator_values = [t.value for t in operator_tokens]
        assert '=' in operator_values
        assert '+' in operator_values
        assert '*' in operator_values

    def test_operator_precedence_tokens(self):
        """Test that operators are tokenized correctly in precedence contexts."""
        lexer = PythonLexer()
        lexer.lex(None, '2 ** 3 * 4 + 5')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        operator_values = [t.value for t in operator_tokens]
        assert '**' in operator_values
        assert '*' in operator_values
        assert '+' in operator_values

    def test_comparison_chaining(self):
        """Test comparison operator chaining."""
        lexer = PythonLexer()
        lexer.lex(None, '1 < x <= 10')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        operator_values = [t.value for t in operator_tokens]
        assert '<' in operator_values
        assert '<=' in operator_values

    def test_unary_operators(self):
        """Test unary operators."""
        test_cases = [
            ('-x', ['-']),
            ('+x', ['+']),
            ('~x', ['~']),
        ]
        for expr, expected_ops in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, expr)

            tokens = list(lexer._tokens)
            operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
            operator_values = [t.value for t in operator_tokens]
            for op in expected_ops:
                assert op in operator_values, f"Expected operator '{op}' in '{expr}'"

    def test_increment_decrement(self):
        """Test increment/decrement operators (note: not standard in Python but in operator list)."""
        operators = ['++', '--']
        for op in operators:
            lexer = PythonLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Operator '{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_operators_without_spaces(self):
        """Test operators in expressions without spaces."""
        lexer = PythonLexer()
        lexer.lex(None, 'x=1+2*3')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(operator_tokens) == 3, "Should have 3 operators (=, +, *)"

    def test_multiple_assignment_operators(self):
        """Test multiple assignment in one line."""
        lexer = PythonLexer()
        lexer.lex(None, 'x = y = z = 0')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(operator_tokens) == 3, "Should have 3 assignment operators"
        assert all(t.value == '=' for t in operator_tokens)

    def test_slice_notation(self):
        """Test slice notation with colons."""
        lexer = PythonLexer()
        lexer.lex(None, 'arr[1:10:2]')

        tokens = list(lexer._tokens)
        colon_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ':']
        assert len(colon_tokens) == 2, "Should have 2 colon operators in slice"

    def test_dictionary_colon(self):
        """Test colon in dictionary literal."""
        lexer = PythonLexer()
        lexer.lex(None, '{"key": "value"}')

        tokens = list(lexer._tokens)
        colon_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ':']
        assert len(colon_tokens) == 1, "Should have 1 colon operator in dict"

    def test_function_call_parentheses(self):
        """Test parentheses in function calls."""
        lexer = PythonLexer()
        lexer.lex(None, 'func(x, y)')

        tokens = list(lexer._tokens)
        paren_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value in ('(', ')')]
        assert len(paren_tokens) == 2, "Should have 2 parentheses"

    def test_list_brackets(self):
        """Test brackets in list literals."""
        lexer = PythonLexer()
        lexer.lex(None, '[1, 2, 3]')

        tokens = list(lexer._tokens)
        bracket_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value in ('[', ']')]
        assert len(bracket_tokens) == 2, "Should have 2 brackets"

    def test_dict_braces(self):
        """Test braces in dictionary literals."""
        lexer = PythonLexer()
        lexer.lex(None, '{1: 2}')

        tokens = list(lexer._tokens)
        brace_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value in ('{', '}')]
        assert len(brace_tokens) == 2, "Should have 2 braces"

    def test_dot_operator_in_attribute_access(self):
        """Test dot operator in attribute access."""
        lexer = PythonLexer()
        lexer.lex(None, 'obj.method')

        tokens = list(lexer._tokens)
        dot_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '.']
        assert len(dot_tokens) == 1, "Should have 1 dot operator"

    def test_comma_in_tuple(self):
        """Test comma operators in tuple."""
        lexer = PythonLexer()
        lexer.lex(None, '(1, 2, 3)')

        tokens = list(lexer._tokens)
        comma_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ',']
        assert len(comma_tokens) == 2, "Should have 2 comma operators"
