"""
Tests for real-world Python code patterns.
"""
import pytest

from syntax.python.python_lexer import PythonLexer
from syntax.python.python_parser import PythonParser


class TestPythonRealWorld:
    """Test real-world Python code patterns."""

    def test_import_statement(self):
        """Test various import statements."""
        import_statements = [
            'import os',
            'import sys',
            'from typing import List',
            'from . import module',
            'from ..package import module',
            'import numpy as np',
        ]

        for stmt in import_statements:
            parser = PythonParser()
            parser.parse(None, stmt)

            tokens = list(parser._tokens)
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 1, f"Import statement should have keywords: {stmt}"

    def test_list_comprehension(self):
        """Test list comprehension."""
        parser = PythonParser()
        parser.parse(None, '[x for x in range(10)]')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'for' in keyword_values
        assert 'in' in keyword_values

    def test_dict_comprehension(self):
        """Test dictionary comprehension."""
        parser = PythonParser()
        parser.parse(None, '{k: v for k, v in items}')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'for' in keyword_values
        assert 'in' in keyword_values

    def test_set_comprehension(self):
        """Test set comprehension."""
        parser = PythonParser()
        parser.parse(None, '{x for x in range(10)}')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'for' in keyword_values
        assert 'in' in keyword_values

    def test_generator_expression(self):
        """Test generator expression."""
        parser = PythonParser()
        parser.parse(None, '(x for x in range(10))')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'for' in keyword_values
        assert 'in' in keyword_values

    def test_comprehension_with_condition(self):
        """Test comprehension with if condition."""
        parser = PythonParser()
        parser.parse(None, '[x for x in range(10) if x % 2 == 0]')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'for' in keyword_values
        assert 'in' in keyword_values
        assert 'if' in keyword_values

    def test_f_string(self):
        """Test f-string formatting."""
        parser = PythonParser()
        parser.parse(None, 'f"Hello {name}"')

        tokens = list(parser._tokens)
        # f is an identifier, followed by string
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER' and t.value == 'f']
        string_tokens = [t for t in tokens if t.type.name == 'STRING']

        assert len(identifier_tokens) == 1, "Should have 'f' prefix"
        assert len(string_tokens) == 1, "Should have string"

    def test_raw_string(self):
        """Test raw string."""
        parser = PythonParser()
        parser.parse(None, r'r"\n\t"')

        tokens = list(parser._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER' and t.value == 'r']
        string_tokens = [t for t in tokens if t.type.name == 'STRING']

        assert len(identifier_tokens) == 1, "Should have 'r' prefix"
        assert len(string_tokens) == 1, "Should have string"

    def test_multiline_string_literal(self):
        """Test multiline string literal (docstring)."""
        lexer = PythonLexer()
        lexer.lex(None, '"""This is a docstring"""')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_dictionary_literal(self):
        """Test dictionary literal."""
        parser = PythonParser()
        parser.parse(None, '{"key": "value", "num": 42}')

        tokens = list(parser._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']

        assert len(string_tokens) >= 2, "Should have string keys and values"
        assert len(number_tokens) == 1, "Should have number value"

    def test_list_literal(self):
        """Test list literal."""
        parser = PythonParser()
        parser.parse(None, '[1, 2, 3, 4, 5]')

        tokens = list(parser._tokens)
        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']

        assert len(number_tokens) == 5, "Should have 5 numbers"

    def test_tuple_literal(self):
        """Test tuple literal."""
        parser = PythonParser()
        parser.parse(None, '(1, 2, 3)')

        tokens = list(parser._tokens)
        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']

        assert len(number_tokens) == 3, "Should have 3 numbers"

    def test_set_literal(self):
        """Test set literal."""
        parser = PythonParser()
        parser.parse(None, '{1, 2, 3}')

        tokens = list(parser._tokens)
        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']

        assert len(number_tokens) == 3, "Should have 3 numbers"

    def test_slice_notation(self):
        """Test slice notation."""
        parser = PythonParser()
        parser.parse(None, 'arr[1:10:2]')

        tokens = list(parser._tokens)
        colon_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ':']

        assert len(colon_tokens) == 2, "Should have 2 colons in slice"

    def test_unpacking_assignment(self):
        """Test unpacking assignment."""
        parser = PythonParser()
        parser.parse(None, 'a, b, c = 1, 2, 3')

        tokens = list(parser._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) == 3, "Should have 3 identifiers"

    def test_star_unpacking(self):
        """Test star unpacking."""
        parser = PythonParser()
        parser.parse(None, 'first, *rest = items')

        tokens = list(parser._tokens)
        star_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '*']

        assert len(star_tokens) == 1, "Should have star operator"

    def test_kwargs_unpacking(self):
        """Test kwargs unpacking."""
        parser = PythonParser()
        parser.parse(None, 'func(**kwargs)')

        tokens = list(parser._tokens)
        # Check for ** operator specifically
        doublestar_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '**']

        assert len(doublestar_tokens) == 1, "Should have ** operator"

    def test_lambda_expression(self):
        """Test lambda expression."""
        parser = PythonParser()
        parser.parse(None, 'lambda x, y: x + y')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']

        assert any(t.value == 'lambda' for t in keyword_tokens)

    def test_decorator_chain(self):
        """Test multiple decorators."""
        decorators = [
            '@decorator1',
            '@decorator2',
        ]

        for dec in decorators:
            parser = PythonParser()
            parser.parse(None, dec)

            tokens = list(parser._tokens)
            at_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '@']

            assert len(at_tokens) == 1, "Should have @ operator"

    def test_context_manager(self):
        """Test context manager usage."""
        parser = PythonParser()
        parser.parse(None, 'with open("file.txt") as f:')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'with' in keyword_values
        assert 'as' in keyword_values

    def test_exception_handling(self):
        """Test exception handling."""
        lines = [
            'try:',
            'except ValueError as e:',
            'finally:',
        ]

        for line in lines:
            parser = PythonParser()
            parser.parse(None, line)

            tokens = list(parser._tokens)
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 1, f"Should have keywords in: {line}"

    def test_type_annotation(self):
        """Test type annotations."""
        parser = PythonParser()
        parser.parse(None, 'def func(x: int, y: str) -> bool:')

        tokens = list(parser._tokens)
        arrow_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '->']

        assert len(arrow_tokens) == 1, "Should have return type arrow"

    def test_walrus_operator_usage(self):
        """Test walrus operator in real context."""
        parser = PythonParser()
        parser.parse(None, 'if (n := len(items)) > 10:')

        tokens = list(parser._tokens)
        walrus_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ':=']

        assert len(walrus_tokens) == 1, "Should have walrus operator"

    def test_async_function_definition(self):
        """Test async function definition."""
        parser = PythonParser()
        parser.parse(None, 'async def fetch_data():')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'async' in keyword_values
        assert 'def' in keyword_values

    def test_await_expression(self):
        """Test await expression."""
        parser = PythonParser()
        parser.parse(None, 'result = await fetch_data()')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']

        assert any(t.value == 'await' for t in keyword_tokens)

    def test_complex_expression(self):
        """Test complex mathematical expression."""
        parser = PythonParser()
        parser.parse(None, 'result = (a + b) * c / d ** 2')

        tokens = list(parser._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']

        # Should have various operators
        assert len(operator_tokens) >= 5, "Should have multiple operators"

    def test_boolean_expression(self):
        """Test boolean expression with multiple operators."""
        parser = PythonParser()
        parser.parse(None, 'if x > 0 and y < 10 or z == 5:')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'if' in keyword_values
        assert 'and' in keyword_values
        assert 'or' in keyword_values

    def test_membership_test(self):
        """Test membership test operators."""
        parser = PythonParser()
        parser.parse(None, 'if item in collection:')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'if' in keyword_values
        assert 'in' in keyword_values

    def test_identity_test(self):
        """Test identity test operators."""
        parser = PythonParser()
        parser.parse(None, 'if obj is None:')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'if' in keyword_values
        assert 'is' in keyword_values
        assert 'None' in keyword_values

    def test_not_in_operator(self):
        """Test 'not in' operator."""
        parser = PythonParser()
        parser.parse(None, 'if item not in collection:')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'not' in keyword_values
        assert 'in' in keyword_values

    def test_is_not_operator(self):
        """Test 'is not' operator."""
        parser = PythonParser()
        parser.parse(None, 'if obj is not None:')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'is' in keyword_values
        assert 'not' in keyword_values
        assert 'None' in keyword_values
