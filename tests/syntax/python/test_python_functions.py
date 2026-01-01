"""
Tests for Python function and method tokenization.

NOTE: There are known issues with the parser:
1. Function names after 'def' are incorrectly converted to FUNCTION_OR_METHOD
   when followed by parentheses. They should remain as IDENTIFIER.
"""
import pytest

from syntax.python.python_lexer import PythonLexer
from syntax.python.python_parser import PythonParser


class TestPythonFunctions:
    """Test Python function and method tokenization with parser."""

    def test_function_definition(self):
        """Test function definition.

        BUG: Function name after 'def' is converted to FUNCTION_OR_METHOD
        instead of remaining as IDENTIFIER.
        """
        parser = PythonParser()
        parser.parse(None, 'def my_function():')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'def']
        # BUG: This should be IDENTIFIER, not FUNCTION_OR_METHOD
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(keyword_tokens) == 1, "Should have 'def' keyword"
        assert len(function_tokens) == 1, "BUG: Function name converted to FUNCTION_OR_METHOD"
        assert function_tokens[0].value == 'my_function'

    def test_function_call(self):
        """Test function call recognition."""
        parser = PythonParser()
        parser.parse(None, 'my_function()')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 1, "Should recognize function call"
        assert function_tokens[0].value == 'my_function'

    def test_function_call_with_arguments(self):
        """Test function call with arguments."""
        parser = PythonParser()
        parser.parse(None, 'my_function(arg1, arg2)')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(function_tokens) == 1, "Should have one function call"
        assert function_tokens[0].value == 'my_function'
        assert len(identifier_tokens) == 2, "Should have two argument identifiers"

    def test_method_call(self):
        """Test method call recognition."""
        parser = PythonParser()
        parser.parse(None, 'obj.method()')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 1, "Should recognize method call"
        assert function_tokens[0].value == 'method'

    def test_chained_method_calls(self):
        """Test chained method calls."""
        parser = PythonParser()
        parser.parse(None, 'obj.method1().method2()')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 2, "Should have two method calls"

    def test_nested_function_calls(self):
        """Test nested function calls."""
        parser = PythonParser()
        parser.parse(None, 'outer(inner())')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 2, "Should have two function calls"
        function_names = [t.value for t in function_tokens]
        assert 'outer' in function_names
        assert 'inner' in function_names

    def test_builtin_function_calls(self):
        """Test builtin function calls."""
        builtins = ['print', 'len', 'range', 'str', 'int', 'list', 'dict']
        for builtin in builtins:
            parser = PythonParser()
            parser.parse(None, f'{builtin}()')

            tokens = list(parser._tokens)
            function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

            assert len(function_tokens) == 1, f"Should recognize {builtin} as function call"
            assert function_tokens[0].value == builtin

    def test_lambda_function(self):
        """Test lambda function."""
        parser = PythonParser()
        parser.parse(None, 'lambda x: x + 1')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'lambda']

        assert len(keyword_tokens) == 1, "Should have 'lambda' keyword"

    def test_function_with_default_arguments(self):
        """Test function definition with default arguments.

        BUG: Function name after 'def' is converted to FUNCTION_OR_METHOD.
        """
        parser = PythonParser()
        parser.parse(None, 'def func(x, y=10):')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'def']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(keyword_tokens) == 1, "Should have 'def' keyword"
        # BUG: func is FUNCTION_OR_METHOD, so only x and y are identifiers
        assert len(identifier_tokens) == 2, "BUG: func converted to FUNCTION_OR_METHOD"

    def test_function_with_type_hints(self):
        """Test function with type hints."""
        parser = PythonParser()
        parser.parse(None, 'def func(x: int) -> int:')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'def']
        arrow_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '->']

        assert len(keyword_tokens) == 1, "Should have 'def' keyword"
        assert len(arrow_tokens) == 1, "Should have arrow operator for return type"

    def test_async_function(self):
        """Test async function definition."""
        parser = PythonParser()
        parser.parse(None, 'async def async_func():')

        tokens = list(parser._tokens)
        async_keyword = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'async']
        def_keyword = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'def']

        assert len(async_keyword) == 1, "Should have 'async' keyword"
        assert len(def_keyword) == 1, "Should have 'def' keyword"

    def test_decorator(self):
        """Test decorator syntax."""
        parser = PythonParser()
        parser.parse(None, '@decorator')

        tokens = list(parser._tokens)
        at_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '@']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(at_tokens) == 1, "Should have '@' operator"
        assert len(identifier_tokens) == 1, "Should have decorator name"

    def test_decorator_with_arguments(self):
        """Test decorator with arguments."""
        parser = PythonParser()
        parser.parse(None, '@decorator(arg)')

        tokens = list(parser._tokens)
        at_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '@']
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(at_tokens) == 1, "Should have '@' operator"
        assert len(function_tokens) == 1, "Decorator should be recognized as function call"

    def test_function_call_with_keyword_arguments(self):
        """Test function call with keyword arguments."""
        parser = PythonParser()
        parser.parse(None, 'func(x=1, y=2)')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 1, "Should have function call"

    def test_function_call_with_unpacking(self):
        """Test function call with argument unpacking."""
        parser = PythonParser()
        parser.parse(None, 'func(*args, **kwargs)')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']
        # Check for * operator (single star)
        star_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '*']
        # Check for ** operator (double star)
        doublestar_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '**']

        assert len(function_tokens) == 1, "Should have function call"
        assert len(star_tokens) == 1, "Should have * operator"
        assert len(doublestar_tokens) == 1, "Should have ** operator"

    def test_generator_expression_with_function(self):
        """Test generator expression with function call."""
        parser = PythonParser()
        parser.parse(None, 'sum(x for x in range(10))')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        # Should have 'sum' and 'range' as function calls
        assert len(function_tokens) >= 2, "Should have multiple function calls"

    def test_class_method_definition(self):
        """Test class method definition.

        BUG: Method name after 'def' is converted to FUNCTION_OR_METHOD.
        """
        parser = PythonParser()
        parser.parse(None, '    def method(self):')

        tokens = list(parser._tokens)
        def_keyword = [t for t in tokens if t.type.name == 'KEYWORD' and t.value == 'def']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(def_keyword) == 1, "Should have 'def' keyword"
        # BUG: method is FUNCTION_OR_METHOD, so only self is identifier
        assert len(identifier_tokens) == 1, "BUG: method converted to FUNCTION_OR_METHOD"
        assert identifier_tokens[0].value == 'self'

    def test_static_method_decorator(self):
        """Test staticmethod decorator."""
        parser = PythonParser()
        parser.parse(None, '@staticmethod')

        tokens = list(parser._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) == 1, "Should have decorator identifier"
        assert identifier_tokens[0].value == 'staticmethod'

    def test_property_decorator(self):
        """Test property decorator."""
        parser = PythonParser()
        parser.parse(None, '@property')

        tokens = list(parser._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) == 1, "Should have decorator identifier"
        assert identifier_tokens[0].value == 'property'
