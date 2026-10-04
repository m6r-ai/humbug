"""
Tests for Python class and object-oriented programming tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer
from syntax.python.python_parser import PythonParser


class TestPythonClasses:
    """Test Python class and OOP tokenization."""

    def test_class_definition(self):
        """Test class definition."""
        lexer = PythonLexer()
        lexer.lex(None, 'class MyClass:')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(keyword_tokens) == 1, "Should have 'class' keyword"
        assert keyword_tokens[0].value == 'class'
        assert len(identifier_tokens) == 1, "Should have class name"
        assert identifier_tokens[0].value == 'MyClass'

    def test_class_with_inheritance(self):
        """Test class with inheritance."""
        lexer = PythonLexer()
        lexer.lex(None, 'class Child(Parent):')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(keyword_tokens) == 1, "Should have 'class' keyword"
        assert len(identifier_tokens) == 2, "Should have Child and Parent identifiers"

    def test_class_with_multiple_inheritance(self):
        """Test class with multiple inheritance."""
        lexer = PythonLexer()
        lexer.lex(None, 'class Child(Parent1, Parent2):')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(keyword_tokens) == 1, "Should have 'class' keyword"
        assert len(identifier_tokens) == 3, "Should have Child, Parent1, Parent2"

    def test_init_method(self):
        """Test __init__ method definition."""
        lexer = PythonLexer()
        lexer.lex(None, '    def __init__(self):')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(keyword_tokens) == 1, "Should have 'def' keyword"
        # Should have __init__ and self
        assert len(identifier_tokens) >= 2

    def test_self_parameter(self):
        """Test self parameter in methods."""
        lexer = PythonLexer()
        lexer.lex(None, '    def method(self, arg):')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        identifier_values = [t.value for t in identifier_tokens]

        assert 'self' in identifier_values
        assert 'method' in identifier_values
        assert 'arg' in identifier_values

    def test_attribute_access(self):
        """Test attribute access with parser."""
        parser = PythonParser()
        parser.parse(None, 'self.attribute')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type.name == 'ELEMENT']

        assert len(element_tokens) == 1, "Should have element token for attribute"
        assert element_tokens[0].value == 'attribute'

    def test_method_call_on_object(self):
        """Test method call on object."""
        parser = PythonParser()
        parser.parse(None, 'obj.method()')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 1, "Should recognize method call"
        assert function_tokens[0].value == 'method'

    def test_chained_attribute_access(self):
        """Test chained attribute access."""
        parser = PythonParser()
        parser.parse(None, 'obj.attr1.attr2')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type.name == 'ELEMENT']

        # Both attr1 and attr2 should be elements
        assert len(element_tokens) >= 2, "Should have multiple element tokens"

    def test_class_variable_assignment(self):
        """Test class variable assignment."""
        lexer = PythonLexer()
        lexer.lex(None, '    class_var = 42')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) == 1, "Should have class_var identifier"

    def test_instance_variable_assignment(self):
        """Test instance variable assignment."""
        parser = PythonParser()
        parser.parse(None, '        self.instance_var = 42')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type.name == 'ELEMENT']

        assert len(element_tokens) == 1, "Should have element for instance_var"

    def test_property_decorator(self):
        """Test @property decorator."""
        parser = PythonParser()
        parser.parse(None, '    @property')

        tokens = list(parser._tokens)
        at_operator = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '@']
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(at_operator) == 1, "Should have @ operator"
        assert len(identifier_tokens) == 1, "Should have property identifier"

    def test_staticmethod_decorator(self):
        """Test @staticmethod decorator."""
        parser = PythonParser()
        parser.parse(None, '    @staticmethod')

        tokens = list(parser._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) == 1
        assert identifier_tokens[0].value == 'staticmethod'

    def test_classmethod_decorator(self):
        """Test @classmethod decorator."""
        parser = PythonParser()
        parser.parse(None, '    @classmethod')

        tokens = list(parser._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) == 1
        assert identifier_tokens[0].value == 'classmethod'

    def test_cls_parameter(self):
        """Test cls parameter in classmethods."""
        lexer = PythonLexer()
        lexer.lex(None, '    def method(cls):')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        identifier_values = [t.value for t in identifier_tokens]

        assert 'cls' in identifier_values

    def test_super_call(self):
        """Test super() call."""
        parser = PythonParser()
        parser.parse(None, 'super().__init__()')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']
        function_values = [t.value for t in function_tokens]

        assert 'super' in function_values
        assert '__init__' in function_values

    def test_dunder_methods(self):
        """Test various dunder (magic) methods."""
        dunder_methods = [
            '__init__', '__str__', '__repr__', '__len__',
            '__getitem__', '__setitem__', '__call__', '__enter__', '__exit__'
        ]

        for method in dunder_methods:
            lexer = PythonLexer()
            lexer.lex(None, f'    def {method}(self):')

            tokens = list(lexer._tokens)
            identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
            identifier_values = [t.value for t in identifier_tokens]

            assert method in identifier_values, f"Should recognize {method}"

    def test_private_attribute(self):
        """Test private attribute (single underscore)."""
        parser = PythonParser()
        parser.parse(None, 'self._private')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type.name == 'ELEMENT']

        assert len(element_tokens) == 1
        assert element_tokens[0].value == '_private'

    def test_name_mangled_attribute(self):
        """Test name-mangled attribute (double underscore)."""
        parser = PythonParser()
        parser.parse(None, 'self.__private')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type.name == 'ELEMENT']

        assert len(element_tokens) == 1
        assert element_tokens[0].value == '__private'

    def test_class_instantiation(self):
        """Test class instantiation."""
        parser = PythonParser()
        parser.parse(None, 'obj = MyClass()')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 1, "Class instantiation should be recognized as function call"
        assert function_tokens[0].value == 'MyClass'

    def test_isinstance_check(self):
        """Test isinstance check."""
        parser = PythonParser()
        parser.parse(None, 'isinstance(obj, MyClass)')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 1
        assert function_tokens[0].value == 'isinstance'

    def test_issubclass_check(self):
        """Test issubclass check."""
        parser = PythonParser()
        parser.parse(None, 'issubclass(Child, Parent)')

        tokens = list(parser._tokens)
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']

        assert len(function_tokens) == 1
        assert function_tokens[0].value == 'issubclass'

    def test_abstract_base_class(self):
        """Test abstract base class import."""
        parser = PythonParser()
        parser.parse(None, 'from abc import ABC')

        tokens = list(parser._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]

        assert 'from' in keyword_values
        assert 'import' in keyword_values

    def test_dataclass_decorator(self):
        """Test @dataclass decorator."""
        parser = PythonParser()
        parser.parse(None, '@dataclass')

        tokens = list(parser._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) == 1
        assert identifier_tokens[0].value == 'dataclass'

    def test_type_hints_in_class(self):
        """Test type hints in class definition."""
        lexer = PythonLexer()
        lexer.lex(None, '    name: str')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']

        assert len(identifier_tokens) >= 2, "Should have name and str"
