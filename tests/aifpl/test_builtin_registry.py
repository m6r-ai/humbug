"""Test the unified builtin registry."""

import pytest
from aifpl.aifpl_builtins import AIFPLBuiltinRegistry
from aifpl.aifpl_value import AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLList


class TestBuiltinRegistry:
    """Test the unified builtin function registry."""

    def test_registry_creation(self):
        """Test that registry can be created."""
        registry = AIFPLBuiltinRegistry()
        assert registry is not None

    def test_registry_has_math_functions(self):
        """Test that math functions are registered."""
        registry = AIFPLBuiltinRegistry()
        assert registry.has_function('+')
        assert registry.has_function('-')
        assert registry.has_function('*')
        assert registry.has_function('/')
        assert registry.has_function('sqrt')
        assert registry.has_function('sin')

    def test_registry_has_collection_functions(self):
        """Test that collection functions are registered."""
        registry = AIFPLBuiltinRegistry()
        assert registry.has_function('list')
        assert registry.has_function('first')
        assert registry.has_function('rest')
        assert registry.has_function('length')
        assert registry.has_function('string-append')

    def test_registry_excludes_special_forms(self):
        """Test that special forms are NOT in the registry."""
        registry = AIFPLBuiltinRegistry()
        # Only and/or require special evaluation (short-circuit)
        assert not registry.has_function('and')
        assert not registry.has_function('or')
        # Higher-order functions are now regular functions in the registry
        assert registry.has_function('map')
        assert registry.has_function('filter')
        assert registry.has_function('fold')

    def test_call_arithmetic_function(self):
        """Test calling an arithmetic function through the registry."""
        registry = AIFPLBuiltinRegistry()

        # Test addition
        args = [AIFPLNumber(1), AIFPLNumber(2), AIFPLNumber(3)]
        result = registry.call_builtin('+', args)
        assert isinstance(result, AIFPLNumber)
        assert result.value == 6

    def test_call_list_function(self):
        """Test calling a list function through the registry."""
        registry = AIFPLBuiltinRegistry()

        # Test list creation
        args = [AIFPLNumber(1), AIFPLNumber(2), AIFPLNumber(3)]
        result = registry.call_builtin('list', args)
        assert isinstance(result, AIFPLList)
        assert len(result.elements) == 3

    def test_call_string_function(self):
        """Test calling a string function through the registry."""
        registry = AIFPLBuiltinRegistry()

        # Test string-append
        args = [AIFPLString("hello"), AIFPLString(" "), AIFPLString("world")]
        result = registry.call_builtin('string-append', args)
        assert isinstance(result, AIFPLString)
        assert result.value == "hello world"

    def test_builtin_function_objects(self):
        """Test creating AIFPLBuiltinFunction objects."""
        registry = AIFPLBuiltinRegistry()
        builtins = registry.create_builtin_function_objects()

        # Check that we got function objects
        assert '+' in builtins
        assert 'sqrt' in builtins
        assert 'list' in builtins

        # Verify they're callable
        from aifpl.aifpl_value import AIFPLFunction
        assert isinstance(builtins['+'], AIFPLFunction)
        assert builtins['+'].is_native  # Should be a native builtin

    def test_error_on_unknown_function(self):
        """Test that calling unknown function raises error."""
        registry = AIFPLBuiltinRegistry()

        with pytest.raises(KeyError):
            registry.call_builtin('nonexistent-function', [])

    def test_get_all_names(self):
        """Test getting all function names."""
        registry = AIFPLBuiltinRegistry()
        names = registry.get_all_names()

        assert '+' in names
        assert 'sqrt' in names
        assert 'list' in names
        assert 'string-append' in names

        # Special forms should NOT be in the list
        assert 'and' not in names
        assert 'or' not in names

        # Higher-order functions are now in the registry
        assert 'map' in names
        assert 'filter' in names
        assert 'fold' in names
