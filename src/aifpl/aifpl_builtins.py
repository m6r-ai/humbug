"""Unified builtin function registry for AIFPL.

This module provides a single source of truth for all builtin function implementations,
used by both the tree-walking evaluator and the bytecode VM.
"""

from typing import List, Dict, Callable

from aifpl.aifpl_collections import AIFPLCollectionsFunctions
from aifpl.aifpl_math import AIFPLMathFunctions
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLFunction,
    AIFPLInteger, AIFPLFloat, AIFPLComplex
)


# Helper functions for numeric type compatibility during Phase 1 migration

def is_numeric_type(value: AIFPLValue) -> bool:
    """
    Check if value is any numeric type (old or new).

    Returns True for AIFPLInteger, AIFPLFloat, or AIFPLComplex.
    """
    return isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex))

class AIFPLBuiltinRegistry:
    """
    Central registry for all builtin functions.

    This class aggregates builtin implementations from various modules and provides
    a unified interface for both the evaluator and VM to access them.
    """

    def __init__(self, floating_point_tolerance: float = 1e-10):
        """
        Initialize the builtin registry.

        Args:
            floating_point_tolerance: Tolerance for floating point operations
        """
        self.floating_point_tolerance = floating_point_tolerance

        # Create function modules
        self.math_functions = AIFPLMathFunctions(floating_point_tolerance)
        self.collections_functions = AIFPLCollectionsFunctions()

        # Build the registry
        self._registry: Dict[str, Callable] = {}
        self._build_registry()

    def _build_registry(self) -> None:
        """Build the complete registry of builtin functions."""
        # Add math functions
        self._registry.update(self.math_functions.get_functions())

        # Add collections functions
        self._registry.update(self.collections_functions.get_functions())

    def get_function(self, name: str) -> Callable:
        """
        Get a builtin function implementation by name.

        Args:
            name: Function name

        Returns:
            Function implementation callable

        Raises:
            KeyError: If function name is not found
        """
        return self._registry[name]

    def has_function(self, name: str) -> bool:
        """
        Check if a builtin function exists.

        Args:
            name: Function name

        Returns:
            True if function exists, False otherwise
        """
        return name in self._registry

    def get_all_names(self) -> List[str]:
        """
        Get list of all builtin function names.

        Returns:
            List of function names
        """
        return list(self._registry.keys())

    def create_builtin_function_objects(self) -> Dict[str, AIFPLFunction]:
        """
        Create AIFPLFunction objects for all builtins.

        This is used by the evaluator to populate the global environment.

        Builtins are represented as AIFPLFunction objects with:
        - native_impl set to the Python implementation
        - is_variadic=True for functions that accept any number of args
        - parameters tuple indicating arity (or ('args',) for variadic)

        Returns:
            Dictionary mapping function names to AIFPLFunction objects
        """
        builtins = {}
        for name, impl in self._registry.items():
            # Determine if function is variadic and its parameters
            # For now, we'll use introspection or a simple heuristic
            # Most builtins are variadic (like +, *, list, etc.)
            # We'll mark specific ones as non-variadic based on their names
            is_variadic = self._is_variadic_builtin(name)
            parameters = self._get_builtin_parameters(name, is_variadic)

            builtins[name] = AIFPLFunction(
                parameters=parameters,
                native_impl=impl,
                name=name,
                is_variadic=is_variadic
            )

        return builtins

    def call_builtin(self, name: str, args: List[AIFPLValue]) -> AIFPLValue:
        """Call a builtin function by name.

        Args:
            name: Function name
            args: Already-evaluated arguments

        Returns:
            Function result

        Raises:
            KeyError: If function name is not found
        """
        func = self.get_function(name)
        return func(args)

    def _is_variadic_builtin(self, name: str) -> bool:
        """
        Determine if a builtin function is variadic.

        Returns True if the function accepts variable number of arguments.
        """
        # Arithmetic operations are variadic
        if name in ['+', '-', '*', '/', '//', '%', '**', 'min', 'max']:
            return True

        # List construction is variadic
        if name in ['list', 'append']:
            return True

        # String operations that are variadic
        if name in ['string-append', 'string-join']:
            return True

        # Bitwise operations are variadic
        if name in ['bit-or', 'bit-and', 'bit-xor']:
            return True

        # Most other functions have fixed arity
        return False

    def _get_builtin_parameters(self, name: str, is_variadic: bool) -> tuple:
        """
        Get parameter tuple for a builtin function.

        For variadic functions, returns ('args',) to indicate rest parameter.
        For fixed-arity functions, returns appropriate parameter names.
        """
        if is_variadic:
            # Variadic functions use a single rest parameter
            return ('args',)

        # Fixed arity functions - define parameters based on expected arity
        # This is a simplified version; ideally we'd have a complete mapping
        fixed_arity_params = {
            # Unary functions
            'not': ('x',),
            'sqrt': ('x',),
            'abs': ('x',),
            'sin': ('x',), 'cos': ('x',), 'tan': ('x',),
            'log': ('x',), 'log10': ('x',), 'exp': ('x',),
            'round': ('x',), 'floor': ('x',), 'ceil': ('x',),
            # Binary functions
            'pow': ('base', 'exponent'),
            # Add more as needed...
        }

        # Default to ('a', 'b') for unknown fixed-arity functions
        return fixed_arity_params.get(name, ('a', 'b'))
