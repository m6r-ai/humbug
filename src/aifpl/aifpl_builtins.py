"""Unified builtin function registry for AIFPL.

This module provides a single source of truth for all builtin function implementations,
used by both the tree-walking evaluator and the bytecode VM.
"""

from typing import List, Dict, Callable, Any
from aifpl.aifpl_value import AIFPLValue, AIFPLBuiltinFunction
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_math import AIFPLMathFunctions
from aifpl.aifpl_collections import AIFPLCollectionsFunctions


class AIFPLBuiltinRegistry:
    """Central registry for all builtin functions.
    
    This class aggregates builtin implementations from various modules and provides
    a unified interface for both the evaluator and VM to access them.
    """
    
    def __init__(self, floating_point_tolerance: float = 1e-10):
        """Initialize the builtin registry.
        
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
        
        # Note: Special forms (and, or, map, filter, fold, range, find, any?, all?, alist)
        # are NOT included here because they require special handling in the evaluator/VM
        # (they need unevaluated arguments or special evaluation logic)
    
    def get_function(self, name: str) -> Callable:
        """Get a builtin function implementation by name.
        
        Args:
            name: Function name
            
        Returns:
            Function implementation callable
            
        Raises:
            KeyError: If function name is not found
        """
        return self._registry[name]
    
    def has_function(self, name: str) -> bool:
        """Check if a builtin function exists.
        
        Args:
            name: Function name
            
        Returns:
            True if function exists, False otherwise
        """
        return name in self._registry
    
    def get_all_names(self) -> List[str]:
        """Get list of all builtin function names.
        
        Returns:
            List of function names
        """
        return list(self._registry.keys())
    
    def create_builtin_function_objects(self) -> Dict[str, AIFPLBuiltinFunction]:
        """Create AIFPLBuiltinFunction objects for all builtins.
        
        This is used by the evaluator to populate the global environment.
        
        Returns:
            Dictionary mapping function names to AIFPLBuiltinFunction objects
        """
        builtins = {}
        for name, impl in self._registry.items():
            builtins[name] = AIFPLBuiltinFunction(name, impl)
        return builtins
    
    def call_builtin(self, name: str, args: List[AIFPLValue], 
                    env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Call a builtin function by name.
        
        Args:
            name: Function name
            args: Already-evaluated arguments
            env: Current environment (may be needed by some functions)
            depth: Current recursion depth
            
        Returns:
            Function result
            
        Raises:
            KeyError: If function name is not found
        """
        func = self.get_function(name)
        return func(args, env, depth)


# Export the standard builtin table order for the compiler
# This must match the order in AIFPLCompiler.BUILTIN_TABLE
BUILTIN_TABLE_ORDER = [
    # Arithmetic
    '+', '-', '*', '/', '//', '%', '**',
    # Comparison
    '=', '!=', '<', '>', '<=', '>=',
    # Boolean
    'and', 'or', 'not',
    # Bitwise
    'bit-or', 'bit-and', 'bit-xor', 'bit-not', 'bit-shift-left', 'bit-shift-right',
    # Lists
    'list', 'cons', 'append', 'reverse', 'first', 'rest', 'length', 'last',
    'member?', 'null?', 'position', 'take', 'drop', 'remove', 'list-ref',
    # Type predicates
    'number?', 'integer?', 'float?', 'complex?', 'string?', 'boolean?', 'list?', 'alist?', 'function?',
    # Higher-order
    'map', 'filter', 'fold', 'range', 'find', 'any?', 'all?',
    # Strings
    'string-append', 'string-length', 'string-upcase', 'string-downcase',
    'string-trim', 'string-replace', 'string-split', 'string-join',
    'string-contains?', 'string-prefix?', 'string-suffix?', 'string-ref',
    'substring', 'string->number', 'number->string', 'string=?', 'string->list', 'list->string',
    # Alists
    'alist', 'alist-get', 'alist-set', 'alist-remove', 'alist-has?',
    'alist-keys', 'alist-values', 'alist-merge', 'alist?',
    # Math
    'sqrt', 'abs', 'min', 'max', 'pow',
    'sin', 'cos', 'tan', 'log', 'log10', 'exp',
    'round', 'floor', 'ceil',
    'bin', 'hex', 'oct', 'real', 'imag', 'complex',
]
