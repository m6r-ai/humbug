"""
Unified builtin function registry for AIFPL.

This module provides a single source of truth for all builtin function implementations,
used by the bytecode VM.
"""

from typing import List, Dict, Callable

from aifpl.aifpl_builtin_functions import AIFPLBuiltinFunctions
from aifpl.aifpl_value import AIFPLFunction


class AIFPLBuiltinRegistry:
    """
    Central registry for all builtin functions.

    This class aggregates builtin implementations from various modules and provides
    a unified array-based interface for fast VM access.
    """

    # Authoritative list of builtin names mapped to indices (for CALL_BUILTIN)
    # This defines the canonical ordering - all other structures follow this order
    BUILTIN_TABLE = [
        '+', '-', '*', '/', '//', '%', '**',
        '=', '!=', '<', '>', '<=', '>=',
        'not',
        'bit-or', 'bit-and', 'bit-xor', 'bit-not', 'bit-shift-left', 'bit-shift-right',
        'list', 'cons', 'append', 'reverse', 'first', 'rest', 'length', 'last',
        'member?', 'null?', 'position', 'take', 'drop', 'remove', 'list-ref',
        'number?', 'integer?', 'float?', 'complex?', 'string?', 'boolean?', 'list?', 'alist?', 'function?',
        'integer', 'float',
        'range',
        'string-append', 'string-length', 'string-upcase', 'string-downcase',
        'string-trim', 'string-replace', 'string-split', 'string-join',
        'string-contains?', 'string-prefix?', 'string-suffix?', 'string-ref',
        'substring', 'string->number', 'number->string', 'string=?', 'string->list', 'list->string',
        'number=?', 'integer=?', 'float=?', 'complex=?', 'boolean=?', 'list=?', 'alist=?',
        'alist', 'alist-get', 'alist-set', 'alist-remove', 'alist-has?',
        'alist-keys', 'alist-values', 'alist-merge', 'alist-length',
        'sqrt', 'abs', 'min', 'max', 'pow',
        'sin', 'cos', 'tan', 'log', 'log10', 'exp',
        'round', 'floor', 'ceil',
        'bin', 'hex', 'oct', 'real', 'imag', 'complex',
    ]

    def __init__(self) -> None:
        """
        Initialize the builtin registry.
        """

        # Create function modules
        self.builtin_functions = AIFPLBuiltinFunctions()

        # Build function array in BUILTIN_TABLE order for fast VM access
        self._function_array: List[Callable] = self._build_function_array()

    def _build_function_array(self) -> List[Callable]:
        """
        Build array of builtin functions in BUILTIN_TABLE order.

        Returns:
            List of function implementations indexed by BUILTIN_TABLE position
        """
        # First build a temporary dict from all function modules
        functions_dict: Dict[str, Callable] = {}
        functions_dict.update(self.builtin_functions.get_functions())

        # Now build array in BUILTIN_TABLE order
        function_array = []
        for name in self.BUILTIN_TABLE:
            if name not in functions_dict:
                raise RuntimeError(f"Builtin function '{name}' in BUILTIN_TABLE but not implemented")

            function_array.append(functions_dict[name])

        return function_array

    def get_function_array(self) -> List[Callable]:
        """
        Get the builtin function array for VM use.

        Returns:
            List of function implementations indexed by BUILTIN_TABLE position
        """
        return self._function_array

    def create_builtin_function_objects(self) -> Dict[str, AIFPLFunction]:
        """
        Create AIFPLFunction objects for all builtins.

        This is used to populate the global environment with first-class function objects.

        Builtins are represented as AIFPLFunction objects with:
        - native_impl set to the Python implementation
        - is_variadic=True for functions that accept any number of args
        - parameters tuple indicating arity (or ('args',) for variadic)

        Returns:
            Dictionary mapping function names to AIFPLFunction objects
        """
        builtins = {}
        for i, name in enumerate(self.BUILTIN_TABLE):
            impl = self._function_array[i]

            # Determine if function is variadic and its parameters
            is_variadic = self._is_variadic_builtin(name)
            parameters = self._get_builtin_parameters(name, is_variadic)

            builtins[name] = AIFPLFunction(
                parameters=parameters,
                native_impl=impl,
                name=name,
                is_variadic=is_variadic
            )

        return builtins

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
        }

        # Default to ('a', 'b') for unknown fixed-arity functions
        return fixed_arity_params.get(name, ('a', 'b'))
