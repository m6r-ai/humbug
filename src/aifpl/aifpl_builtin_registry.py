"""
Unified builtin function registry for AIFPL.

This module provides a single source of truth for all builtin function implementations,
used by the bytecode VM.
"""

from typing import Dict, List, Callable, Optional, Tuple

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

    # Authoritative arity table for all builtins.
    #
    # Each entry is (min_args, max_args) where max_args is None for truly
    # variadic functions (no upper bound).  Functions with fixed arity
    # have min_args == max_args.
    #
    # This is the single source of truth consumed by the semantic analyzer
    # for early arity checking, and by create_builtin_function_objects() for
    # building AIFPLFunction metadata.
    ARITY_TABLE: Dict[str, Tuple[int, Optional[int]]] = {
        # Arithmetic
        '+': (0, None),
        '-': (1, None),
        '*': (0, None),
        '/': (2, None),
        '//': (2, 2),
        '%': (2, 2),
        '**': (2, 2),
        '=': (2, None),
        '!=': (2, None),
        '<': (2, None),
        '>': (2, None),
        '<=': (2, None),
        '>=': (2, None),
        'not': (1, 1),
        'bit-or': (2, None),
        'bit-and': (2, None),
        'bit-xor': (2, None),
        'bit-not': (1, 1),
        'bit-shift-left': (2, 2),
        'bit-shift-right': (2, 2),
        'list': (0, None),
        'cons': (2, 2),
        'append': (2, None),
        'reverse': (1, 1),
        'first': (1, 1),
        'rest': (1, 1),
        'length': (1, 1),
        'last': (1, 1),
        'member?': (2, 2),
        'null?': (1, 1),
        'position': (2, 2),
        'take': (2, 2),
        'drop': (2, 2),
        'remove': (2, 2),
        'list-ref': (2, 2),
        'number?': (1, 1),
        'integer?': (1, 1),
        'float?': (1, 1),
        'complex?': (1, 1),
        'string?': (1, 1),
        'boolean?': (1, 1),
        'list?': (1, 1),
        'alist?': (1, 1),
        'function?': (1, 1),
        'integer': (1, 1),
        'float': (1, 1),
        'range': (2, 3),
        'string-append': (0, None),
        'string-length': (1, 1),
        'string-upcase': (1, 1),
        'string-downcase': (1, 1),
        'string-trim': (1, 1),
        'string-replace': (3, 3),
        'string-split': (2, 2),
        'string-join': (2, 2),
        'string-contains?': (2, 2),
        'string-prefix?': (2, 2),
        'string-suffix?': (2, 2),
        'string-ref': (2, 2),
        'substring': (3, 3),
        'string->number': (1, 1),
        'number->string': (1, 1),
        'string=?': (2, None),
        'string->list': (1, 1),
        'list->string': (1, 1),
        'number=?': (2, None),
        'integer=?': (2, None),
        'float=?': (2, None),
        'complex=?': (2, None),
        'boolean=?': (2, None),
        'list=?': (2, None),
        'alist=?': (2, None),
        'alist': (0, None),
        'alist-get': (2, 3),
        'alist-set': (3, 3),
        'alist-remove': (2, 2),
        'alist-has?': (2, 2),
        'alist-keys': (1, 1),
        'alist-values': (1, 1),
        'alist-merge': (2, 2),
        'alist-length': (1, 1),
        'sqrt': (1, 1),
        'abs': (1, 1),
        'min': (1, None),
        'max': (1, None),
        'pow': (2, 2),
        'sin': (1, 1),
        'cos': (1, 1),
        'tan': (1, 1),
        'log': (1, 1),
        'log10': (1, 1),
        'exp': (1, 1),
        'round': (1, 1),
        'floor': (1, 1),
        'ceil': (1, 1),
        'bin': (1, 1),
        'hex': (1, 1),
        'oct': (1, 1),
        'real': (1, 1),
        'imag': (1, 1),
        'complex': (2, 2),
    }

    def __init__(self) -> None:
        """
        Initialize the builtin registry.
        """

        # Create function modules
        self.builtin_functions = AIFPLBuiltinFunctions()

        # Sanity check: every builtin must have an arity entry
        missing = [name for name in self.BUILTIN_TABLE if name not in self.ARITY_TABLE]
        if missing:
            raise RuntimeError(
                f"Builtins in BUILTIN_TABLE missing from ARITY_TABLE: {missing}"
            )

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
        min_args, max_args = self.ARITY_TABLE[name]
        return max_args is None or min_args != max_args

    def _get_builtin_parameters(self, name: str, is_variadic: bool) -> tuple:
        """
        Get parameter tuple for a builtin function.

        For variadic functions, returns ('args',) to indicate rest parameter.
        For fixed-arity functions, returns appropriate parameter names.
        """
        if is_variadic:
            return ('args',)

        min_args, _ = self.ARITY_TABLE[name]
        return tuple(f'arg{i}' for i in range(min_args))
