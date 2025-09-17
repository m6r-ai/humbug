"""Evaluator for AIFPL Abstract Syntax Trees."""

import cmath
import math
from typing import Any, Dict, List, Union

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_parser import SExpression


class AIFPLEvaluator:
    """Evaluates AIFPL Abstract Syntax Trees."""

    # Mathematical constants
    CONSTANTS = {
        'pi': math.pi,
        'e': math.e,
        'j': 1j,
        'true': True,
        'false': False,
    }

    # Operator and function definitions
    OPERATORS: Dict[str, Dict[str, Any]] = {
        # Conditional operators
        'if': {'type': 'special', 'args': 3, 'lazy_evaluation': True},

        # Arithmetic operators
        '+': {'type': 'variadic', 'min_args': 0, 'identity': 0},
        '-': {'type': 'variadic', 'min_args': 1},
        '*': {'type': 'variadic', 'min_args': 0, 'identity': 1},
        '/': {'type': 'variadic', 'min_args': 2},
        '//': {'type': 'binary'},
        '%': {'type': 'binary'},
        '**': {'type': 'binary'},

        # Comparison operators
        '=': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '<': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '>': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '<=': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '>=': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},

        # Boolean operators
        'and': {'type': 'variadic', 'min_args': 0, 'identity': True, 'boolean_only': True},
        'or': {'type': 'variadic', 'min_args': 0, 'identity': False, 'boolean_only': True},
        'not': {'type': 'unary', 'boolean_only': True},

        # Bitwise operators
        'bit-or': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-and': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-xor': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-not': {'type': 'unary', 'bitwise': True},
        'bit-shift-left': {'type': 'binary', 'bitwise': True},
        'bit-shift-right': {'type': 'binary', 'bitwise': True},

        # Mathematical functions
        'sin': {'type': 'unary'},
        'cos': {'type': 'unary'},
        'tan': {'type': 'unary'},
        'log': {'type': 'unary'},
        'log10': {'type': 'unary'},
        'exp': {'type': 'unary'},
        'sqrt': {'type': 'unary'},
        'abs': {'type': 'unary'},
        'round': {'type': 'unary', 'real_only': True},
        'floor': {'type': 'unary', 'real_only': True},
        'ceil': {'type': 'unary', 'real_only': True},
        'min': {'type': 'variadic', 'min_args': 1},
        'max': {'type': 'variadic', 'min_args': 1},
        'pow': {'type': 'binary'},

        # Base conversion functions
        'bin': {'type': 'unary', 'integer_only': True, 'returns_string': True},
        'hex': {'type': 'unary', 'integer_only': True, 'returns_string': True},
        'oct': {'type': 'unary', 'integer_only': True, 'returns_string': True},

        # Complex number functions
        'real': {'type': 'unary'},
        'imag': {'type': 'unary'},
        'complex': {'type': 'binary'},

        # String functions
        'string-append': {'type': 'variadic', 'min_args': 0, 'identity': '', 'string_only': True},
        'string-length': {'type': 'unary', 'string_only': True},
        'substring': {'type': 'ternary', 'string_only': True},
        'string-upcase': {'type': 'unary', 'string_only': True},
        'string-downcase': {'type': 'unary', 'string_only': True},
        'string-ref': {'type': 'binary', 'string_only': True},
        'string->number': {'type': 'unary', 'string_only': True},
        'number->string': {'type': 'unary', 'converts_to_string': True},

        # String predicates
        'string-contains?': {'type': 'binary', 'string_only': True, 'returns_boolean': True},
        'string-prefix?': {'type': 'binary', 'string_only': True, 'returns_boolean': True},
        'string-suffix?': {'type': 'binary', 'string_only': True, 'returns_boolean': True},
        'string=?': {'type': 'variadic', 'min_args': 2, 'string_only': True, 'returns_boolean': True},

        # List construction and manipulation
        'list': {'type': 'variadic', 'min_args': 0, 'list_operation': True},
        'cons': {'type': 'binary', 'list_operation': True},
        'append': {'type': 'variadic', 'min_args': 2, 'list_operation': True},
        'reverse': {'type': 'unary', 'list_operation': True},

        # List access and properties
        'first': {'type': 'unary', 'list_operation': True},
        'rest': {'type': 'unary', 'list_operation': True},
        'list-ref': {'type': 'binary', 'list_operation': True},
        'length': {'type': 'unary', 'list_operation': True},

        # List predicates
        'null?': {'type': 'unary', 'list_operation': True, 'returns_boolean': True},
        'list?': {'type': 'unary', 'returns_boolean': True},
        'member?': {'type': 'binary', 'list_operation': True, 'returns_boolean': True},

        # String-list conversion
        'string->list': {'type': 'unary', 'string_only': True, 'returns_list': True},
        'list->string': {'type': 'unary', 'list_operation': True, 'converts_to_string': True},
        'string-split': {'type': 'binary', 'string_only': True, 'returns_list': True},
        'string-join': {'type': 'binary', 'list_operation': True, 'converts_to_string': True},
    }

    # Tolerance for considering imaginary part as zero
    IMAGINARY_TOLERANCE = 1e-10

    def __init__(self, max_depth: int = 100, imaginary_tolerance: float = 1e-10):
        """
        Initialize evaluator.

        Args:
            max_depth: Maximum recursion depth
            imaginary_tolerance: Tolerance for considering imaginary part as zero
        """
        self.max_depth = max_depth
        self.imaginary_tolerance = imaginary_tolerance

    def evaluate(self, expr: SExpression, depth: int = 0) -> Union[int, float, complex, str, bool, list]:
        """
        Recursively evaluate AST.

        Args:
            expr: Expression to evaluate
            depth: Current recursion depth

        Returns:
            Evaluation result

        Raises:
            AIFPLEvalError: If evaluation fails
        """
        if depth > self.max_depth:
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})")

        # Atom evaluation
        if isinstance(expr, (int, float, complex, str, bool)):
            return expr

        if isinstance(expr, str):
            # Symbol lookup (constants)
            if expr in self.CONSTANTS:
                return self.CONSTANTS[expr]

            raise AIFPLEvalError(f"Unknown symbol: '{expr}'")

        # List evaluation
        if isinstance(expr, list):
            if not expr:
                raise AIFPLEvalError("Cannot evaluate empty list")

            operator = expr[0]
            args = expr[1:]

            if not isinstance(operator, str):
                raise AIFPLEvalError(f"Operator must be a symbol, got {type(operator).__name__}")

            return self._apply_operator(operator, args, depth + 1)

        raise AIFPLEvalError(f"Invalid expression type: {type(expr).__name__}")

    def _apply_operator(self, operator: str, args: List[SExpression], depth: int) -> Union[int, float, complex, str, bool, list]:
        """Apply an operator to its arguments."""
        if operator not in self.OPERATORS:
            raise AIFPLEvalError(f"Unknown operator: '{operator}'")

        op_def: Dict[str, Any] = self.OPERATORS[operator]

        # Handle special forms that require lazy evaluation
        if op_def.get('type') == 'special':
            if operator == 'if':
                return self._apply_if_conditional(args, depth)

        # For regular operators, evaluate arguments first
        evaluated_args = [self.evaluate(arg, depth) for arg in args]

        # Check argument count
        self._validate_arity(operator, op_def, evaluated_args)

        # Handle list operations
        if op_def.get('list_operation'):
            return self._apply_list_operator(operator, evaluated_args)

        # Handle special cases that return strings
        if op_def.get('returns_string'):
            return self._apply_string_function(operator, evaluated_args)

        # Handle functions that convert to strings
        if op_def.get('converts_to_string'):
            return self._apply_conversion_to_string(operator, evaluated_args)

        # Handle functions that return lists
        if op_def.get('returns_list'):
            return self._apply_list_returning_function(operator, evaluated_args)

        # Handle boolean-only operations
        if op_def.get('boolean_only'):
            return self._apply_boolean_operator(operator, op_def, evaluated_args)

        # Handle string-only operations
        if op_def.get('string_only'):
            return self._apply_string_operator(operator, op_def, evaluated_args)

        # Handle operations that return booleans
        if op_def.get('returns_boolean'):
            return self._apply_boolean_returning_operator(operator, evaluated_args)

        # Filter out string, boolean, and list arguments for mathematical operations
        for arg in evaluated_args:
            if isinstance(arg, (str, bool, list)):
                raise AIFPLEvalError(f"Operator '{operator}' cannot operate on {type(arg).__name__} arguments")

        # Handle bitwise operations (require integers)
        if op_def.get('bitwise'):
            return self._apply_bitwise_operator(operator, evaluated_args)

        # Handle real-only operations
        if op_def.get('real_only'):
            return self._apply_real_only_function(operator, evaluated_args)

        # Handle integer-only operations
        if op_def.get('integer_only'):
            return self._apply_integer_only_function(operator, evaluated_args)

        # Handle regular mathematical operations
        return self._apply_mathematical_operator(operator, op_def, evaluated_args)

    def _apply_if_conditional(self, args: List[SExpression], depth: int) -> Union[int, float, complex, str, bool, list]:
        """
        Handle if conditional with lazy evaluation of branches.

        Args:
            args: List of unevaluated arguments [condition, then-expr, else-expr]
            depth: Current recursion depth

        Returns:
            Result of evaluating the chosen branch

        Raises:
            AIFPLEvalError: If condition is not boolean or wrong number of arguments
        """
        if len(args) != 3:
            raise AIFPLEvalError(f"Operator 'if' requires exactly 3 arguments (condition, then, else), got {len(args)}")

        condition_expr, then_expr, else_expr = args

        # Evaluate condition first
        condition = self.evaluate(condition_expr, depth)

        # Validate condition is boolean
        if not isinstance(condition, bool):
            raise AIFPLEvalError(f"Operator 'if' requires boolean condition, got {type(condition).__name__}")

        # Lazy evaluation: only evaluate the chosen branch
        if condition:
            return self.evaluate(then_expr, depth)

        return self.evaluate(else_expr, depth)

    def _validate_arity(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> None:
        """Validate argument count for an operator."""
        op_type = op_def['type']
        arg_count = len(args)

        if op_type == 'unary' and arg_count != 1:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 1 argument, got {arg_count}")

        if op_type == 'binary' and arg_count != 2:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 2 arguments, got {arg_count}")

        if op_type == 'ternary' and arg_count != 3:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 3 arguments, got {arg_count}")

        if op_type == 'variadic':
            min_args = op_def.get('min_args', 0)
            if arg_count < min_args:
                raise AIFPLEvalError(f"Operator '{operator}' requires at least {min_args} arguments, got {arg_count}")

    def _apply_list_operator(self, operator: str, args: List[Any]) -> Union[list, bool, Any]:
        """Apply list operations."""
        if operator == 'list':
            # Create a new list from arguments (heterogeneous allowed)
            return list(args)

        if operator == 'cons':
            element, list_arg = args
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list as second argument, got {type(list_arg).__name__}")

            return [element] + list_arg

        if operator == 'append':
            # All arguments must be lists
            for i, arg in enumerate(args):
                if not isinstance(arg, list):
                    raise AIFPLEvalError(
                        f"Operator '{operator}' requires list arguments, got {type(arg).__name__} at position {i}"
                    )

            result = []
            for list_arg in args:
                result.extend(list_arg)

            return result

        if operator == 'reverse':
            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list argument, got {type(list_arg).__name__}")

            return list(reversed(list_arg))

        if operator == 'first':
            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list argument, got {type(list_arg).__name__}")

            if not list_arg:
                raise AIFPLEvalError("Cannot get first element of empty list")

            return list_arg[0]

        if operator == 'rest':
            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list argument, got {type(list_arg).__name__}")

            if not list_arg:
                raise AIFPLEvalError("Cannot get rest of empty list")

            return list_arg[1:]

        if operator == 'list-ref':
            list_arg, index_arg = args

            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list as first argument, got {type(list_arg).__name__}")

            index = self._to_integer(index_arg, operator)

            if index < 0 or index >= len(list_arg):
                raise AIFPLEvalError(f"List index {index} out of bounds for list of length {len(list_arg)}")

            return list_arg[index]

        if operator == 'length':
            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list argument, got {type(list_arg).__name__}")

            return len(list_arg)

        if operator == 'null?':
            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list argument, got {type(list_arg).__name__}")

            return len(list_arg) == 0

        if operator == 'member?':
            element, list_arg = args

            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list as second argument, got {type(list_arg).__name__}")

            return element in list_arg

        if operator == 'list->string':
            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list argument, got {type(list_arg).__name__}")

            # All elements must be strings
            for i, element in enumerate(list_arg):
                if not isinstance(element, str):
                    raise AIFPLEvalError(
                        f"Operator '{operator}' requires list of strings, got {type(element).__name__} at position {i}"
                    )

            return ''.join(list_arg)

        if operator == 'string-join':
            list_arg, separator_arg = args

            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"Operator '{operator}' requires list as first argument, got {type(list_arg).__name__}")

            if not isinstance(separator_arg, str):
                raise AIFPLEvalError(
                    f"Operator '{operator}' requires string as second argument, got {type(separator_arg).__name__}"
                )

            # All list elements must be strings
            for i, element in enumerate(list_arg):
                if not isinstance(element, str):
                    raise AIFPLEvalError(
                        f"Operator '{operator}' requires list of strings, got {type(element).__name__} at position {i}"
                    )

            return separator_arg.join(list_arg)

        raise AIFPLEvalError(f"Unknown list operator: '{operator}'")

    def _apply_list_returning_function(self, operator: str, args: List[Any]) -> list:
        """Apply functions that return lists."""
        if operator == 'string->list':
            string_arg = args[0]
            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string argument, got {type(string_arg).__name__}")

            return list(string_arg)

        if operator == 'string-split':
            string_arg, separator_arg = args

            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as first argument, got {type(string_arg).__name__}")

            if not isinstance(separator_arg, str):
                raise AIFPLEvalError(
                    f"Operator '{operator}' requires string as second argument, got {type(separator_arg).__name__}"
                )

            if not separator_arg:
                raise AIFPLEvalError("String separator cannot be empty")

            return string_arg.split(separator_arg)

        raise AIFPLEvalError(f"Unknown list-returning function: '{operator}'")

    def _apply_string_function(self, operator: str, args: List[Any]) -> str:
        """Apply functions that return strings."""
        arg = args[0]

        # Convert to integer for base conversion
        int_arg = self._to_integer(arg, operator)

        if operator == 'bin':
            return bin(int_arg)

        if operator == 'hex':
            return hex(int_arg)

        if operator == 'oct':
            return oct(int_arg)

        raise AIFPLEvalError(f"Unknown string function: '{operator}'")

    def _apply_conversion_to_string(self, operator: str, args: List[Any]) -> str:
        """Apply functions that convert values to strings."""
        if operator == 'number->string':
            arg = args[0]
            if isinstance(arg, (int, float, complex)):
                return str(arg)

            raise AIFPLEvalError(f"Operator '{operator}' requires numeric argument, got {type(arg).__name__}")

        raise AIFPLEvalError(f"Unknown conversion function: '{operator}'")

    def _apply_boolean_operator(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> bool:
        """Apply boolean operators."""
        # Validate all arguments are booleans
        for i, arg in enumerate(args):
            if not isinstance(arg, bool):
                raise AIFPLEvalError(f"Operator '{operator}' requires boolean arguments, got {type(arg).__name__} at position {i}")

        if operator == 'and':
            if not args and 'identity' in op_def:
                return op_def['identity']

            return all(args)

        if operator == 'or':
            if not args and 'identity' in op_def:
                return op_def['identity']

            return any(args)

        if operator == 'not':
            return not args[0]

        raise AIFPLEvalError(f"Unknown boolean operator: '{operator}'")

    def _apply_string_operator(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> Union[str, int, float, bool]:
        """Apply string operators."""
        if operator == 'string-append':
            if not args and 'identity' in op_def:
                return op_def['identity']

            # Validate all arguments are strings
            for i, arg in enumerate(args):
                if not isinstance(arg, str):
                    raise AIFPLEvalError(
                        f"Operator '{operator}' requires string arguments, got {type(arg).__name__} at position {i}"
                    )

            return ''.join(args)

        if operator == 'string-length':
            arg = args[0]
            if not isinstance(arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string argument, got {type(arg).__name__}")

            return len(arg)

        if operator == 'substring':
            string_arg, start_arg, end_arg = args

            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as first argument, got {type(string_arg).__name__}")

            start = self._to_integer(start_arg, operator)
            end = self._to_integer(end_arg, operator)

            # Validate indices
            if start < 0:
                raise AIFPLEvalError(f"String index cannot be negative: {start}")

            if end < start:
                raise AIFPLEvalError(f"End index ({end}) cannot be less than start index ({start})")

            if start > len(string_arg):
                raise AIFPLEvalError(f"Start index ({start}) beyond string length ({len(string_arg)})")

            return string_arg[start:end]

        if operator == 'string-upcase':
            arg = args[0]
            if not isinstance(arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string argument, got {type(arg).__name__}")

            return arg.upper()

        if operator == 'string-downcase':
            arg = args[0]
            if not isinstance(arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string argument, got {type(arg).__name__}")

            return arg.lower()

        if operator == 'string-ref':
            string_arg, index_arg = args

            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as first argument, got {type(string_arg).__name__}")

            index = self._to_integer(index_arg, operator)

            if index < 0 or index >= len(string_arg):
                raise AIFPLEvalError(f"String index {index} out of range for string of length {len(string_arg)}")

            return string_arg[index]

        if operator == 'string->number':
            arg = args[0]
            if not isinstance(arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string argument, got {type(arg).__name__}")

            # Try to parse as number
            arg = arg.strip()

            try:
                # Try integer first
                if '.' not in arg and 'e' not in arg.lower():
                    return int(arg)

                return float(arg)

            except ValueError as e:
                raise AIFPLEvalError(f"Cannot convert string '{arg}' to number") from e

        # String predicates
        if operator == 'string-contains?':
            string_arg, substring_arg = args

            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as first argument, got {type(string_arg).__name__}")

            if not isinstance(substring_arg, str):
                raise AIFPLEvalError(
                    f"Operator '{operator}' requires string as second argument, got {type(substring_arg).__name__}"
                )

            return substring_arg in string_arg

        if operator == 'string-prefix?':
            string_arg, prefix_arg = args

            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as first argument, got {type(string_arg).__name__}")

            if not isinstance(prefix_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as second argument, got {type(prefix_arg).__name__}")

            return string_arg.startswith(prefix_arg)

        if operator == 'string-suffix?':
            string_arg, suffix_arg = args

            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as first argument, got {type(string_arg).__name__}")

            if not isinstance(suffix_arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string as second argument, got {type(suffix_arg).__name__}")

            return string_arg.endswith(suffix_arg)

        if operator == 'string=?':
            # Validate all arguments are strings
            for i, arg in enumerate(args):
                if not isinstance(arg, str):
                    raise AIFPLEvalError(
                        f"Operator '{operator}' requires string arguments, got {type(arg).__name__} at position {i}"
                    )

            # Check if all strings are equal
            return all(arg == args[0] for arg in args[1:])

        raise AIFPLEvalError(f"Unknown string operator: '{operator}'")

    def _apply_boolean_returning_operator(self, operator: str, args: List[Any]) -> bool:
        """Apply operators that return booleans (including comparisons and predicates)."""
        # Handle list? predicate specially
        if operator == 'list?':
            return isinstance(args[0], list)

        # Handle list equality specially
        if operator == '=' and any(isinstance(arg, list) for arg in args):
            # All arguments must be lists for list comparison
            for arg in args:
                if not isinstance(arg, list):
                    raise AIFPLEvalError(f"Cannot compare list with {type(arg).__name__}")

            # Check if all lists are equal
            return all(arg == args[0] for arg in args[1:])

        # Filter out string, boolean, and list arguments for numeric comparisons
        for arg in args:
            if isinstance(arg, (str, bool, list)):
                if isinstance(arg, list):
                    raise AIFPLEvalError(f"Comparison operator '{operator}' cannot compare lists (only equality '=' works)")

                raise AIFPLEvalError(f"Comparison operator '{operator}' cannot operate on {type(arg).__name__} arguments")

        # Promote types to common type
        promoted_args = self._promote_types(*args)

        try:
            if operator == '=':
                return all(arg == promoted_args[0] for arg in promoted_args[1:])

            if operator == '<':
                return all(promoted_args[i] < promoted_args[i + 1] for i in range(len(promoted_args) - 1))

            if operator == '>':
                return all(promoted_args[i] > promoted_args[i + 1] for i in range(len(promoted_args) - 1))

            if operator == '<=':
                return all(promoted_args[i] <= promoted_args[i + 1] for i in range(len(promoted_args) - 1))

            if operator == '>=':
                return all(promoted_args[i] >= promoted_args[i + 1] for i in range(len(promoted_args) - 1))

            raise AIFPLEvalError(f"Unknown comparison operator: '{operator}'")

        except TypeError as e:
            raise AIFPLEvalError(f"Cannot compare values in '{operator}': {e}") from e

    def _apply_bitwise_operator(self, operator: str, args: List[Any]) -> int:
        """Apply bitwise operators (require integer arguments)."""
        # Convert all arguments to integers
        int_args = [self._to_integer(arg, operator) for arg in args]

        if operator == 'bit-or':
            result = int_args[0]
            for arg in int_args[1:]:
                result |= arg

            return result

        if operator == 'bit-and':
            result = int_args[0]
            for arg in int_args[1:]:
                result &= arg

            return result

        if operator == 'bit-xor':
            result = int_args[0]
            for arg in int_args[1:]:
                result ^= arg

            return result

        if operator == 'bit-not':
            return ~int_args[0]

        if operator == 'bit-shift-left':
            left, right = int_args
            if right < 0:
                raise AIFPLEvalError("Shift count cannot be negative")

            if right > 64:
                raise AIFPLEvalError("Shift count too large (max 64)")

            return left << right

        if operator == 'bit-shift-right':
            left, right = int_args
            if right < 0:
                raise AIFPLEvalError("Shift count cannot be negative")

            if right > 64:
                raise AIFPLEvalError("Shift count too large (max 64)")

            return left >> right

        raise AIFPLEvalError(f"Unknown bitwise operator: '{operator}'")

    def _apply_real_only_function(self, operator: str, args: List[Any]) -> Union[int, float]:
        """Apply functions that only work with real numbers."""
        arg = args[0]

        # Convert complex to real if imaginary part is negligible
        if isinstance(arg, complex):
            if abs(arg.imag) >= self.imaginary_tolerance:
                raise AIFPLEvalError(f"Function '{operator}' does not support complex numbers")
            arg = arg.real

        if operator == 'round':
            return round(arg)

        if operator == 'floor':
            return math.floor(arg)

        if operator == 'ceil':
            return math.ceil(arg)

        raise AIFPLEvalError(f"Unknown real-only function: '{operator}'")

    def _apply_integer_only_function(self, operator: str, args: List[Any]) -> int:
        """Apply functions that only work with integers."""
        arg = args[0]
        return self._to_integer(arg, operator)

    def _apply_mathematical_operator(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> Union[int, float, complex]:
        """Apply regular mathematical operators with type promotion."""
        if not args and 'identity' in op_def:
            return op_def['identity']

        # Promote types to common type
        promoted_args = self._promote_types(*args)

        try:
            if operator == '+':
                return sum(promoted_args)

            if operator == '-':
                if len(promoted_args) == 1:
                    return -promoted_args[0]

                result = promoted_args[0]
                for arg in promoted_args[1:]:
                    result -= arg

                return result

            if operator == '*':
                result = promoted_args[0] if promoted_args else 1
                for arg in promoted_args[1:]:
                    result *= arg

                return result

            if operator == '/':
                result = promoted_args[0]
                for arg in promoted_args[1:]:
                    if arg == 0:
                        raise ZeroDivisionError("Division by zero")

                    result /= arg

                return result

            if operator == '//':
                a, b = promoted_args
                if b == 0:
                    raise ZeroDivisionError("Division by zero")

                return a // b

            if operator == '%':
                a, b = promoted_args
                if b == 0:
                    raise ZeroDivisionError("Modulo by zero")

                return a % b

            if operator == '**':
                a, b = promoted_args
                return a ** b

            if operator == 'pow':
                a, b = promoted_args
                return pow(a, b)

            if operator == 'sin':
                return cmath.sin(promoted_args[0])

            if operator == 'cos':
                return cmath.cos(promoted_args[0])

            if operator == 'tan':
                return cmath.tan(promoted_args[0])

            if operator == 'log':
                return cmath.log(promoted_args[0])

            if operator == 'log10':
                return cmath.log10(promoted_args[0])

            if operator == 'exp':
                return cmath.exp(promoted_args[0])

            if operator == 'sqrt':
                return cmath.sqrt(promoted_args[0])

            if operator == 'abs':
                return abs(promoted_args[0])

            if operator == 'min':
                return min(promoted_args)

            if operator == 'max':
                return max(promoted_args)

            if operator == 'complex':
                real, imag = promoted_args
                return complex(real, imag)

            if operator == 'real':
                return self._extract_real_part(promoted_args[0])

            if operator == 'imag':
                return self._extract_imaginary_part(promoted_args[0])

            raise AIFPLEvalError(f"Unknown mathematical operator: '{operator}'")

        except ZeroDivisionError as e:
            raise AIFPLEvalError(f"Division by zero error in '{operator}': {e}") from e

        except (ValueError, OverflowError) as e:
            raise AIFPLEvalError(f"Mathematical error in '{operator}': {e}") from e

    def _extract_real_part(self, value: Union[int, float, complex]) -> Union[int, float]:
        """
        Extract the real part of a number, returning the most specific type.

        Args:
            value: Numeric value to extract real part from

        Returns:
            Real part as int or float
        """
        if isinstance(value, complex):
            real_part = value.real
            # If the imaginary part is below tolerance, we might want to simplify
            # but we still return the real part with its original precision
            if isinstance(real_part, float) and real_part.is_integer():
                return int(real_part)

            return real_part

        # For real numbers, return as-is (int or float)
        return value

    def _extract_imaginary_part(self, value: Union[int, float, complex]) -> Union[int, float]:
        """
        Extract the imaginary part of a number, returning the most specific type.

        Args:
            value: Numeric value to extract imaginary part from

        Returns:
            Imaginary part as int or float
        """
        if isinstance(value, complex):
            imag_part = value.imag
            # Apply tolerance check - if below threshold, return 0 (int)
            if abs(imag_part) < self.imaginary_tolerance:
                return 0

            # Otherwise return the imaginary part, simplifying to int if possible
            if isinstance(imag_part, float) and imag_part.is_integer():
                return int(imag_part)

            return imag_part

        # For real numbers, imaginary part is always 0 (int)
        return 0

    def _promote_types(self, *values: Any) -> tuple:
        """Promote arguments to common type: int → float → complex."""
        has_complex = any(isinstance(v, complex) for v in values)
        has_float = any(isinstance(v, float) for v in values)

        if has_complex:
            return tuple(complex(v) for v in values)

        if has_float:
            return tuple(float(v) for v in values)

        return values  # All integers

    def _to_integer(self, value: Union[int, float, complex], operation: str) -> int:
        """Convert a numeric value to integer for operations that require it."""
        if isinstance(value, complex):
            if abs(value.imag) >= self.imaginary_tolerance:
                raise AIFPLEvalError(f"Operation '{operation}' does not support complex numbers")

            value = value.real

        if isinstance(value, float):
            if not value.is_integer():
                raise AIFPLEvalError(f"Operation '{operation}' requires integer values, got float: {value}")

            value = int(value)

        return value

    def simplify_result(self, result: Union[int, float, complex, str, bool, list]) -> Union[int, float, complex, str, bool, list]:
        """Simplify complex results to real numbers when imaginary part is negligible."""
        if isinstance(result, complex):
            # If imaginary part is effectively zero, return just the real part
            if abs(result.imag) < self.imaginary_tolerance:
                real_part = result.real
                # Convert to int if it's a whole number
                if isinstance(real_part, float) and real_part.is_integer():
                    return int(real_part)

                return real_part

        # For real numbers, convert float to int if it's a whole number
        if isinstance(result, float) and result.is_integer():
            return int(result)

        return result

    def format_result(self, result: Union[int, float, complex, str, bool, list]) -> str:
        """
        Format result for display, using LISP conventions for lists and booleans.

        Args:
            result: The result to format

        Returns:
            String representation of the result
        """
        if isinstance(result, bool):
            return "#t" if result else "#f"

        if isinstance(result, str):
            # For strings, add quotes to distinguish from symbols
            return f'"{result}"'

        if isinstance(result, list):
            # Format list in LISP notation: (element1 element2 ...)
            if not result:
                return "()"

            formatted_elements = []
            for element in result:
                formatted_elements.append(self.format_result(element))

            return f"({' '.join(formatted_elements)})"

        # For other types, use standard string representation
        return str(result)
