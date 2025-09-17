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
    }

    # Operator and function definitions
    OPERATORS: Dict[str, Dict[str, Any]] = {
        # Arithmetic operators
        '+': {'type': 'variadic', 'min_args': 0, 'identity': 0},
        '-': {'type': 'variadic', 'min_args': 1},
        '*': {'type': 'variadic', 'min_args': 0, 'identity': 1},
        '/': {'type': 'variadic', 'min_args': 2},
        '//': {'type': 'binary'},
        '%': {'type': 'binary'},
        '**': {'type': 'binary'},

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

    def evaluate(self, expr: SExpression, depth: int = 0) -> Union[int, float, complex, str]:
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
        if isinstance(expr, (int, float, complex)):
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

    def _apply_operator(self, operator: str, args: List[SExpression], depth: int) -> Union[int, float, complex, str]:
        """Apply an operator to its arguments."""
        if operator not in self.OPERATORS:
            raise AIFPLEvalError(f"Unknown operator: '{operator}'")

        op_def: Dict[str, Any] = self.OPERATORS[operator]

        # Evaluate arguments
        evaluated_args = [self.evaluate(arg, depth) for arg in args]

        # Check argument count
        self._validate_arity(operator, op_def, evaluated_args)

        # Handle special cases that return strings
        if op_def.get('returns_string'):
            return self._apply_string_function(operator, evaluated_args)

        # Filter out string arguments for mathematical operations
        for arg in evaluated_args:
            if isinstance(arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' cannot operate on string arguments")

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

    def _validate_arity(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> None:
        """Validate argument count for an operator."""
        op_type = op_def['type']
        arg_count = len(args)

        if op_type == 'unary' and arg_count != 1:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 1 argument, got {arg_count}")

        if op_type == 'binary' and arg_count != 2:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 2 arguments, got {arg_count}")

        if op_type == 'variadic':
            min_args = op_def.get('min_args', 0)
            if arg_count < min_args:
                raise AIFPLEvalError(f"Operator '{operator}' requires at least {min_args} arguments, got {arg_count}")

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

    def simplify_result(self, result: Union[int, float, complex, str]) -> Union[int, float, complex, str]:
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
