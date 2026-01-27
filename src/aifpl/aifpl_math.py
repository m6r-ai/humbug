"""Mathematical built-in functions for AIFPL."""

import cmath
import math
from typing import List, Union, Callable

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLString, AIFPLBoolean


class AIFPLMathFunctions:
    """Mathematical built-in functions for AIFPL."""

    def __init__(self):
        """Initialize with floating point tolerance."""

    def _extract_numeric_value(self, value: AIFPLValue) -> Union[int, float, complex]:
        """Extract Python numeric value from AIFPL number types."""
        if isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            return value.value

        raise AIFPLEvalError(f"Expected number, got {value.type_name()}")

    def _wrap_numeric_result(self, result: Union[int, float, complex]) -> AIFPLValue:
        """Wrap Python numeric value in appropriate AIFPL type based on its type."""
        if isinstance(result, int):
            return AIFPLInteger(result)

        if isinstance(result, float):
            return AIFPLFloat(result)

        if isinstance(result, complex):
            return AIFPLComplex(result)

        # Fallback (shouldn't happen)
        raise AIFPLEvalError(f"Unexpected numeric type: {type(result)}")

    def _is_numeric_type(self, value: AIFPLValue) -> bool:
        """Check if value is any numeric type."""
        return isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex))

    def _is_integer_type(self, value: AIFPLValue) -> bool:
        """Check if value is an integer type."""
        return isinstance(value, AIFPLInteger)

    def get_functions(self) -> dict[str, Callable]:
        """Return dictionary of mathematical function implementations."""
        return {
            # Arithmetic functions
            '+': self._builtin_plus,
            '-': self._builtin_minus,
            '*': self._builtin_star,
            '/': self._builtin_slash,
            '//': self._builtin_slash_slash,
            '%': self._builtin_percent,
            '**': self._builtin_star_star,

            # Comparison functions
            '=': self._builtin_eq,
            '!=': self._builtin_bang_eq,
            '<': self._builtin_lt,
            '>': self._builtin_gt,
            '<=': self._builtin_lte,
            '>=': self._builtin_gte,

            # Boolean functions
            'not': self._builtin_not,

            # Bitwise functions
            'bit-or': self._builtin_bit_or,
            'bit-and': self._builtin_bit_and,
            'bit-xor': self._builtin_bit_xor,
            'bit-not': self._builtin_bit_not,
            'bit-shift-left': self._builtin_bit_shift_left,
            'bit-shift-right': self._builtin_bit_shift_right,

            # Mathematical functions
            'sin': self._builtin_sin,
            'cos': self._builtin_cos,
            'tan': self._builtin_tan,
            'log': self._builtin_log,
            'log10': self._builtin_log10,
            'exp': self._builtin_exp,
            'sqrt': self._builtin_sqrt,
            'abs': self._builtin_abs,
            'round': self._builtin_round,
            'floor': self._builtin_floor,
            'ceil': self._builtin_ceil,
            'min': self._builtin_min,
            'max': self._builtin_max,
            'pow': self._builtin_pow,

            # Base conversion functions
            'bin': self._builtin_bin,
            'hex': self._builtin_hex,
            'oct': self._builtin_oct,

            # Complex number functions
            'real': self._builtin_real,
            'imag': self._builtin_imag,
            'complex': self._builtin_complex,

            # Type conversion functions
            'integer': self._builtin_integer,
            'float': self._builtin_float,
        }

    # Arithmetic operations
    def _builtin_plus(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement + operation."""
        if not args:
            return self._wrap_numeric_result(0)

        # Phase 1: Extract values from both old and new types
        values = []
        for arg in args:
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '+' requires numeric arguments, got {arg.type_name()}")

            values.append(self._extract_numeric_value(arg))

        return self._wrap_numeric_result(sum(values))

    def _builtin_minus(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement - operation."""
        if len(args) == 0:
            raise AIFPLEvalError("Function '-' requires at least 1 argument, got 0")

        # Phase 1: Extract values from both old and new types
        values = []
        for arg in args:
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '-' requires numeric arguments, got {arg.type_name()}")

            values.append(self._extract_numeric_value(arg))

        if len(args) == 1:
            return self._wrap_numeric_result(-values[0])

        result = values[0]
        for val in values[1:]:
            result -= val

        return self._wrap_numeric_result(result)

    def _builtin_star(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement * operation."""
        if not args:
            return self._wrap_numeric_result(1)

        # Phase 1: Extract values from both old and new types
        values = []
        for arg in args:
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '*' requires numeric arguments, got {arg.type_name()}")

            values.append(self._extract_numeric_value(arg))

        result = values[0]
        for val in values[1:]:
            result *= val
        return self._wrap_numeric_result(result)

    def _builtin_slash(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement / operation."""
        if len(args) < 2:
            raise AIFPLEvalError("Function '/' requires at least 2 arguments, got " + str(len(args)))

        # Phase 1: Extract values from both old and new types
        values = []
        for arg in args:
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '/' requires numeric arguments, got {arg.type_name()}")

            values.append(self._extract_numeric_value(arg))

        # Check for division by zero
        for i, val in enumerate(values[1:], 1):
            if val == 0:
                raise AIFPLEvalError(f"Division by zero at argument {i+1}")

        result = values[0]
        for val in values[1:]:
            result /= val

        return self._wrap_numeric_result(result)

    def _builtin_slash_slash(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement // (floor division) operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Floor division requires exactly 2 arguments, got {len(args)}")

        left_val = self._ensure_real_number(args[0], "//")
        right_val = self._ensure_real_number(args[1], "//")

        if right_val == 0:
            raise AIFPLEvalError("Division by zero")

        return self._wrap_numeric_result(left_val // right_val)

    def _builtin_percent(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement % (modulo) operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Modulo requires exactly 2 arguments, got {len(args)}")

        left_val = self._ensure_real_number(args[0], "%")
        right_val = self._ensure_real_number(args[1], "%")

        if right_val == 0:
            raise AIFPLEvalError("Modulo by zero")

        return self._wrap_numeric_result(left_val % right_val)

    def _builtin_star_star(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement ** (exponentiation) operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Function '**' requires exactly 2 arguments, got {len(args)}")

        base = self._ensure_number(args[0], "**")
        exponent = self._ensure_number(args[1], "**")
        result = self._extract_numeric_value(base) ** self._extract_numeric_value(exponent)
        return self._wrap_numeric_result(result)

    # Comparison operations
    def _builtin_eq(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement = (equality) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '=' requires at least 2 arguments, got {len(args)}")

        # All values must be equal
        first = args[0]
        return AIFPLBoolean(all(first == arg for arg in args[1:]))

    def _builtin_bang_eq(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement != (inequality) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '!=' requires at least 2 arguments, got {len(args)}")

        # Any values not equal
        for i, arg_i in enumerate(args):
            for j in range(i + 1, len(args)):
                if arg_i != args[j]:
                    return AIFPLBoolean(True)

        return AIFPLBoolean(False)

    def _builtin_lt(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement < (less than) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '<' requires at least 2 arguments, got {len(args)}")

        # Phase 2: Extract values and ensure they're numeric and real (not complex)
        values = []
        for i, arg in enumerate(args):
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '<' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            val = self._extract_numeric_value(arg)
            if isinstance(val, complex):
                raise AIFPLEvalError("Function '<' does not support complex numbers")

            values.append(val)

        # Check comparison chain
        for i in range(len(args) - 1):
            # Use extracted values
            if not values[i] < values[i + 1]:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_gt(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement > (greater than) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '>' requires at least 2 arguments, got {len(args)}")

        # Phase 2: Extract values and ensure they're numeric and real (not complex)
        values = []
        for i, arg in enumerate(args):
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '>' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            val = self._extract_numeric_value(arg)
            if isinstance(val, complex):
                raise AIFPLEvalError("Function '>' does not support complex numbers")

            values.append(val)

        # Check comparison chain
        for i in range(len(args) - 1):
            if not values[i] > values[i + 1]:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_lte(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement <= (less than or equal) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '<=' requires at least 2 arguments, got {len(args)}")

        # Phase 2: Extract values and ensure they're numeric and real (not complex)
        values = []
        for i, arg in enumerate(args):
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '<=' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            val = self._extract_numeric_value(arg)
            if isinstance(val, complex):
                raise AIFPLEvalError("Function '<=' does not support complex numbers")

            values.append(val)

        # Check comparison chain
        for i in range(len(args) - 1):
            if not values[i] <= values[i + 1]:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_gte(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement >= (greater than or equal) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '>=' requires at least 2 arguments, got {len(args)}")

        # Phase 2: Extract values and ensure they're numeric and real (not complex)
        values = []
        for i, arg in enumerate(args):
            if not self._is_numeric_type(arg):
                raise AIFPLEvalError(f"Function '>=' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            val = self._extract_numeric_value(arg)
            if isinstance(val, complex):
                raise AIFPLEvalError("Function '>=' does not support complex numbers")

            values.append(val)

        # Check comparison chain
        for i in range(len(args) - 1):
            if not values[i] >= values[i + 1]:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_not(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement not operation."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'not' requires exactly 1 argument, got {len(args)}")

        bool_val = self._ensure_boolean(args[0], "not")
        return AIFPLBoolean(not bool_val.value)

    # Bitwise operations
    def _builtin_bit_or(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bit-or operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function 'bit-or' requires at least 2 arguments, got {len(args)}")

        int_args = [self._ensure_integer(arg, "bit-or") for arg in args]
        result = int_args[0]
        for arg in int_args[1:]:
            result |= arg

        return AIFPLInteger(result)

    def _builtin_bit_and(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bit-and operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function 'bit-and' requires at least 2 arguments, got {len(args)}")

        int_args = [self._ensure_integer(arg, "bit-and") for arg in args]
        result = int_args[0]
        for arg in int_args[1:]:
            result &= arg

        return AIFPLInteger(result)

    def _builtin_bit_xor(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bit-xor operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function 'bit-xor' requires at least 2 arguments, got {len(args)}")

        int_args = [self._ensure_integer(arg, "bit-xor") for arg in args]
        result = int_args[0]
        for arg in int_args[1:]:
            result ^= arg

        return AIFPLInteger(result)

    def _builtin_bit_not(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bit-not operation."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'bit-not' requires exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "bit-not")
        return AIFPLInteger(~int_val)

    def _builtin_bit_shift_left(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bit-shift-left operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Function 'bit-shift-left' requires exactly 2 arguments, got {len(args)}")

        value = self._ensure_integer(args[0], "bit-shift-left")
        shift = self._ensure_integer(args[1], "bit-shift-left")
        return AIFPLInteger(value << shift)

    def _builtin_bit_shift_right(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bit-shift-right operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Function 'bit-shift-right' requires exactly 2 arguments, got {len(args)}")

        value = self._ensure_integer(args[0], "bit-shift-right")
        shift = self._ensure_integer(args[1], "bit-shift-right")
        return AIFPLInteger(value >> shift)

    # Mathematical functions
    def _builtin_sin(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement sin function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'sin' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.sin(val))

        return self._wrap_numeric_result(math.sin(val))

    def _builtin_cos(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement cos function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'cos' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.cos(val))

        return self._wrap_numeric_result(math.cos(val))

    def _builtin_tan(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement tan function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'tan' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.tan(val))

        return self._wrap_numeric_result(math.tan(val))

    def _builtin_log(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement log (natural logarithm) function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'log' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        # Handle log(0) = -inf
        if isinstance(val, (int, float)) and val == 0:
            return AIFPLFloat(float('-inf'))

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return self._wrap_numeric_result(cmath.log(val))

        return self._wrap_numeric_result(math.log(val))

    def _builtin_log10(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement log10 function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'log10' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        # Handle log10(0) = -inf
        if isinstance(val, (int, float)) and val == 0:
            return AIFPLFloat(float('-inf'))

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return self._wrap_numeric_result(cmath.log10(val))

        return self._wrap_numeric_result(math.log10(val))

    def _builtin_exp(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement exp function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'exp' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.exp(val))

        return self._wrap_numeric_result(math.exp(val))

    def _builtin_sqrt(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement sqrt function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'sqrt' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return self._wrap_numeric_result(cmath.sqrt(val))

        return self._wrap_numeric_result(math.sqrt(val))

    def _builtin_abs(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement abs function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'abs' requires exactly 1 argument, got {len(args)}")

        return self._wrap_numeric_result(abs(self._extract_numeric_value(args[0])))

    def _builtin_round(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement round function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'round' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        # Extract real part if complex
        if isinstance(val, complex):
            raise AIFPLEvalError("Function 'round' does not support complex numbers")

        return AIFPLInteger(round(val))

    def _builtin_floor(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement floor function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'floor' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        # Extract real part if complex
        if isinstance(val, complex):
            raise AIFPLEvalError("Function 'floor' does not support complex numbers")

        return AIFPLInteger(math.floor(val))

    def _builtin_ceil(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement ceil function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'ceil' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        # Extract real part if complex
        if isinstance(val, complex):
            raise AIFPLEvalError("Function 'ceil' does not support complex numbers")

        return AIFPLInteger(math.ceil(val))

    def _builtin_min(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement min function."""
        if not args:
            raise AIFPLEvalError("Function 'min' requires at least 1 argument, got 0")

        # Use type narrowing to handle only real numbers for min/max
        real_values = []
        for arg in args:
            real_val = self._ensure_real_number(arg, "min")
            real_values.append(real_val)

        return self._wrap_numeric_result(min(real_values))

    def _builtin_max(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement max function."""
        if not args:
            raise AIFPLEvalError("Function 'max' requires at least 1 argument, got 0")

        # Use type narrowing to handle only real numbers for min/max
        real_values = []
        for arg in args:
            real_val = self._ensure_real_number(arg, "max")
            real_values.append(real_val)

        return self._wrap_numeric_result(max(real_values))

    def _builtin_pow(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement pow function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Function 'pow' requires exactly 2 arguments, got {len(args)}")

        base = self._extract_numeric_value(args[0])
        exponent = self._extract_numeric_value(args[1])
        return self._wrap_numeric_result(base ** exponent)

    # Base conversion functions
    def _builtin_bin(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bin function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'bin' requires exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "bin")
        return AIFPLString(bin(int_val))

    def _builtin_hex(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement hex function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'hex' requires exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "hex")
        return AIFPLString(hex(int_val))

    def _builtin_oct(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement oct function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'oct' requires exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "oct")
        return AIFPLString(oct(int_val))

    # Complex number functions
    def _builtin_real(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement real function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'real' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        if isinstance(val, complex):
            real_part = val.real
            # Convert to int if it's a whole number
            if isinstance(real_part, float) and real_part.is_integer():
                return AIFPLInteger(int(real_part))

            return AIFPLFloat(real_part)

        # For real numbers, return as-is
        return args[0]

    def _builtin_imag(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement imag function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'imag' requires exactly 1 argument, got {len(args)}")

        val = self._extract_numeric_value(args[0])

        if isinstance(val, complex):
            imag_part = val.imag
            # Convert to int if it's a whole number
            if isinstance(imag_part, float) and imag_part.is_integer():
                return AIFPLInteger(int(imag_part))

            return AIFPLFloat(imag_part)

        # For real numbers, imaginary part is 0
        return AIFPLInteger(0)

    def _builtin_complex(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement complex function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Function 'complex' requires exactly 2 arguments, got {len(args)}")

        real_part = self._extract_numeric_value(args[0])
        imag_part = self._extract_numeric_value(args[1])

        if isinstance(real_part, complex) or isinstance(imag_part, complex):
            raise AIFPLEvalError("Function 'complex' arguments must be real numbers")

        return AIFPLComplex(complex(real_part, imag_part))

    # Type conversion functions
    def _builtin_integer(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement integer conversion function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'integer' requires exactly 1 argument, got {len(args)}")

        if not self._is_numeric_type(args[0]):
            raise AIFPLEvalError(f"Function 'integer' requires numeric arguments, got {args[0].type_name()}")

        val = self._extract_numeric_value(args[0])

        # Handle complex numbers
        if isinstance(val, complex):
            raise AIFPLEvalError("Function 'integer' cannot convert complex number with non-zero imaginary part")

        # Handle float - truncate toward zero
        if isinstance(val, float):
            return AIFPLInteger(int(val))

        # Already an integer
        return AIFPLInteger(val)

    def _builtin_float(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement float conversion function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'float' requires exactly 1 argument, got {len(args)}")

        if not self._is_numeric_type(args[0]):
            raise AIFPLEvalError(f"Function 'float' requires numeric arguments, got {args[0].type_name()}")

        val = self._extract_numeric_value(args[0])

        # Handle complex numbers
        if isinstance(val, complex):
            raise AIFPLEvalError("Function 'float' cannot convert complex number with non-zero imaginary part")

        # Convert to float
        return AIFPLFloat(float(val))

    # Helper methods for type checking and conversion
    def _ensure_number(self, value: AIFPLValue, function_name: str) -> AIFPLInteger | AIFPLFloat | AIFPLComplex:
        """Ensure value is a number, raise error if not."""
        if not isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            raise AIFPLEvalError(f"Function '{function_name}' requires numeric arguments, got {value.type_name()}")

        return value

    def _ensure_boolean(self, value: AIFPLValue, function_name: str) -> AIFPLBoolean:
        """Ensure value is a boolean, raise error if not."""
        if not isinstance(value, AIFPLBoolean):
            raise AIFPLEvalError(f"Function '{function_name}' requires boolean arguments, got {value.type_name()}")

        return value

    def _ensure_integer(self, value: AIFPLValue, function_name: str) -> int:
        """Ensure value is an integer, raise error if not, return Python int."""
        if isinstance(value, AIFPLInteger):
            return value.value

        # Not an integer type
        if isinstance(value, (AIFPLFloat, AIFPLComplex)):
            raise AIFPLEvalError(f"Function '{function_name}' requires integer arguments, got {value.type_name()}")

        # Not a numeric type at all
        raise AIFPLEvalError(f"Function '{function_name}' requires integer arguments, got {value.type_name()}")


    def _ensure_real_number(self, value: AIFPLValue, function_name: str) -> Union[int, float]:
        """Ensure value is a real number (int or float), raise error if complex."""
        if isinstance(value, AIFPLComplex):
            raise AIFPLEvalError(f"Function '{function_name}' does not support complex numbers")

        if isinstance(value, (AIFPLInteger, AIFPLFloat)):
            return value.value

        raise AIFPLEvalError(f"Function '{function_name}' requires numeric arguments, got {value.type_name()}")
