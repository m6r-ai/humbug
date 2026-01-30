"""Mathematical built-in functions for AIFPL."""

import cmath
import math
from typing import List, Union, Callable, cast

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLString, AIFPLBoolean


class AIFPLMathFunctions:
    """Mathematical built-in functions for AIFPL."""

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

        values = []
        for arg in args:
            val = self._ensure_number(arg, "+")
            values.append(val)

        return self._wrap_numeric_result(sum(values))

    def _builtin_minus(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement - operation."""
        if len(args) == 0:
            raise AIFPLEvalError("Function '-' requires at least 1 argument, got 0")

        values = []
        for arg in args:
            val = self._ensure_number(arg, "-")
            values.append(val)

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

        values = []
        for arg in args:
            val = self._ensure_number(arg, "*")
            values.append(val)

        result = values[0]
        for val in values[1:]:
            result *= val

        return self._wrap_numeric_result(result)

    def _builtin_slash(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement / operation."""
        if len(args) < 2:
            raise AIFPLEvalError("Function '/' requires at least 2 arguments, got " + str(len(args)))

        values = []
        for arg in args:
            val = self._ensure_number(arg, "/")
            values.append(val)

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
        result = base ** exponent
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

        values = []
        for i, arg in enumerate(args):
            val = self._ensure_real_number(arg, "<")
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

        values = []
        for i, arg in enumerate(args):
            val = self._ensure_real_number(arg, ">")
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

        values = []
        for i, arg in enumerate(args):
            val = self._ensure_real_number(arg, "<=")
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

        values = []
        for i, arg in enumerate(args):
            val = self._ensure_real_number(arg, ">=")
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
        return AIFPLBoolean(not bool_val)

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

        val = self._ensure_number(args[0], "sin")

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.sin(val))

        return self._wrap_numeric_result(math.sin(cast(float, val)))

    def _builtin_cos(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement cos function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'cos' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "cos")

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.cos(val))

        return self._wrap_numeric_result(math.cos(cast(float, val)))

    def _builtin_tan(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement tan function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'tan' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "tan")

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.tan(val))

        return self._wrap_numeric_result(math.tan(cast(float, val)))

    def _builtin_log(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement log (natural logarithm) function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'log' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "log")

        # Handle log(0) = -inf
        if isinstance(val, (int, float)) and val == 0:
            return AIFPLFloat(float('-inf'))

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return self._wrap_numeric_result(cmath.log(val))

        return self._wrap_numeric_result(math.log(cast(float, val)))

    def _builtin_log10(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement log10 function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'log10' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "log10")

        # Handle log10(0) = -inf
        if isinstance(val, (int, float)) and val == 0:
            return AIFPLFloat(float('-inf'))

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return self._wrap_numeric_result(cmath.log10(val))

        return self._wrap_numeric_result(math.log10(cast(float, val)))

    def _builtin_exp(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement exp function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'exp' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "exp")

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.exp(val))

        return self._wrap_numeric_result(math.exp(cast(float, val)))

    def _builtin_sqrt(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement sqrt function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'sqrt' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "sqrt")

        if isinstance(val, complex):
            return self._wrap_numeric_result(cmath.sqrt(val))

        if val < 0:
            raise AIFPLEvalError("Function 'sqrt' requires a non-negative argument")

        return self._wrap_numeric_result(math.sqrt(float(val)))

    def _builtin_abs(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement abs function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'abs' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "abs")
        return self._wrap_numeric_result(abs(val))

    def _builtin_round(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement round function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'round' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_real_number(args[0], "round")
        return AIFPLInteger(round(val))

    def _builtin_floor(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement floor function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'floor' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_real_number(args[0], "floor")
        return AIFPLInteger(math.floor(val))

    def _builtin_ceil(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement ceil function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'ceil' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_real_number(args[0], "ceil")
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

        base = self._ensure_real_number(args[0], "pow")
        exponent = self._ensure_real_number(args[1], "pow")
        return self._wrap_numeric_result(base ** exponent)

    # Base conversion functions
    def _builtin_bin(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement bin function - returns Scheme-style binary string (#b1010 or -#b1010)."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'bin' requires exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "bin")
        # Convert Python's 0b1010 to Scheme's #b1010 or -#b1010
        if int_val < 0:
            return AIFPLString(f"-#b{bin(-int_val)[2:]}")

        return AIFPLString(f"#b{bin(int_val)[2:]}")

    def _builtin_hex(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement hex function - returns Scheme-style hex string (#xff or -#xff)."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'hex' requires exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "hex")
        # Convert Python's 0xff to Scheme's #xff or -#xff
        if int_val < 0:
            return AIFPLString(f"-#x{hex(-int_val)[2:]}")

        return AIFPLString(f"#x{hex(int_val)[2:]}")

    def _builtin_oct(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement oct function - returns Scheme-style octal string (#o755 or -#o755)."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'oct' requires exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "oct")
        # Convert Python's 0o755 to Scheme's #o755 or -#o755
        if int_val < 0:
            return AIFPLString(f"-#o{oct(-int_val)[2:]}")

        return AIFPLString(f"#o{oct(int_val)[2:]}")

    # Complex number functions
    def _builtin_real(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement real function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'real' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "real")

        if isinstance(val, complex):
            return AIFPLFloat(val.real)

        # For real numbers, return as-is
        return AIFPLFloat(float(cast(float, val)))

    def _builtin_imag(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement imag function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'imag' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_number(args[0], "imag")

        if isinstance(val, complex):
            return AIFPLFloat(val.imag)

        # For real numbers, imaginary part is 0.0
        return AIFPLFloat(0.0)

    def _builtin_complex(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement complex function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Function 'complex' requires exactly 2 arguments, got {len(args)}")

        real_part = self._ensure_real_number(args[0], "complex")
        imag_part = self._ensure_real_number(args[1], "complex")
        return AIFPLComplex(complex(real_part, imag_part))

    # Type conversion functions
    def _builtin_integer(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement integer conversion function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'integer' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_real_number(args[0], "integer")

        # Handle float - truncate toward zero
        if isinstance(val, float):
            return AIFPLInteger(int(val))

        # Already an integer
        return AIFPLInteger(val)

    def _builtin_float(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement float conversion function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function 'float' requires exactly 1 argument, got {len(args)}")

        val = self._ensure_real_number(args[0], "float")
        return AIFPLFloat(float(val))

    # Helper methods for type checking and conversion
    def _ensure_number(self, value: AIFPLValue, function_name: str) -> int | float | complex:
        """Ensure value is a number, raise error if not."""
        if not isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            raise AIFPLEvalError(f"Function '{function_name}' requires numeric arguments, got {value.type_name()}")

        return value.value

    def _ensure_boolean(self, value: AIFPLValue, function_name: str) -> bool:
        """Ensure value is a boolean, raise error if not."""
        if not isinstance(value, AIFPLBoolean):
            raise AIFPLEvalError(f"Function '{function_name}' requires boolean arguments, got {value.type_name()}")

        return value.value

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
