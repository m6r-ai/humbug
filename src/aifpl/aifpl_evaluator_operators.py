"""Complete operator implementations for AIFPL evaluator."""

import cmath
import math
from typing import List

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLList


class AIFPLOperatorMixin:
    """Mixin class containing all operator implementations."""

    def _ensure_number(self, value: AIFPLValue, operator: str) -> AIFPLNumber:
        """Ensure value is a number, raise error if not."""
        if not isinstance(value, AIFPLNumber):
            raise AIFPLEvalError(f"Operator '{operator}' requires numeric arguments, got {value.type_name()}")

        return value

    def _ensure_string(self, value: AIFPLValue, operator: str) -> AIFPLString:
        """Ensure value is a string, raise error if not."""
        if not isinstance(value, AIFPLString):
            raise AIFPLEvalError(f"Operator '{operator}' requires string arguments, got {value.type_name()}")

        return value

    def _ensure_boolean(self, value: AIFPLValue, operator: str) -> AIFPLBoolean:
        """Ensure value is a boolean, raise error if not."""
        if not isinstance(value, AIFPLBoolean):
            raise AIFPLEvalError(f"Operator '{operator}' requires boolean arguments, got {value.type_name()}")

        return value

    def _ensure_list(self, value: AIFPLValue, operator: str) -> AIFPLList:
        """Ensure value is a list, raise error if not."""
        if not isinstance(value, AIFPLList):
            raise AIFPLEvalError(f"Operator '{operator}' requires list arguments, got {value.type_name()}")

        return value

    def _ensure_integer(self, value: AIFPLValue, operator: str) -> int:
        """Ensure value is an integer, raise error if not."""
        if not isinstance(value, AIFPLNumber) or not value.is_integer():
            raise AIFPLEvalError(f"Operator '{operator}' requires integer arguments, got {value.type_name()}")

        return int(value.value)

    def _apply_string_operator(self, operator: str, op_def: dict, args: List[AIFPLValue]) -> AIFPLValue:
        """Apply string operations."""
        if operator == 'string-append':
            if not args:
                return AIFPLString(op_def.get('identity', ''))

            # Ensure all arguments are strings
            string_args = [self._ensure_string(arg, operator) for arg in args]
            result = ''.join(arg.value for arg in string_args)
            return AIFPLString(result)

        if operator == 'string-length':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-length requires exactly 1 argument, got {len(args)}")

            string_arg = self._ensure_string(args[0], operator)
            return AIFPLNumber(len(string_arg.value))

        if operator == 'string-upcase':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-upcase requires exactly 1 argument, got {len(args)}")

            string_arg = self._ensure_string(args[0], operator)
            return AIFPLString(string_arg.value.upper())

        if operator == 'string-downcase':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-downcase requires exactly 1 argument, got {len(args)}")

            string_arg = self._ensure_string(args[0], operator)
            return AIFPLString(string_arg.value.lower())

        if operator == 'string-trim':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-trim requires exactly 1 argument, got {len(args)}")

            string_arg = self._ensure_string(args[0], operator)
            return AIFPLString(string_arg.value.strip())

        if operator == 'string-replace':
            if len(args) != 3:
                raise AIFPLEvalError(f"string-replace requires exactly 3 arguments, got {len(args)}")

            string_arg, old_str, new_str = [self._ensure_string(arg, operator) for arg in args]
            result = string_arg.value.replace(old_str.value, new_str.value)
            return AIFPLString(result)

        if operator == 'substring':
            if len(args) != 3:
                raise AIFPLEvalError(f"substring requires exactly 3 arguments, got {len(args)}")

            string_arg = self._ensure_string(args[0], operator)
            start_idx = self._ensure_integer(args[1], operator)
            end_idx = self._ensure_integer(args[2], operator)

            string_len = len(string_arg.value)
            if start_idx < 0:
                raise AIFPLEvalError(f"substring start index cannot be negative: {start_idx}")

            if end_idx < 0:
                raise AIFPLEvalError(f"substring end index cannot be negative: {end_idx}")

            if start_idx > string_len:
                raise AIFPLEvalError(f"substring start index out of range: {start_idx} (string length: {string_len})")

            if end_idx > string_len:
                raise AIFPLEvalError(f"substring end index out of range: {end_idx} (string length: {string_len})")

            if start_idx > end_idx:
                raise AIFPLEvalError(f"substring start index ({start_idx}) cannot be greater than end index ({end_idx})")

            return AIFPLString(string_arg.value[start_idx:end_idx])

        if operator == 'string-ref':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-ref requires exactly 2 arguments, got {len(args)}")

            string_arg = self._ensure_string(args[0], operator)
            index = self._ensure_integer(args[1], operator)

            # Check for negative index (not allowed in AIFPL)
            if index < 0:
                raise AIFPLEvalError(f"string-ref index out of range: {index}")

            string_len = len(string_arg.value)
            if index >= string_len:
                raise AIFPLEvalError(f"string-ref index out of range: {index}")

            return AIFPLString(string_arg.value[index])

        if operator == 'string->number':
            if len(args) != 1:
                raise AIFPLEvalError(f"string->number requires exactly 1 argument, got {len(args)}")
            string_arg = self._ensure_string(args[0], operator)

            try:
                # Try to parse as integer first
                if '.' not in string_arg.value and 'e' not in string_arg.value.lower() and 'j' not in string_arg.value.lower():
                    return AIFPLNumber(int(string_arg.value))

                # Try complex number
                if 'j' in string_arg.value.lower():
                    return AIFPLNumber(complex(string_arg.value))

                # Otherwise float
                return AIFPLNumber(float(string_arg.value))

            except ValueError as e:
                raise AIFPLEvalError(f"Cannot convert string to number: '{string_arg.value}'") from e

        # String predicates
        if operator == 'string-contains?':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-contains? requires exactly 2 arguments, got {len(args)}")

            string_arg, substring = [self._ensure_string(arg, operator) for arg in args]
            return AIFPLBoolean(substring.value in string_arg.value)

        if operator == 'string-prefix?':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-prefix? requires exactly 2 arguments, got {len(args)}")

            string_arg, prefix = [self._ensure_string(arg, operator) for arg in args]
            return AIFPLBoolean(string_arg.value.startswith(prefix.value))

        if operator == 'string-suffix?':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-suffix? requires exactly 2 arguments, got {len(args)}")

            string_arg, suffix = [self._ensure_string(arg, operator) for arg in args]
            return AIFPLBoolean(string_arg.value.endswith(suffix.value))

        if operator == 'string=?':
            if len(args) < 2:
                raise AIFPLEvalError(f"string=? requires at least 2 arguments, got {len(args)}")

            string_args = [self._ensure_string(arg, operator) for arg in args]
            first_val = string_args[0].value
            return AIFPLBoolean(all(arg.value == first_val for arg in string_args[1:]))

        raise AIFPLEvalError(f"Unknown string operator: '{operator}'")

    def _apply_conversion_to_string(self, operator: str, args: List[AIFPLValue]) -> AIFPLString:
        """Apply functions that convert values to strings."""
        if operator == 'number->string':
            if len(args) != 1:
                raise AIFPLEvalError(f"number->string requires exactly 1 argument, got {len(args)}")

            num_arg = self._ensure_number(args[0], operator)
            return AIFPLString(str(num_arg.value))

        if operator == 'list->string':
            if len(args) != 1:
                raise AIFPLEvalError(f"list->string requires exactly 1 argument, got {len(args)}")

            list_arg = self._ensure_list(args[0], operator)

            try:
                return AIFPLString(''.join(str(elem.to_python()) for elem in list_arg.elements))

            except Exception as e:
                raise AIFPLEvalError(f"Cannot convert list to string: {e}") from e

        if operator == 'string-join':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-join requires exactly 2 arguments, got {len(args)}")
            list_arg = self._ensure_list(args[0], operator)
            separator = self._ensure_string(args[1], operator)

            # Ensure all list elements are strings
            str_items = []
            for item in list_arg.elements:
                if not isinstance(item, AIFPLString):
                    raise AIFPLEvalError(f"string-join requires list of strings, found {item.type_name()}")

                str_items.append(item.value)

            return AIFPLString(separator.value.join(str_items))

        raise AIFPLEvalError(f"Unknown string conversion function: '{operator}'")

    def _apply_list_returning_function(self, operator: str, args: List[AIFPLValue]) -> AIFPLList:
        """Apply functions that return lists."""
        if operator == 'string->list':
            if len(args) != 1:
                raise AIFPLEvalError(f"string->list requires exactly 1 argument, got {len(args)}")

            string_arg = self._ensure_string(args[0], operator)
            elements = tuple(AIFPLString(char) for char in string_arg.value)
            return AIFPLList(elements)

        if operator == 'string-split':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-split requires exactly 2 arguments, got {len(args)}")
            string_arg, delimiter = [self._ensure_string(arg, operator) for arg in args]

            # Handle empty separator case - split into individual characters
            if delimiter.value == "":
                return self._apply_list_returning_function('string->list', [string_arg])

            parts = string_arg.value.split(delimiter.value)
            elements = tuple(AIFPLString(part) for part in parts)
            return AIFPLList(elements)

        raise AIFPLEvalError(f"Unknown list-returning function: '{operator}'")

    def _apply_string_function(self, operator: str, args: List[AIFPLValue]) -> AIFPLString:
        """Apply functions that return strings."""
        if operator == 'bin':
            if len(args) != 1:
                raise AIFPLEvalError(f"bin requires exactly 1 argument, got {len(args)}")

            int_val = self._ensure_integer(args[0], operator)
            return AIFPLString(bin(int_val))

        if operator == 'hex':
            if len(args) != 1:
                raise AIFPLEvalError(f"hex requires exactly 1 argument, got {len(args)}")

            int_val = self._ensure_integer(args[0], operator)
            return AIFPLString(hex(int_val))

        if operator == 'oct':
            if len(args) != 1:
                raise AIFPLEvalError(f"oct requires exactly 1 argument, got {len(args)}")

            int_val = self._ensure_integer(args[0], operator)
            return AIFPLString(oct(int_val))

        raise AIFPLEvalError(f"Unknown string function: '{operator}'")

    def _apply_bitwise_operator(self, operator: str, args: List[AIFPLValue]) -> AIFPLNumber:
        """Apply bitwise operators (require integer arguments)."""
        # Convert all arguments to integers
        int_args = [self._ensure_integer(arg, operator) for arg in args]

        if operator == 'bit-or':
            result = int_args[0]
            for arg in int_args[1:]:
                result |= arg

            return AIFPLNumber(result)

        if operator == 'bit-and':
            result = int_args[0]
            for arg in int_args[1:]:
                result &= arg

            return AIFPLNumber(result)

        if operator == 'bit-xor':
            result = int_args[0]
            for arg in int_args[1:]:
                result ^= arg

            return AIFPLNumber(result)

        if operator == 'bit-not':
            if len(int_args) != 1:
                raise AIFPLEvalError(f"bit-not requires exactly 1 argument, got {len(int_args)}")

            return AIFPLNumber(~int_args[0])

        if operator == 'bit-shift-left':
            if len(int_args) != 2:
                raise AIFPLEvalError(f"bit-shift-left requires exactly 2 arguments, got {len(int_args)}")

            return AIFPLNumber(int_args[0] << int_args[1])

        if operator == 'bit-shift-right':
            if len(int_args) != 2:
                raise AIFPLEvalError(f"bit-shift-right requires exactly 2 arguments, got {len(int_args)}")

            return AIFPLNumber(int_args[0] >> int_args[1])

        raise AIFPLEvalError(f"Unknown bitwise operator: '{operator}'")

    def _apply_real_only_function(self, operator: str, args: List[AIFPLValue]) -> AIFPLNumber:
        """Apply functions that only work with real numbers."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function '{operator}' requires exactly 1 argument, got {len(args)}")

        num_arg = self._ensure_number(args[0], operator)

        # Extract real part if complex
        if isinstance(num_arg.value, complex):
            if abs(num_arg.value.imag) >= self.imaginary_tolerance:
                raise AIFPLEvalError(f"Function '{operator}' does not support complex numbers")

            val = num_arg.value.real

        else:
            val = num_arg.value

        if operator == 'round':
            return AIFPLNumber(round(val))

        if operator == 'floor':
            return AIFPLNumber(math.floor(val))

        if operator == 'ceil':
            return AIFPLNumber(math.ceil(val))

        raise AIFPLEvalError(f"Unknown real-only function: '{operator}'")

    def _apply_integer_only_function(self, operator: str, args: List[AIFPLValue]) -> AIFPLValue:
        """Apply functions that require integer arguments."""
        # These are handled in _apply_string_function for base conversions
        raise AIFPLEvalError(f"Unknown integer-only function: '{operator}'")

    def _apply_mathematical_operator(self, operator: str, op_def: dict, args: List[AIFPLValue]) -> AIFPLNumber:
        """Apply mathematical operators - enhanced version."""
        # Ensure all args are numbers
        num_args = [self._ensure_number(arg, operator) for arg in args]

        if operator == '+':
            if not num_args:
                return AIFPLNumber(op_def.get('identity', 0))

            result = sum(arg.value for arg in num_args)
            return AIFPLNumber(result)

        if operator == '-':
            if len(num_args) == 1:
                return AIFPLNumber(-num_args[0].value)

            result = num_args[0].value
            for arg in num_args[1:]:
                result -= arg.value

            return AIFPLNumber(result)

        if operator == '*':
            if not num_args:
                return AIFPLNumber(op_def.get('identity', 1))

            result = num_args[0].value
            for arg in num_args[1:]:
                result *= arg.value

            return AIFPLNumber(result)

        if operator == '/':
            # Check for division by zero
            for i, arg in enumerate(num_args[1:], 1):
                if arg.value == 0:
                    raise AIFPLEvalError(f"Division by zero at argument {i+1}")

            result = num_args[0].value
            for arg in num_args[1:]:
                result /= arg.value

            return AIFPLNumber(result)

        if operator == '//':
            if len(num_args) != 2:
                raise AIFPLEvalError(f"Floor division requires exactly 2 arguments, got {len(num_args)}")

            left, right = num_args[0].value, num_args[1].value
            if right == 0:
                raise AIFPLEvalError("Division by zero")

            return AIFPLNumber(left // right)

        if operator == '%':
            if len(num_args) != 2:
                raise AIFPLEvalError(f"Modulo requires exactly 2 arguments, got {len(num_args)}")

            left, right = num_args[0].value, num_args[1].value
            if right == 0:
                raise AIFPLEvalError("Modulo by zero")

            return AIFPLNumber(left % right)

        if operator in ['**', 'pow']:
            if len(num_args) != 2:
                raise AIFPLEvalError(f"Power requires exactly 2 arguments, got {len(num_args)}")

            base, exponent = num_args[0].value, num_args[1].value
            return AIFPLNumber(base ** exponent)

        # Mathematical functions
        if operator == 'abs':
            return AIFPLNumber(abs(num_args[0].value))

        if operator == 'sin':
            val = num_args[0].value

            if isinstance(val, complex):
                return AIFPLNumber(cmath.sin(val))

            return AIFPLNumber(math.sin(val))

        if operator == 'cos':
            val = num_args[0].value
            if isinstance(val, complex):
                return AIFPLNumber(cmath.cos(val))

            return AIFPLNumber(math.cos(val))

        if operator == 'tan':
            val = num_args[0].value
            if isinstance(val, complex):
                return AIFPLNumber(cmath.tan(val))

            return AIFPLNumber(math.tan(val))

        if operator == 'log':
            val = num_args[0].value
            if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
                return AIFPLNumber(cmath.log(val))

            return AIFPLNumber(math.log(val))

        if operator == 'log10':
            val = num_args[0].value
            if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
                return AIFPLNumber(cmath.log10(val))

            return AIFPLNumber(math.log10(val))

        if operator == 'exp':
            val = num_args[0].value
            if isinstance(val, complex):
                return AIFPLNumber(cmath.exp(val))

            return AIFPLNumber(math.exp(val))

        if operator == 'sqrt':
            val = num_args[0].value
            if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
                return AIFPLNumber(cmath.sqrt(val))

            return AIFPLNumber(math.sqrt(val))

        if operator == 'min':
            if not num_args:
                raise AIFPLEvalError("min requires at least 1 argument")

            return AIFPLNumber(min(arg.value for arg in num_args))

        if operator == 'max':
            if not num_args:
                raise AIFPLEvalError("max requires at least 1 argument")

            return AIFPLNumber(max(arg.value for arg in num_args))

        # Complex number functions
        if operator == 'real':
            val = num_args[0].value
            if isinstance(val, complex):
                real_part = val.real
                # Convert to int if it's a whole number
                if isinstance(real_part, float) and real_part.is_integer():
                    return AIFPLNumber(int(real_part))

                return AIFPLNumber(real_part)

            # For real numbers, return as-is
            return num_args[0]

        if operator == 'imag':
            val = num_args[0].value
            if isinstance(val, complex):
                imag_part = val.imag
                # Convert to int if it's a whole number
                if isinstance(imag_part, float) and imag_part.is_integer():
                    return AIFPLNumber(int(imag_part))

                return AIFPLNumber(imag_part)

            # For real numbers, imaginary part is 0
            return AIFPLNumber(0)

        if operator == 'complex':
            if len(num_args) != 2:
                raise AIFPLEvalError(f"complex requires exactly 2 arguments, got {len(num_args)}")

            real_part, imag_part = num_args[0].value, num_args[1].value
            return AIFPLNumber(complex(real_part, imag_part))

        raise AIFPLEvalError(f"Unknown mathematical operator: '{operator}'")
