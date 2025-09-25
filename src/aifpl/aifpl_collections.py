"""String and list built-in functions for AIFPL."""

from typing import List, Callable

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_value import AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLList, AIFPLFunction, AIFPLBuiltinFunction


class AIFPLCollectionsFunctions:
    """String and list built-in functions for AIFPL."""

    def get_functions(self) -> dict[str, Callable]:
        """Return dictionary of collections function implementations."""
        return {
            # String functions
            'string-append': self._builtin_string_append,
            'string-length': self._builtin_string_length,
            'substring': self._builtin_substring,
            'string-upcase': self._builtin_string_upcase,
            'string-downcase': self._builtin_string_downcase,
            'string-ref': self._builtin_string_ref,
            'string->number': self._builtin_string_to_number,
            'number->string': self._builtin_number_to_string,
            'string-trim': self._builtin_string_trim,
            'string-replace': self._builtin_string_replace,

            # String predicates
            'string-contains?': self._builtin_string_contains_p,
            'string-prefix?': self._builtin_string_prefix_p,
            'string-suffix?': self._builtin_string_suffix_p,
            'string=?': self._builtin_string_eq_p,

            # List construction and manipulation functions
            'list': self._builtin_list,
            'cons': self._builtin_cons,
            'append': self._builtin_append,
            'reverse': self._builtin_reverse,

            # List access and property functions
            'first': self._builtin_first,
            'rest': self._builtin_rest,
            'last': self._builtin_last,
            'list-ref': self._builtin_list_ref,
            'length': self._builtin_length,

            # List predicates
            'null?': self._builtin_null_p,
            'list?': self._builtin_list_p,
            'member?': self._builtin_member_p,

            # List utilities
            'remove': self._builtin_remove,
            'position': self._builtin_position,
            'take': self._builtin_take,
            'drop': self._builtin_drop,

            # String-list conversion functions
            'string->list': self._builtin_string_to_list,
            'list->string': self._builtin_list_to_string,
            'string-split': self._builtin_string_split,
            'string-join': self._builtin_string_join,

            # Type predicates
            'number?': self._builtin_number_p,
            'integer?': self._builtin_integer_p,
            'float?': self._builtin_float_p,
            'complex?': self._builtin_complex_p,
            'string?': self._builtin_string_p,
            'boolean?': self._builtin_boolean_p,
            'function?': self._builtin_function_p,
        }

    # String functions
    def _builtin_string_append(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-append function."""
        if not args:
            return AIFPLString("")

        # Ensure all arguments are strings
        string_args = [self._ensure_string(arg, "string-append") for arg in args]
        result = ''.join(arg.value for arg in string_args)
        return AIFPLString(result)

    def _builtin_string_length(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-length function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-length takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-length")
        return AIFPLNumber(len(string_arg.value))

    def _builtin_substring(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement substring function."""
        if len(args) != 3:
            raise AIFPLEvalError(f"substring takes exactly 3 arguments, got {len(args)}")

        string_arg = self._ensure_string(args[0], "substring")
        start_idx = self._ensure_integer(args[1], "substring")
        end_idx = self._ensure_integer(args[2], "substring")

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

    def _builtin_string_upcase(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-upcase function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-upcase takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-upcase")
        return AIFPLString(string_arg.value.upper())

    def _builtin_string_downcase(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-downcase function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-downcase takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-downcase")
        return AIFPLString(string_arg.value.lower())

    def _builtin_string_ref(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-ref function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-ref takes exactly 2 arguments, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-ref")
        index = self._ensure_integer(args[1], "string-ref")

        # Check for negative index (not allowed in AIFPL)
        if index < 0:
            raise AIFPLEvalError(f"string-ref index out of range: {index}")

        string_len = len(string_arg.value)
        if index >= string_len:
            raise AIFPLEvalError(f"string-ref index out of range: {index}")

        return AIFPLString(string_arg.value[index])

    def _builtin_string_to_number(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string->number function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string->number takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string->number")

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

    def _builtin_number_to_string(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement number->string function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"number->string takes exactly 1 argument, got {len(args)}")

        num_arg = self._ensure_number(args[0], "number->string")
        return AIFPLString(str(num_arg.value))

    def _builtin_string_trim(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-trim function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-trim takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-trim")
        return AIFPLString(string_arg.value.strip())

    def _builtin_string_replace(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-replace function."""
        if len(args) != 3:
            raise AIFPLEvalError(f"string-replace takes exactly 3 arguments, got {len(args)}")

        string_arg, old_str, new_str = [self._ensure_string(arg, "string-replace") for arg in args]
        result = string_arg.value.replace(old_str.value, new_str.value)
        return AIFPLString(result)

    def _builtin_string_contains_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-contains? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-contains? takes exactly 2 arguments, got {len(args)}")

        string_arg, substring = [self._ensure_string(arg, "string-contains?") for arg in args]
        return AIFPLBoolean(substring.value in string_arg.value)

    def _builtin_string_prefix_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-prefix? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-prefix? takes exactly 2 arguments, got {len(args)}")

        string_arg, prefix = [self._ensure_string(arg, "string-prefix?") for arg in args]
        return AIFPLBoolean(string_arg.value.startswith(prefix.value))

    def _builtin_string_suffix_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-suffix? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-suffix? takes exactly 2 arguments, got {len(args)}")

        string_arg, suffix = [self._ensure_string(arg, "string-suffix?") for arg in args]
        return AIFPLBoolean(string_arg.value.endswith(suffix.value))

    def _builtin_string_eq_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string=? function."""
        if len(args) < 2:
            raise AIFPLEvalError(f"string=? requires at least 2 arguments, got {len(args)}")

        string_args = [self._ensure_string(arg, "string=?") for arg in args]
        first_val = string_args[0].value
        return AIFPLBoolean(all(arg.value == first_val for arg in string_args[1:]))

    # List functions
    def _builtin_list(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement list constructor."""
        return AIFPLList(tuple(args))

    def _builtin_cons(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement cons function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"cons takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "cons")
        return list_val.cons(item)

    def _builtin_append(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement append function."""
        if len(args) < 2:
            raise AIFPLEvalError(f"append requires at least 2 arguments, got {len(args)}")

        # Validate all arguments are lists
        list_vals = [self._ensure_list(arg, "append") for arg in args]

        # Concatenate all lists
        result = list_vals[0]
        for list_val in list_vals[1:]:
            result = result.append_list(list_val)

        return result

    def _builtin_reverse(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement reverse function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"reverse takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "reverse")
        return list_val.reverse()

    def _builtin_first(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement first function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"first takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "first")
        try:
            return list_val.first()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_rest(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement rest function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"rest takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "rest")
        try:
            return list_val.rest()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_last(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement last function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"last takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "last")
        try:
            return list_val.last()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_list_ref(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement list-ref function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"list-ref takes exactly 2 arguments, got {len(args)}")

        list_val = self._ensure_list(args[0], "list-ref")
        index = self._ensure_integer(args[1], "list-ref")

        # Check for negative index (not allowed in AIFPL)
        if index < 0:
            raise AIFPLEvalError(f"list-ref index out of range: {index}")

        try:
            return list_val.get(index)

        except IndexError as e:
            raise AIFPLEvalError(f"list-ref index out of range: {index}") from e

    def _builtin_length(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement length function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"length takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "length")
        return AIFPLNumber(list_val.length())

    def _builtin_null_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement null? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"null? takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "null?")
        return AIFPLBoolean(list_val.is_empty())

    def _builtin_list_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement list? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"list? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLList))

    def _builtin_member_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement member? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"member? takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "member?")
        return AIFPLBoolean(list_val.contains(item))

    def _builtin_remove(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement remove function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"remove takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "remove")
        return list_val.remove_all(item)

    def _builtin_position(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement position function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"position takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "position")

        pos = list_val.position(item)
        if pos is not None:
            return AIFPLNumber(pos)

        return AIFPLBoolean(False)

    def _builtin_take(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement take function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"take takes exactly 2 arguments, got {len(args)}")

        n = self._ensure_integer(args[0], "take")
        list_val = self._ensure_list(args[1], "take")
        if n < 0:
            raise AIFPLEvalError(f"take count cannot be negative: {n}")

        return list_val.take(n)

    def _builtin_drop(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement drop function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"drop takes exactly 2 arguments, got {len(args)}")

        n = self._ensure_integer(args[0], "drop")
        list_val = self._ensure_list(args[1], "drop")
        if n < 0:
            raise AIFPLEvalError(f"drop count cannot be negative: {n}")

        return list_val.drop(n)

    def _builtin_string_to_list(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string->list function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string->list takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string->list")
        elements = tuple(AIFPLString(char) for char in string_arg.value)
        return AIFPLList(elements)

    def _builtin_list_to_string(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement list->string function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"list->string takes exactly 1 argument, got {len(args)}")

        list_arg = self._ensure_list(args[0], "list->string")

        try:
            return AIFPLString(''.join(str(elem.to_python()) for elem in list_arg.elements))

        except Exception as e:
            raise AIFPLEvalError(f"Cannot convert list to string: {e}") from e

    def _builtin_string_split(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-split function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-split takes exactly 2 arguments, got {len(args)}")

        string_arg, delimiter = [self._ensure_string(arg, "string-split") for arg in args]

        # Handle empty separator case - split into individual characters
        if delimiter.value == "":
            return self._builtin_string_to_list([string_arg], env, depth)

        parts = string_arg.value.split(delimiter.value)
        elements = tuple(AIFPLString(part) for part in parts)
        return AIFPLList(elements)

    def _builtin_string_join(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string-join function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-join takes exactly 2 arguments, got {len(args)}")

        list_arg = self._ensure_list(args[0], "string-join")
        separator = self._ensure_string(args[1], "string-join")

        # Ensure all list elements are strings
        str_items = []
        for item in list_arg.elements:
            if not isinstance(item, AIFPLString):
                raise AIFPLEvalError(f"string-join requires list of strings, found {item.type_name()}")

            str_items.append(item.value)

        return AIFPLString(separator.value.join(str_items))

    # Type predicates
    def _builtin_number_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement number? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"number? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber))

    def _builtin_integer_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement integer? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"integer? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_integer())

    def _builtin_float_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement float? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"float? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_float())

    def _builtin_complex_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement complex? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"complex? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_complex())

    def _builtin_string_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement string? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLString))

    def _builtin_boolean_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement boolean? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"boolean? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLBoolean))

    def _builtin_function_p(self, args: List[AIFPLValue], _env: AIFPLEnvironment, _depth: int) -> AIFPLValue:
        """Implement function? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"function? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], (AIFPLFunction, AIFPLBuiltinFunction)))

    # Helper methods for type checking and conversion
    def _ensure_string(self, value: AIFPLValue, function_name: str) -> AIFPLString:
        """Ensure value is a string, raise error if not."""
        if not isinstance(value, AIFPLString):
            raise AIFPLEvalError(f"Function '{function_name}' requires string arguments, got {value.type_name()}")

        return value

    def _ensure_list(self, value: AIFPLValue, function_name: str) -> AIFPLList:
        """Ensure value is a list, raise error if not."""
        if not isinstance(value, AIFPLList):
            raise AIFPLEvalError(f"Function '{function_name}' requires list arguments, got {value.type_name()}")

        return value

    def _ensure_number(self, value: AIFPLValue, function_name: str) -> AIFPLNumber:
        """Ensure value is a number, raise error if not."""
        if not isinstance(value, AIFPLNumber):
            raise AIFPLEvalError(f"Function '{function_name}' requires numeric arguments, got {value.type_name()}")

        return value

    def _ensure_integer(self, value: AIFPLValue, function_name: str) -> int:
        """Ensure value is an integer, raise error if not."""
        if not isinstance(value, AIFPLNumber) or not value.is_integer():
            raise AIFPLEvalError(f"Function '{function_name}' requires integer arguments, got {value.type_name()}")

        # Type narrowing: we know value.value is int here
        assert isinstance(value.value, int), "is_integer() should guarantee int type"
        return value.value
