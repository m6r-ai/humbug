"""String and list built-in functions for AIFPL."""

from typing import List, Callable

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex,
    AIFPLString, AIFPLBoolean, AIFPLList, AIFPLAList, AIFPLFunction
)


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

            # AList functions
            'alist': self._builtin_alist,
            'alist-get': self._builtin_alist_get,
            'alist-set': self._builtin_alist_set,
            'alist-has?': self._builtin_alist_has_p,
            'alist-keys': self._builtin_alist_keys,
            'alist-values': self._builtin_alist_values,
            'alist-remove': self._builtin_alist_remove,
            'alist-merge': self._builtin_alist_merge,
            'alist?': self._builtin_alist_p,

            # Range function
            'range': self._builtin_range,
        }

    # String functions
    def _builtin_string_append(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-append function."""
        if not args:
            return AIFPLString("")

        # Ensure all arguments are strings
        string_args = [self._ensure_string(arg, "string-append") for arg in args]
        result = ''.join(arg.value for arg in string_args)
        return AIFPLString(result)

    def _builtin_string_length(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-length function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-length requires exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-length")
        return AIFPLInteger(len(string_arg.value))

    def _builtin_substring(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement substring function."""
        if len(args) != 3:
            raise AIFPLEvalError(f"substring requires exactly 3 arguments, got {len(args)}")

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

    def _builtin_string_upcase(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-upcase function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-upcase requires exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-upcase")
        return AIFPLString(string_arg.value.upper())

    def _builtin_string_downcase(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-downcase function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-downcase requires exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-downcase")
        return AIFPLString(string_arg.value.lower())

    def _builtin_string_ref(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-ref function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-ref requires exactly 2 arguments, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-ref")
        index = self._ensure_integer(args[1], "string-ref")

        # Check for negative index (not allowed in AIFPL)
        if index < 0:
            raise AIFPLEvalError(f"string-ref index out of range: {index}")

        string_len = len(string_arg.value)
        if index >= string_len:
            raise AIFPLEvalError(f"string-ref index out of range: {index}")

        return AIFPLString(string_arg.value[index])

    def _builtin_string_to_number(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string->number function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string->number requires exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string->number")

        try:
            # Try to parse as integer first
            # Phase 2: Return typed numbers
            if '.' not in string_arg.value and 'e' not in string_arg.value.lower() and 'j' not in string_arg.value.lower():
                return AIFPLInteger(int(string_arg.value))

            # Try complex number
            if 'j' in string_arg.value.lower():
                return AIFPLComplex(complex(string_arg.value))

            # Otherwise float
            return AIFPLFloat(float(string_arg.value))

        except ValueError as e:
            raise AIFPLEvalError(f"Cannot convert string to number: '{string_arg.value}'") from e

    def _builtin_number_to_string(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement number->string function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"number->string requires exactly 1 argument, got {len(args)}")

        # Phase 1: Works with both old and new number types via _ensure_number
        num_arg = self._ensure_number(args[0], "number->string")
        return AIFPLString(str(num_arg.value))

    def _builtin_string_trim(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-trim function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-trim requires exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-trim")
        return AIFPLString(string_arg.value.strip())

    def _builtin_string_replace(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-replace function."""
        if len(args) != 3:
            raise AIFPLEvalError(f"string-replace requires exactly 3 arguments, got {len(args)}")

        string_arg, old_str, new_str = [self._ensure_string(arg, "string-replace") for arg in args]
        result = string_arg.value.replace(old_str.value, new_str.value)
        return AIFPLString(result)

    def _builtin_string_contains_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-contains? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-contains? requires exactly 2 arguments, got {len(args)}")

        string_arg, substring = [self._ensure_string(arg, "string-contains?") for arg in args]
        return AIFPLBoolean(substring.value in string_arg.value)

    def _builtin_string_prefix_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-prefix? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-prefix? requires exactly 2 arguments, got {len(args)}")

        string_arg, prefix = [self._ensure_string(arg, "string-prefix?") for arg in args]
        return AIFPLBoolean(string_arg.value.startswith(prefix.value))

    def _builtin_string_suffix_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-suffix? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-suffix? requires exactly 2 arguments, got {len(args)}")

        string_arg, suffix = [self._ensure_string(arg, "string-suffix?") for arg in args]
        return AIFPLBoolean(string_arg.value.endswith(suffix.value))

    def _builtin_string_eq_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string=? function."""
        if len(args) < 2:
            raise AIFPLEvalError(f"string=? requires at least 2 arguments, got {len(args)}")

        string_args = [self._ensure_string(arg, "string=?") for arg in args]
        first_val = string_args[0].value
        return AIFPLBoolean(all(arg.value == first_val for arg in string_args[1:]))

    # List functions
    def _builtin_list(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement list constructor."""
        return AIFPLList(tuple(args))

    def _builtin_cons(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement cons function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"cons requires exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "cons")
        return list_val.cons(item)

    def _builtin_append(self, args: List[AIFPLValue]) -> AIFPLValue:
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

    def _builtin_reverse(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement reverse function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"reverse requires exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "reverse")
        return list_val.reverse()

    def _builtin_first(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement first function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"first requires exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "first")
        try:
            return list_val.first()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_rest(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement rest function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"rest requires exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "rest")
        try:
            return list_val.rest()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_last(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement last function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"last requires exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "last")
        try:
            return list_val.last()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_list_ref(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement list-ref function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"list-ref requires exactly 2 arguments, got {len(args)}")

        list_val = self._ensure_list(args[0], "list-ref")
        index = self._ensure_integer(args[1], "list-ref")

        # Check for negative index (not allowed in AIFPL)
        if index < 0:
            raise AIFPLEvalError(f"list-ref index out of range: {index}")

        try:
            return list_val.get(index)

        except IndexError as e:
            raise AIFPLEvalError(f"list-ref index out of range: {index}") from e

    def _builtin_length(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement length function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"length requires exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "length")
        return AIFPLInteger(list_val.length())

    def _builtin_null_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement null? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"null? requires exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "null?")
        return AIFPLBoolean(list_val.is_empty())

    def _builtin_list_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement list? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"list? requires exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLList))

    def _builtin_member_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement member? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"member? requires exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "member?")
        return AIFPLBoolean(list_val.contains(item))

    def _builtin_remove(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement remove function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"remove requires exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "remove")
        return list_val.remove_all(item)

    def _builtin_position(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement position function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"position requires exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "position")

        pos = list_val.position(item)
        if pos is not None:
            return AIFPLInteger(pos)

        return AIFPLBoolean(False)

    def _builtin_take(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement take function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"take requires exactly 2 arguments, got {len(args)}")

        n = self._ensure_integer(args[0], "take")
        list_val = self._ensure_list(args[1], "take")
        if n < 0:
            raise AIFPLEvalError(f"take count cannot be negative: {n}")

        return list_val.take(n)

    def _builtin_drop(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement drop function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"drop requires exactly 2 arguments, got {len(args)}")

        n = self._ensure_integer(args[0], "drop")
        list_val = self._ensure_list(args[1], "drop")
        if n < 0:
            raise AIFPLEvalError(f"drop count cannot be negative: {n}")

        return list_val.drop(n)

    def _builtin_string_to_list(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string->list function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string->list requires exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string->list")
        elements = tuple(AIFPLString(char) for char in string_arg.value)
        return AIFPLList(elements)

    def _builtin_list_to_string(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement list->string function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"list->string requires exactly 1 argument, got {len(args)}")

        list_arg = self._ensure_list(args[0], "list->string")
        return AIFPLString(''.join(str(elem.to_python()) for elem in list_arg.elements))

    def _builtin_string_split(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-split function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-split requires exactly 2 arguments, got {len(args)}")

        string_arg, delimiter = [self._ensure_string(arg, "string-split") for arg in args]

        # Handle empty separator case - split into individual characters
        if delimiter.value == "":
            return self._builtin_string_to_list([string_arg])

        parts = string_arg.value.split(delimiter.value)
        elements = tuple(AIFPLString(part) for part in parts)
        return AIFPLList(elements)

    def _builtin_string_join(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string-join function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-join requires exactly 2 arguments, got {len(args)}")

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
    def _builtin_number_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement number? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"number? requires exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], (AIFPLInteger, AIFPLFloat, AIFPLComplex)))

    def _builtin_integer_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement integer? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"integer? requires exactly 1 argument, got {len(args)}")

        arg = args[0]
        if isinstance(arg, AIFPLInteger):
            return AIFPLBoolean(True)
        return AIFPLBoolean(False)

    def _builtin_float_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement float? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"float? requires exactly 1 argument, got {len(args)}")

        arg = args[0]
        if isinstance(arg, AIFPLFloat):
            return AIFPLBoolean(True)
        return AIFPLBoolean(False)

    def _builtin_complex_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement complex? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"complex? requires exactly 1 argument, got {len(args)}")

        arg = args[0]
        if isinstance(arg, AIFPLComplex):
            return AIFPLBoolean(True)
        return AIFPLBoolean(False)

    def _builtin_string_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement string? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string? requires exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLString))

    def _builtin_boolean_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement boolean? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"boolean? requires exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLBoolean))

    def _builtin_function_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Implement function? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"function? requires exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLFunction))

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

    def _ensure_number(self, value: AIFPLValue, function_name: str) -> AIFPLInteger | AIFPLFloat | AIFPLComplex:
        """Ensure value is a number (old or new type), raise error if not."""
        if not isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            raise AIFPLEvalError(f"Function '{function_name}' requires numeric arguments, got {value.type_name()}")

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


    # AList functions
    def _builtin_alist(self, args: List[AIFPLValue]) -> AIFPLValue:
        """
        Create alist from key-value pairs: (alist (list key1 val1) (list key2 val2) ...)

        Each argument must be a 2-element list representing a key-value pair.
        """
        pairs = []

        for i, arg in enumerate(args):
            # Each arg must be an evaluated 2-element list
            if not isinstance(arg, AIFPLList):
                raise AIFPLEvalError(
                    message=f"AList pair {i+1} must be a list",
                    received=f"Pair {i+1}: {arg.type_name()}",
                    expected="2-element list: (list key value)",
                    example='(alist (list "name" "Alice") (list "age" 30))',
                    suggestion="Each pair should be created with (list key value)"
                )

            if len(arg.elements) != 2:
                raise AIFPLEvalError(
                    message=f"AList pair {i+1} must have exactly 2 elements",
                    received=f"Pair {i+1} has {len(arg.elements)} elements",
                    expected="2 elements: (list key value)",
                    example='(alist (list "name" "Alice") (list "age" 30))',
                    suggestion="Each pair needs exactly one key and one value"
                )

            # Extract key and value from the evaluated list
            key = arg.elements[0]
            value = arg.elements[1]
            pairs.append((key, value))

        return AIFPLAList(tuple(pairs))

    def _builtin_alist_get(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Get value from alist: (alist-get my-alist key [default])"""
        if len(args) < 2 or len(args) > 3:
            raise AIFPLEvalError(
                message="alist-get requires 2 or 3 arguments",
                received=f"Got {len(args)} arguments",
                expected="2 or 3 arguments: (alist-get alist key) or (alist-get alist key default)",
                example='(alist-get person "name") or (alist-get person "email" "none")',
                suggestion="Provide alist, key, and optionally a default value"
            )

        alist_val = args[0]
        key = args[1]
        default = args[2] if len(args) == 3 else AIFPLBoolean(False)

        if not isinstance(alist_val, AIFPLAList):
            raise AIFPLEvalError(
                message="First argument must be an alist",
                received=f"Got: {alist_val.type_name()}",
                expected="alist",
                example='(alist-get (alist ("name" "Alice")) "name")',
                suggestion="Use (alist ...) to create an alist"
            )

        result = alist_val.get(key)
        return result if result is not None else default

    def _builtin_alist_set(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Set key in alist (returns new alist): (alist-set my-alist key value)"""
        if len(args) != 3:
            raise AIFPLEvalError(
                message="alist-set requires exactly 3 arguments",
                received=f"Got {len(args)} arguments",
                expected="3 arguments: (alist-set alist key value)",
                example='(alist-set person "age" 31)',
                suggestion="Provide alist, key, and new value"
            )

        alist_val, key, value = args

        if not isinstance(alist_val, AIFPLAList):
            raise AIFPLEvalError(
                message="First argument must be an alist",
                received=f"Got: {alist_val.type_name()}",
                expected="alist",
                suggestion="Use (alist ...) to create an alist"
            )

        return alist_val.set(key, value)

    def _builtin_alist_has_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Check if alist has key: (alist-has? my-alist key)"""
        if len(args) != 2:
            raise AIFPLEvalError(
                message="alist-has? requires exactly 2 arguments",
                received=f"Got {len(args)} arguments",
                expected="2 arguments: (alist-has? alist key)",
                example='(alist-has? person "email")',
                suggestion="Provide alist and key to check"
            )

        alist_val, key = args

        if not isinstance(alist_val, AIFPLAList):
            raise AIFPLEvalError(
                message="First argument must be an alist",
                received=f"Got: {alist_val.type_name()}",
                expected="alist"
            )

        return AIFPLBoolean(alist_val.has_key(key))

    def _builtin_alist_keys(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Get all keys from alist: (alist-keys my-alist)"""
        if len(args) != 1:
            raise AIFPLEvalError(
                message="alist-keys requires exactly 1 argument",
                received=f"Got {len(args)} arguments",
                expected="1 argument: (alist-keys alist)",
                example='(alist-keys person)',
                suggestion="Provide an alist"
            )

        alist_val = args[0]

        if not isinstance(alist_val, AIFPLAList):
            raise AIFPLEvalError(
                message="Argument must be an alist",
                received=f"Got: {alist_val.type_name()}",
                expected="alist"
            )

        return AIFPLList(alist_val.keys())

    def _builtin_alist_values(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Get all values from alist: (alist-values my-alist)"""
        if len(args) != 1:
            raise AIFPLEvalError(
                message="alist-values requires exactly 1 argument",
                received=f"Got {len(args)} arguments",
                expected="1 argument: (alist-values alist)",
                example='(alist-values person)',
                suggestion="Provide an alist"
            )

        alist_val = args[0]

        if not isinstance(alist_val, AIFPLAList):
            raise AIFPLEvalError(
                message="Argument must be an alist",
                received=f"Got: {alist_val.type_name()}",
                expected="alist"
            )

        return AIFPLList(alist_val.values())

    def _builtin_alist_remove(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Remove key from alist (returns new alist): (alist-remove my-alist key)"""
        if len(args) != 2:
            raise AIFPLEvalError(
                message="alist-remove requires exactly 2 arguments",
                received=f"Got {len(args)} arguments",
                expected="2 arguments: (alist-remove alist key)",
                example='(alist-remove person "age")',
                suggestion="Provide alist and key to remove"
            )

        alist_val, key = args

        if not isinstance(alist_val, AIFPLAList):
            raise AIFPLEvalError(
                message="First argument must be an alist",
                received=f"Got: {alist_val.type_name()}",
                expected="alist"
            )

        return alist_val.remove(key)

    def _builtin_alist_merge(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Merge two alists (second wins on conflicts): (alist-merge alist1 alist2)"""
        if len(args) != 2:
            raise AIFPLEvalError(
                message="alist-merge requires exactly 2 arguments",
                received=f"Got {len(args)} arguments",
                expected="2 arguments: (alist-merge alist1 alist2)",
                example='(alist-merge defaults config)',
                suggestion="Provide two alists to merge"
            )

        alist1, alist2 = args

        if not isinstance(alist1, AIFPLAList):
            raise AIFPLEvalError(
                message="First argument must be an alist",
                received=f"Got: {alist1.type_name()}",
                expected="alist"
            )

        if not isinstance(alist2, AIFPLAList):
            raise AIFPLEvalError(
                message="Second argument must be an alist",
                received=f"Got: {alist2.type_name()}",
                expected="alist"
            )

        return alist1.merge(alist2)

    def _builtin_alist_p(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Check if value is an alist: (alist? value)"""
        if len(args) != 1:
            raise AIFPLEvalError(
                message="alist? requires exactly 1 argument",
                received=f"Got {len(args)} arguments",
                expected="1 argument: (alist? value)",
                example='(alist? my-data)',
                suggestion="Provide a value to check"
            )

        return AIFPLBoolean(isinstance(args[0], AIFPLAList))

    def _builtin_range(self, args: List[AIFPLValue]) -> AIFPLValue:
        """Generate range of numbers: (range start end) or (range start end step)"""
        if len(args) < 2 or len(args) > 3:
            raise AIFPLEvalError(
                message="Range function has wrong number of arguments",
                received=f"Got {len(args)} arguments",
                expected="2 or 3 arguments: (range start end) or (range start end step)",
                example="(range 1 5) or (range 0 10 2)",
                suggestion="Range needs start and end, optionally step"
            )

        start_val = args[0]
        end_val = args[1]

        if not isinstance(start_val, AIFPLInteger):
            raise AIFPLEvalError(
                message="Range start must be an integer",
                received=f"Start: {start_val.type_name()}",
                expected="Number (integer)",
                example="(range 1 5)",
                suggestion="Use numeric values for range bounds"
            )

        if not isinstance(end_val, AIFPLInteger):
            raise AIFPLEvalError(
                message="Range end must be an integer",
                received=f"End: {end_val.type_name()}",
                expected="Number (integer)",
                example="(range 1 5)",
                suggestion="Use numeric values for range bounds"
            )

        start_int = int(start_val.value.real) if isinstance(start_val.value, complex) else int(start_val.value)
        end_int = int(end_val.value.real) if isinstance(end_val.value, complex) else int(end_val.value)

        if len(args) == 3:
            step_val = args[2]
            if not isinstance(step_val, AIFPLInteger):
                raise AIFPLEvalError(
                    message="Range step must be an integer",
                    received=f"Step: {step_val.type_name()}",
                    expected="Number (integer)",
                    example="(range 0 10 2)",
                    suggestion="Use numeric values for range parameters"
                )
            step_int = int(step_val.value.real) if isinstance(step_val.value, complex) else int(step_val.value)

        else:
            step_int = 1

        if step_int == 0:
            raise AIFPLEvalError(
                message="Range step cannot be zero",
                received="Step: 0",
                expected="Non-zero integer",
                example="(range 0 10 2) or (range 10 0 -1)",
                suggestion="Use positive step for ascending range, negative for descending"
            )

        range_values = list(range(start_int, end_int, step_int))
        elements = tuple(AIFPLInteger(val) for val in range_values)
        return AIFPLList(elements)
