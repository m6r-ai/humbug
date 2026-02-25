"""AIFPL Value hierarchy - immutable runtime value types.

These are lightweight runtime values used by the VM and bytecode.
They do NOT carry source location metadata - that's only in AIFPLASTNode.
This separation keeps runtime values fast and memory-efficient.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, List, Tuple

from aifpl.aifpl_error import AIFPLEvalError


@dataclass(frozen=True)
class AIFPLValue(ABC):
    """
    Abstract base class for all AIFPL runtime values.

    All runtime values are immutable and lightweight (no metadata).
    """

    @abstractmethod
    def to_python(self) -> Any:
        """Convert to Python value for operations."""

    @abstractmethod
    def type_name(self) -> str:
        """Return AIFPL type name for error messages."""

    @abstractmethod
    def describe(self) -> str:
        """Describe the value."""


@dataclass(frozen=True)
class AIFPLInteger(AIFPLValue):
    """Represents integer values."""
    value: int

    def to_python(self) -> int:
        return self.value

    def type_name(self) -> str:
        return "integer"

    def describe(self) -> str:
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values."""
        if not isinstance(other, AIFPLInteger):
            return False

        return self.value == other.value


@dataclass(frozen=True)
class AIFPLFloat(AIFPLValue):
    """Represents floating-point values."""
    value: float

    def to_python(self) -> float:
        return self.value

    def type_name(self) -> str:
        return "float"

    def describe(self) -> str:
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values."""
        if not isinstance(other, AIFPLFloat):
            return False

        return self.value == other.value


@dataclass(frozen=True)
class AIFPLComplex(AIFPLValue):
    """Represents complex number values."""
    value: complex

    def to_python(self) -> complex:
        return self.value

    def type_name(self) -> str:
        return "complex"

    def describe(self) -> str:
        r = self.value.real
        i = self.value.imag
        def _fmt_float(x: float) -> str:
            """Format a float component: use integer notation when exact, else full float."""
            try:
                as_int = int(x)
                if x == as_int:
                    return str(as_int)
            except (ValueError, OverflowError):
                pass
            return str(x)

        if r == 0.0 and i == 0.0:
            return "0+0j"
        if r == 0.0:
            # Pure imaginary: omit the real part entirely
            return f"{_fmt_float(i)}j"
        # General case: always show both parts with explicit sign on imaginary
        real_str = _fmt_float(r)
        if i >= 0.0:
            imag_str = f"+{_fmt_float(i)}j"
        else:
            imag_str = f"{_fmt_float(i)}j"
        return f"{real_str}{imag_str}"

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values."""
        if not isinstance(other, AIFPLComplex):
            return False

        return self.value == other.value


@dataclass(frozen=True)
class AIFPLString(AIFPLValue):
    """Represents string values."""
    value: str

    def to_python(self) -> str:
        return self.value

    def type_name(self) -> str:
        return "string"

    def _escape_string(self, s: str) -> str:
        """Escape a string for display format."""
        result = []
        for char in s:
            if char == '"':
                result.append('\\"')

            elif char == '\\':
                result.append('\\\\')

            elif char == '\n':
                result.append('\\n')

            elif char == '\t':
                result.append('\\t')

            elif char == '\r':
                result.append('\\r')

            elif ord(char) < 32:  # Other control characters
                result.append(f'\\u{ord(char):04x}')

            else:
                result.append(char)  # Keep Unicode as-is

        return ''.join(result)

    def describe(self) -> str:
        escaped_content = self._escape_string(self.value)
        return f'"{escaped_content}"'

    def __eq__(self, other: Any) -> bool:
        """Compare string values, ignoring metadata (line, column)."""
        if not isinstance(other, AIFPLString):
            return False

        return self.value == other.value


@dataclass(frozen=True)
class AIFPLBoolean(AIFPLValue):
    """Represents boolean values."""
    value: bool

    def to_python(self) -> bool:
        return self.value

    def type_name(self) -> str:
        return "boolean"

    def describe(self) -> str:
        return "#t" if self.value else "#f"

    def __eq__(self, other: Any) -> bool:
        """Compare boolean values, ignoring metadata (line, column)."""
        if not isinstance(other, AIFPLBoolean):
            return False

        return self.value == other.value


@dataclass(frozen=True)
class AIFPLSymbol(AIFPLValue):
    """Represents symbols that require environment lookup."""
    name: str

    def to_python(self) -> str:
        """Symbols convert to their name string."""
        return self.name

    def type_name(self) -> str:
        return "symbol"

    def describe(self) -> str:
        return self.name

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'AIFPLSymbol({self.name!r})'


@dataclass(frozen=True)
class AIFPLList(AIFPLValue):
    """Represents lists of AIFPL values."""
    elements: Tuple[AIFPLValue, ...] = ()

    def to_python(self) -> List[Any]:
        """Convert to Python list with Python values."""
        return [elem.to_python() for elem in self.elements]

    def type_name(self) -> str:
        return "list"

    def describe(self) -> str:
        # Format list: (element1 element2 ...)
        if self.is_empty():
            return "()"

        formatted_elements = []
        for element in self.elements:
            formatted_elements.append(element.describe())

        return f"({' '.join(formatted_elements)})"

    def length(self) -> int:
        """Return the length of the list."""
        return len(self.elements)

    def is_empty(self) -> bool:
        """Check if the list is empty."""
        return len(self.elements) == 0

    def first(self) -> AIFPLValue:
        """Get the first element (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get first element of empty list")

        return self.elements[0]

    def rest(self) -> 'AIFPLList':
        """Get all elements except the first (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get rest of empty list")

        return AIFPLList(self.elements[1:])

    def last(self) -> AIFPLValue:
        """Get the last element (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get last element of empty list")

        return self.elements[-1]

    def cons(self, element: AIFPLValue) -> 'AIFPLList':
        """Prepend an element to the front of the list."""
        return AIFPLList((element,) + self.elements)

    def append_list(self, other: 'AIFPLList') -> 'AIFPLList':
        """Append another list to this one."""
        return AIFPLList(self.elements + other.elements)

    def reverse(self) -> 'AIFPLList':
        """Return a reversed copy of the list."""
        return AIFPLList(tuple(reversed(self.elements)))

    def get(self, index: int) -> AIFPLValue:
        """Get element at index (raises IndexError if out of bounds)."""
        return self.elements[index]

    def contains(self, value: AIFPLValue) -> bool:
        """Check if the list contains a value (using AIFPL equality)."""
        return value in self.elements

    def remove_all(self, value: AIFPLValue) -> 'AIFPLList':
        """Remove all occurrences of a value."""
        new_elements = tuple(elem for elem in self.elements if elem != value)
        return AIFPLList(new_elements)

    def position(self, value: AIFPLValue) -> int | None:
        """Find the first position of a value, or None if not found."""
        for i, elem in enumerate(self.elements):
            if elem == value:
                return i

        return None

    def take(self, n: int) -> 'AIFPLList':
        """Take the first n elements."""
        return AIFPLList(self.elements[:n])

    def drop(self, n: int) -> 'AIFPLList':
        """Drop the first n elements."""
        return AIFPLList(self.elements[n:])


@dataclass(frozen=True)
class AIFPLAList(AIFPLValue):
    """
    Represents association lists (alists) - immutable key-value mappings.

    Internally uses a dict for O(1) lookups while maintaining insertion order.
    Keys must be hashable (strings, numbers, booleans, symbols).
    """
    pairs: Tuple[Tuple[AIFPLValue, AIFPLValue], ...] = ()
    _lookup: dict = field(default_factory=dict, init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        """Build internal lookup dict after initialization."""
        # Use object.__setattr__ because dataclass is frozen
        lookup = {}
        for key, value in self.pairs:
            hashable_key = self._to_hashable_key(key)
            lookup[hashable_key] = (key, value)
        object.__setattr__(self, '_lookup', lookup)

    def to_python(self) -> dict:
        """Convert to Python dict."""
        result = {}
        for key, value in self.pairs:
            # Use string representation for Python dict keys
            if isinstance(key, AIFPLString):
                py_key = key.value

            elif isinstance(key, AIFPLSymbol):
                py_key = key.name

            else:
                py_key = str(key.to_python())

            result[py_key] = value.to_python()

        return result

    def type_name(self) -> str:
        """Return type name for error messages."""
        return "alist"

    def describe(self) -> str:
        # Format alist with curly braces: {(key1 val1) (key2 val2) ...}
        if self.is_empty():
            return "{}"

        formatted_pairs = []
        for key, value in self.pairs:
            formatted_key = key.describe()
            formatted_value = value.describe()
            formatted_pairs.append(f"({formatted_key} {formatted_value})")

        pairs_str = ' '.join(formatted_pairs)
        return f"{{{pairs_str}}}"

    def get(self, key: AIFPLValue) -> AIFPLValue | None:
        """Get value by key, returns None if not found."""
        hashable_key = self._to_hashable_key(key)
        if hashable_key in self._lookup:
            _, value = self._lookup[hashable_key]
            return value

        return None

    def has_key(self, key: AIFPLValue) -> bool:
        """Check if key exists."""
        hashable_key = self._to_hashable_key(key)
        return hashable_key in self._lookup

    def set(self, key: AIFPLValue, value: AIFPLValue) -> 'AIFPLAList':
        """Return new alist with key set (immutable update)."""
        hashable_key = self._to_hashable_key(key)

        # Build new pairs list, replacing or appending
        new_pairs = []
        found = False

        for k, v in self.pairs:
            if self._to_hashable_key(k) == hashable_key:
                new_pairs.append((key, value))  # Replace with new value
                found = True

            else:
                new_pairs.append((k, v))

        if not found:
            new_pairs.append((key, value))  # Append new pair

        return AIFPLAList(tuple(new_pairs))

    def remove(self, key: AIFPLValue) -> 'AIFPLAList':
        """Return new alist without key."""
        hashable_key = self._to_hashable_key(key)
        new_pairs = tuple(
            (k, v) for k, v in self.pairs
            if self._to_hashable_key(k) != hashable_key
        )
        return AIFPLAList(new_pairs)

    def keys(self) -> Tuple[AIFPLValue, ...]:
        """Get all keys in insertion order."""
        return tuple(k for k, _ in self.pairs)

    def values(self) -> Tuple[AIFPLValue, ...]:
        """Get all values in insertion order."""
        return tuple(v for _, v in self.pairs)

    def merge(self, other: 'AIFPLAList') -> 'AIFPLAList':
        """Merge with another alist (other's values win on conflicts)."""
        # Start with self's pairs
        result_dict = {}
        for k, v in self.pairs:
            hashable_key = self._to_hashable_key(k)
            result_dict[hashable_key] = (k, v)

        # Override/add from other
        for k, v in other.pairs:
            hashable_key = self._to_hashable_key(k)
            result_dict[hashable_key] = (k, v)

        # Preserve insertion order: self's keys first, then other's new keys
        new_pairs = []
        seen = set()

        # Add all of self's keys (with potentially updated values)
        for k, _ in self.pairs:
            hashable_key = self._to_hashable_key(k)
            new_pairs.append(result_dict[hashable_key])
            seen.add(hashable_key)

        # Add other's keys that weren't in self
        for k, v in other.pairs:
            hashable_key = self._to_hashable_key(k)
            if hashable_key not in seen:
                new_pairs.append((k, v))

        return AIFPLAList(tuple(new_pairs))

    def length(self) -> int:
        """Number of key-value pairs."""
        return len(self.pairs)

    def is_empty(self) -> bool:
        """Check if alist is empty."""
        return len(self.pairs) == 0

    @staticmethod
    def _to_hashable_key(key: AIFPLValue) -> Tuple[str, Any]:
        """Convert AIFPL key to hashable Python value."""
        if isinstance(key, AIFPLString):
            return ('str', key.value)

        if isinstance(key, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            return ('num', key.value)

        if isinstance(key, AIFPLBoolean):
            return ('bool', key.value)

        if isinstance(key, AIFPLSymbol):
            return ('sym', key.name)

        raise AIFPLEvalError(
            message="AList keys must be strings, numbers, booleans, or symbols",
            received=f"Key type: {key.type_name()}",
            example='(alist ("name" "Alice") ("age" 30))',
            suggestion="Use strings for most keys"
        )


@dataclass(frozen=True)
class AIFPLFunction(AIFPLValue):
    """
    Represents a function (both user-defined lambdas and builtins).

    This is a first-class value that can be passed around as a value.
    """
    parameters: Tuple[str, ...]
    name: str | None = None
    bytecode: Any = None  # CodeObject for bytecode-compiled functions
    captured_values: Tuple[Any, ...] = ()  # Captured free variables for closures
    is_variadic: bool = False  # True if function accepts variable number of args
    parent_frame: Any = None  # Parent frame for LOAD_PARENT_VAR (lexical parent)

    def to_python(self) -> 'AIFPLFunction | str':
        """Functions return themselves (or their name for builtins as string)."""
        return self

    def type_name(self) -> str:
        return "function"

    def describe(self) -> str:
        """Return a human-readable description of this function."""
        param_str = ', '.join(self.parameters)
        if self.is_variadic and len(self.parameters) > 0:
            # Last parameter is variadic (rest parameter)
            regular_params = ', '.join(self.parameters[:-1]) if len(self.parameters) > 1 else ''
            rest_param = self.parameters[-1]
            param_str = f"{regular_params} . {rest_param}".strip(' .')

        return f"<lambda ({param_str})>"
