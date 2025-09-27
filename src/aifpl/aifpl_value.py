"""AIFPL Value hierarchy - immutable value types for the language."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any, List, Tuple, Union, Callable

from aifpl.aifpl_error import AIFPLEvalError


class AIFPLValue(ABC):
    """
    Abstract base class for all AIFPL values.

    All AIFPL values are immutable.
    """

    @abstractmethod
    def to_python(self) -> Any:
        """Convert to Python value for operations."""

    @abstractmethod
    def type_name(self) -> str:
        """Return AIFPL type name for error messages."""


@dataclass(frozen=True)
class AIFPLNumber(AIFPLValue):
    """Represents numeric values: integers, floats, complex numbers."""
    value: Union[int, float, complex]

    def to_python(self) -> Union[int, float, complex]:
        return self.value

    def type_name(self) -> str:
        if isinstance(self.value, int):
            return "integer"

        if isinstance(self.value, float):
            return "float"

        return "complex"

    def is_integer(self) -> bool:
        """Check if this number is an integer."""
        return isinstance(self.value, int)

    def is_float(self) -> bool:
        """Check if this number is a float."""
        return isinstance(self.value, float)

    def is_complex(self) -> bool:
        """Check if this number is complex."""
        return isinstance(self.value, complex)


@dataclass(frozen=True)
class AIFPLString(AIFPLValue):
    """Represents string values."""
    value: str

    def to_python(self) -> str:
        return self.value

    def type_name(self) -> str:
        return "string"


@dataclass(frozen=True)
class AIFPLBoolean(AIFPLValue):
    """Represents boolean values."""
    value: bool

    def to_python(self) -> bool:
        return self.value

    def type_name(self) -> str:
        return "boolean"


@dataclass(frozen=True)
class AIFPLSymbol(AIFPLValue):
    """Represents symbols that require environment lookup."""
    name: str
    position: int = 0

    def to_python(self) -> str:
        """Symbols convert to their name string."""
        return self.name

    def type_name(self) -> str:
        return "symbol"

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
class AIFPLFunction(AIFPLValue):
    """
    Represents a user-defined function (lambda).

    This is a first-class value that can be stored in environments
    and passed around as a value.
    """
    parameters: Tuple[str, ...]
    body: AIFPLValue
    closure_environment: Any  # AIFPLEnvironment, avoiding circular import (this circularity is intentional!)
    name: str | None = None

    def to_python(self) -> 'AIFPLFunction':
        """Functions return themselves as Python values."""
        return self

    def type_name(self) -> str:
        return "function"


class AIFPLBuiltinFunction(AIFPLValue):
    """
    Represents a built-in function that is a first-class function value.
    """

    def __init__(self, name: str, native_impl: Callable):
        """
        Initialize a built-in function.

        Args:
            name: Function name for display and error messages
            native_impl: Python callable that implements the function
        """
        self.name = name
        self.native_impl = native_impl

    def to_python(self) -> str:
        return self.name

    def type_name(self) -> str:
        return "builtin-function"


class AIFPLRecursivePlaceholder(AIFPLValue):
    """Placeholder for recursive bindings that resolves to actual value when accessed."""

    def __init__(self, name: str):
        self._name = name
        self._resolved_value: AIFPLValue | None = None

    def resolve(self, value: AIFPLValue) -> None:
        """Resolve the placeholder to an actual value."""
        self._resolved_value = value

    def get_resolved_value(self) -> AIFPLValue:
        """Get the resolved value, handling recursive calls."""
        if self._resolved_value is None:
            raise AIFPLEvalError(f"Recursive placeholder '{self._name}' accessed before resolution")

        return self._resolved_value

    def to_python(self) -> Any:
        return self.get_resolved_value().to_python()

    def type_name(self) -> str:
        return f"recursive-placeholder({self._name})"
