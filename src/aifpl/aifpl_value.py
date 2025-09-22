"""AIFPL Value hierarchy - immutable value types for the language."""

from abc import ABC, abstractmethod
from dataclasses import dataclass, replace
from typing import Any, List, Tuple, Union

from aifpl.aifpl_error import AIFPLEvalError


class AIFPLValue(ABC):
    """
    Abstract base class for all AIFPL values.
    
    All AIFPL values are immutable.
    """

    @abstractmethod
    def to_python(self) -> Any:
        """Convert to Python value for operations."""

    @classmethod
    @abstractmethod
    def from_python(cls, value: Any) -> 'AIFPLValue':
        """Create AIFPL value from Python value."""

    @abstractmethod
    def type_name(self) -> str:
        """Return AIFPL type name for error messages."""

    def __eq__(self, other: Any) -> bool:
        """
        Implement AIFPL equality rules.
        
        - Same types: use value equality
        - Numeric types: allow int/float/complex equivalence
        - Different non-numeric types: always False
        """
        if not isinstance(other, AIFPLValue):
            return False

        # Same type - compare values
        if type(self) == type(other):
            return self.to_python() == other.to_python()

        # Both numeric - allow cross-type equality
        if isinstance(self, AIFPLNumber) and isinstance(other, AIFPLNumber):
            return self._numeric_equal(other)

        # Different non-numeric types are never equal
        return False

    def _numeric_equal(self, other: 'AIFPLNumber') -> bool:
        """Check numeric equality allowing type promotion."""
        self_val = self.to_python()
        other_val = other.to_python()

        # Handle complex numbers specially
        if isinstance(self_val, complex) or isinstance(other_val, complex):
            complex_self = complex(self_val) if not isinstance(self_val, complex) else self_val
            complex_other = complex(other_val) if not isinstance(other_val, complex) else other_val

            # Use small tolerance for floating point comparison
            real_equal = abs(complex_self.real - complex_other.real) < 1e-15
            imag_equal = abs(complex_self.imag - complex_other.imag) < 1e-10
            return real_equal and imag_equal

        # Both are real numbers
        return self_val == other_val

    def __hash__(self) -> int:
        """Hash based on value."""
        return hash((type(self), self.to_python()))


@dataclass(frozen=True)
class AIFPLNumber(AIFPLValue):
    """Represents numeric values: integers, floats, complex numbers."""
    value: Union[int, float, complex]

    def __post_init__(self) -> None:
        """Validate that booleans are not treated as numbers."""
        if isinstance(self.value, bool):
            raise ValueError("Booleans are not numbers in AIFPL")

    def to_python(self) -> Union[int, float, complex]:
        return self.value

    @classmethod
    def from_python(cls, value: Union[int, float, complex]) -> 'AIFPLNumber':
        if isinstance(value, bool):
            raise ValueError("Cannot create AIFPLNumber from boolean")

        return cls(value)

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

    @classmethod
    def from_python(cls, value: str) -> 'AIFPLString':
        if not isinstance(value, str):
            raise ValueError(f"Cannot create AIFPLString from {type(value).__name__}")

        return cls(value)

    def type_name(self) -> str:
        return "string"


@dataclass(frozen=True)
class AIFPLBoolean(AIFPLValue):
    """Represents boolean values."""
    value: bool

    def to_python(self) -> bool:
        return self.value

    @classmethod
    def from_python(cls, value: bool) -> 'AIFPLBoolean':
        if not isinstance(value, bool):
            raise ValueError(f"Cannot create AIFPLBoolean from {type(value).__name__}")

        return cls(value)

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

    @classmethod
    def from_python(cls, value: str, position: int = 0) -> 'AIFPLSymbol':
        if not isinstance(value, str):
            raise ValueError(f"Cannot create AIFPLSymbol from {type(value).__name__}")

        return cls(value, position)

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

    @classmethod
    def from_python(cls, value: List[Any]) -> 'AIFPLList':
        """Create AIFPLList from Python list, converting elements."""
        if not isinstance(value, list):
            raise ValueError(f"Cannot create AIFPLList from {type(value).__name__}")

        elements = []
        for item in value:
            elements.append(python_to_aifpl_value(item))

        return cls(tuple(elements))

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

    @classmethod
    def from_python(cls, value: Any) -> 'AIFPLFunction':
        """Cannot create AIFPLFunction from Python values directly."""
        raise ValueError("Cannot create AIFPLFunction from Python value")

    def type_name(self) -> str:
        return "function"

    def with_name(self, name: str) -> 'AIFPLFunction':
        """Return a new function with the given name."""
        return replace(self, name=name)

    def __call__(self, *args: Any, **kwargs: Any) -> None:
        """
        Make AIFPLFunction callable for Python's callable() function.

        This is just to satisfy the callable() check in tests.
        Actual function calling is handled by the evaluator.
        """
        raise RuntimeError("AIFPLFunction objects should be called through the evaluator, not directly")


class AIFPLBuiltinFunction(AIFPLValue):
    """Represents a built-in function that can be used in higher-order contexts."""

    def __init__(self, name: str):
        self.name = name

    def to_python(self) -> str:
        return self.name

    @classmethod
    def from_python(cls, value: str) -> 'AIFPLBuiltinFunction':
        return cls(value)

    def type_name(self) -> str:
        return "builtin-function"

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, AIFPLBuiltinFunction) and self.name == other.name

    def __hash__(self) -> int:
        return hash((type(self), self.name))


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

    @classmethod
    def from_python(cls, value: Any) -> 'AIFPLRecursivePlaceholder':
        raise ValueError("Cannot create AIFPLRecursivePlaceholder from Python value")

    def type_name(self) -> str:
        return f"recursive-placeholder({self._name})"

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, AIFPLRecursivePlaceholder):
            return self._name == other._name

        return self.get_resolved_value() == other

    def __hash__(self) -> int:
        return hash((type(self), self._name))


def python_to_aifpl_value(value: Any) -> AIFPLValue:
    """Convert a Python value to the appropriate AIFPLValue type."""
    if isinstance(value, bool):
        return AIFPLBoolean(value)

    if isinstance(value, (int, float, complex)) and not isinstance(value, bool):
        return AIFPLNumber(value)

    if isinstance(value, str):
        return AIFPLString(value)

    if isinstance(value, list):
        return AIFPLList.from_python(value)

    if isinstance(value, AIFPLValue):
        return value  # Already an AIFPL value

    raise ValueError(f"Cannot convert Python value of type {type(value).__name__} to AIFPLValue")


def aifpl_value_to_python(value: AIFPLValue) -> Any:
    """Convert an AIFPLValue to its Python equivalent."""
    return value.to_python()
