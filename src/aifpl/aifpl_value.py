"""AIFPL Value hierarchy - immutable value types for the language."""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple, Union


class AIFPLValue(ABC):
    """
    Abstract base class for all AIFPL values.
    
    All AIFPL values are immutable and can carry metadata.
    """

    def __init__(self, metadata: Optional[Dict[str, Any]] = None):
        """Initialize with optional metadata."""
        object.__setattr__(self, '_metadata', metadata or {})

    @property
    def metadata(self) -> Dict[str, Any]:
        """Get metadata dictionary."""
        return self._metadata

    @abstractmethod
    def to_python(self) -> Any:
        """Convert to Python value for operations."""
        pass

    @classmethod
    @abstractmethod
    def from_python(cls, value: Any, **metadata) -> 'AIFPLValue':
        """Create AIFPL value from Python value."""
        pass

    @abstractmethod
    def type_name(self) -> str:
        """Return AIFPL type name for error messages."""
        pass

    @abstractmethod
    def with_metadata(self, **kwargs) -> 'AIFPLValue':
        """Return new value with additional metadata."""
        pass

    def __eq__(self, other) -> bool:
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
        """Hash based on value, not metadata."""
        return hash((type(self), self.to_python()))

    def __setattr__(self, name: str, value: Any) -> None:
        """Prevent modification after initialization."""
        if hasattr(self, '_initialized'):
            raise AttributeError(f"Cannot modify immutable {type(self).__name__}")
 
        object.__setattr__(self, name, value)

    def _mark_initialized(self) -> None:
        """Mark the object as initialized and immutable."""
        object.__setattr__(self, '_initialized', True)


@dataclass(frozen=True)
class AIFPLNumber(AIFPLValue):
    """Represents numeric values: integers, floats, complex numbers."""
    value: Union[int, float, complex]

    def __post_init__(self):
        """Validate that booleans are not treated as numbers."""
        if isinstance(self.value, bool):
            raise ValueError("Booleans are not numbers in AIFPL")

        super().__init__()
        self._mark_initialized()

    def to_python(self) -> Union[int, float, complex]:
        return self.value

    @classmethod
    def from_python(cls, value: Union[int, float, complex], **metadata) -> 'AIFPLNumber':
        if isinstance(value, bool):
            raise ValueError("Cannot create AIFPLNumber from boolean")

        result = cls(value)
        if metadata:
            return result.with_metadata(**metadata)

        return result

    def type_name(self) -> str:
        if isinstance(self.value, int):
            return "integer"

        if isinstance(self.value, float):
            return "float"

        return "complex"

    def with_metadata(self, **kwargs) -> 'AIFPLNumber':
        """Return new value with additional metadata."""
        new_metadata = {**self.metadata, **kwargs}
        result = AIFPLNumber(self.value)
        object.__setattr__(result, '_metadata', new_metadata)
        result._mark_initialized()
        return result

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

    def __post_init__(self):
        super().__init__()
        self._mark_initialized()

    def to_python(self) -> str:
        return self.value

    @classmethod
    def from_python(cls, value: str, **metadata) -> 'AIFPLString':
        if not isinstance(value, str):
            raise ValueError(f"Cannot create AIFPLString from {type(value).__name__}")

        result = cls(value)
        if metadata:
            return result.with_metadata(**metadata)

        return result

    def type_name(self) -> str:
        return "string"

    def with_metadata(self, **kwargs) -> 'AIFPLString':
        """Return new value with additional metadata."""
        new_metadata = {**self.metadata, **kwargs}
        result = AIFPLString(self.value)
        object.__setattr__(result, '_metadata', new_metadata)
        result._mark_initialized()
        return result


@dataclass(frozen=True)
class AIFPLBoolean(AIFPLValue):
    """Represents boolean values."""
    value: bool

    def __post_init__(self):
        super().__init__()
        self._mark_initialized()

    def to_python(self) -> bool:
        return self.value

    @classmethod
    def from_python(cls, value: bool, **metadata) -> 'AIFPLBoolean':
        if not isinstance(value, bool):
            raise ValueError(f"Cannot create AIFPLBoolean from {type(value).__name__}")

        result = cls(value)
        if metadata:
            return result.with_metadata(**metadata)

        return result

    def type_name(self) -> str:
        return "boolean"

    def with_metadata(self, **kwargs) -> 'AIFPLBoolean':
        """Return new value with additional metadata."""
        new_metadata = {**self.metadata, **kwargs}
        result = AIFPLBoolean(self.value)
        object.__setattr__(result, '_metadata', new_metadata)
        result._mark_initialized()
        return result


@dataclass(frozen=True)
class AIFPLSymbol(AIFPLValue):
    """Represents symbols that require environment lookup."""
    name: str
    position: int = 0

    def __post_init__(self):
        super().__init__()
        self._mark_initialized()

    def to_python(self) -> str:
        """Symbols convert to their name string."""
        return self.name

    @classmethod
    def from_python(cls, value: str, position: int = 0, **metadata) -> 'AIFPLSymbol':
        if not isinstance(value, str):

            raise ValueError(f"Cannot create AIFPLSymbol from {type(value).__name__}")
        result = cls(value, position)
        if metadata:
            return result.with_metadata(**metadata)

        return result

    def type_name(self) -> str:
        return "symbol"

    def with_metadata(self, **kwargs) -> 'AIFPLSymbol':
        """Return new value with additional metadata."""
        new_metadata = {**self.metadata, **kwargs}
        result = AIFPLSymbol(self.name, self.position)
        object.__setattr__(result, '_metadata', new_metadata)
        result._mark_initialized()
        return result

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'AIFPLSymbol({self.name!r})'


@dataclass(frozen=True)
class AIFPLList(AIFPLValue):
    """Represents lists of AIFPL values."""
    elements: Tuple[AIFPLValue, ...] = field(default_factory=tuple)

    def __post_init__(self):
        super().__init__()
        self._mark_initialized()

    def to_python(self) -> List[Any]:
        """Convert to Python list with Python values."""
        return [elem.to_python() for elem in self.elements]

    @classmethod
    def from_python(cls, value: List[Any], **metadata) -> 'AIFPLList':
        """Create AIFPLList from Python list, converting elements."""
        if not isinstance(value, list):
            raise ValueError(f"Cannot create AIFPLList from {type(value).__name__}")

        elements = []
        for item in value:
            elements.append(python_to_aifpl_value(item))

        result = cls(tuple(elements))
        if metadata:
            return result.with_metadata(**metadata)
        return result

    def type_name(self) -> str:
        return "list"

    def with_metadata(self, **kwargs) -> 'AIFPLList':
        """Return new value with additional metadata."""
        new_metadata = {**self.metadata, **kwargs}
        result = AIFPLList(self.elements)
        object.__setattr__(result, '_metadata', new_metadata)
        result._mark_initialized()
        return result

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

        return AIFPLList(self.elements[1:]).with_metadata(**self.metadata)

    def last(self) -> AIFPLValue:
        """Get the last element (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get last element of empty list")

        return self.elements[-1]

    def cons(self, element: AIFPLValue) -> 'AIFPLList':
        """Prepend an element to the front of the list."""
        return AIFPLList((element,) + self.elements).with_metadata(**self.metadata)

    def append_list(self, other: 'AIFPLList') -> 'AIFPLList':
        """Append another list to this one."""
        return AIFPLList(self.elements + other.elements).with_metadata(**self.metadata)

    def reverse(self) -> 'AIFPLList':
        """Return a reversed copy of the list."""
        return AIFPLList(tuple(reversed(self.elements))).with_metadata(**self.metadata)

    def get(self, index: int) -> AIFPLValue:
        """Get element at index (raises IndexError if out of bounds)."""
        return self.elements[index]

    def contains(self, value: AIFPLValue) -> bool:
        """Check if the list contains a value (using AIFPL equality)."""
        return value in self.elements

    def remove_all(self, value: AIFPLValue) -> 'AIFPLList':
        """Remove all occurrences of a value."""
        new_elements = tuple(elem for elem in self.elements if elem != value)
        return AIFPLList(new_elements).with_metadata(**self.metadata)

    def position(self, value: AIFPLValue) -> Optional[int]:
        """Find the first position of a value, or None if not found."""
        for i, elem in enumerate(self.elements):
            if elem == value:
                return i

        return None

    def take(self, n: int) -> 'AIFPLList':
        """Take the first n elements."""
        return AIFPLList(self.elements[:n]).with_metadata(**self.metadata)

    def drop(self, n: int) -> 'AIFPLList':
        """Drop the first n elements."""
        return AIFPLList(self.elements[n:]).with_metadata(**self.metadata)


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
