"""AIFPL AST Node hierarchy - compile-time representation with source location metadata.

This module defines the Abstract Syntax Tree node types used during compilation.
These are separate from runtime AIFPLValue types to avoid carrying metadata overhead
into the bytecode and VM execution.

Key differences from AIFPLValue:
- AST nodes have source location metadata (line, column, source_file)
- AST nodes are used only during compilation
- Runtime values (AIFPLValue) are lightweight with no metadata

The conversion from AST -> runtime Value happens in the code generator when
building the constants pool for bytecode.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, List, Tuple

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex,
    AIFPLString, AIFPLBoolean, AIFPLList, AIFPLAList
)


@dataclass(frozen=True)
class AIFPLASTNode(ABC):
    """
    Abstract base class for all AIFPL AST nodes.

    All AST nodes are immutable and carry source location metadata
    for error reporting and debugging.
    
    Source location fields are keyword-only to preserve positional argument
    compatibility with existing code.
    """
    # Source location metadata (keyword-only)
    line: int | None = field(default=None, kw_only=True)
    column: int | None = field(default=None, kw_only=True)
    source_file: str = field(default="", kw_only=True)
    
    @abstractmethod
    def to_runtime_value(self) -> AIFPLValue:
        """Convert AST node to runtime value (strips metadata)."""
    
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
class AIFPLASTInteger(AIFPLASTNode):
    """Represents integer values in AST."""
    value: int

    def to_runtime_value(self) -> AIFPLInteger:
        """Convert to runtime integer (no metadata)."""
        return AIFPLInteger(self.value)

    def to_python(self) -> int:
        return self.value

    def type_name(self) -> str:
        return "integer"

    def describe(self) -> str:
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values, allowing cross-type comparison."""
        if isinstance(other, (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex)):
            return self.value == other.value
        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLASTFloat(AIFPLASTNode):
    """Represents floating-point values in AST."""
    value: float

    def to_runtime_value(self) -> AIFPLFloat:
        """Convert to runtime float (no metadata)."""
        return AIFPLFloat(self.value)

    def to_python(self) -> float:
        return self.value

    def type_name(self) -> str:
        return "float"

    def describe(self) -> str:
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values, allowing cross-type comparison."""
        if isinstance(other, (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex)):
            return self.value == other.value
        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLASTComplex(AIFPLASTNode):
    """Represents complex number values in AST."""
    value: complex

    def to_runtime_value(self) -> AIFPLComplex:
        """Convert to runtime complex (no metadata)."""
        return AIFPLComplex(self.value)

    def to_python(self) -> complex:
        return self.value

    def type_name(self) -> str:
        return "complex"

    def describe(self) -> str:
        return str(self.value).strip('()')

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values, allowing cross-type comparison."""
        if isinstance(other, (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex)):
            return self.value == other.value
        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLASTString(AIFPLASTNode):
    """Represents string values in AST."""
    value: str

    def to_runtime_value(self) -> AIFPLString:
        """Convert to runtime string (no metadata)."""
        return AIFPLString(self.value)

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
        if isinstance(other, AIFPLASTString):
            return self.value == other.value
        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLASTBoolean(AIFPLASTNode):
    """Represents boolean values in AST."""
    value: bool

    def to_runtime_value(self) -> AIFPLBoolean:
        """Convert to runtime boolean (no metadata)."""
        return AIFPLBoolean(self.value)

    def to_python(self) -> bool:
        return self.value

    def type_name(self) -> str:
        return "boolean"

    def describe(self) -> str:
        return "#t" if self.value else "#f"

    def __eq__(self, other: Any) -> bool:
        """Compare boolean values, ignoring metadata (line, column)."""
        if isinstance(other, AIFPLASTBoolean):
            return self.value == other.value
        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLASTSymbol(AIFPLASTNode):
    """Represents symbols that require environment lookup in AST."""
    name: str

    def to_runtime_value(self) -> AIFPLValue:
        """Symbols don't have a direct runtime representation - they're resolved at compile time."""
        raise NotImplementedError("Symbols should be resolved during compilation, not converted to runtime values")

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
        return f'AIFPLASTSymbol({self.name!r})'


@dataclass(frozen=True)
class AIFPLASTList(AIFPLASTNode):
    """Represents lists of AIFPL AST nodes."""
    elements: Tuple[AIFPLASTNode, ...] = ()

    def to_runtime_value(self) -> AIFPLList:
        """Convert to runtime list (recursively converts elements)."""
        runtime_elements = tuple(elem.to_runtime_value() for elem in self.elements)
        return AIFPLList(runtime_elements)

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

    def first(self) -> AIFPLASTNode:
        """Get the first element (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get first element of empty list")
        return self.elements[0]

    def rest(self) -> 'AIFPLASTList':
        """Get all elements except the first (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get rest of empty list")
        return AIFPLASTList(self.elements[1:])

    def last(self) -> AIFPLASTNode:
        """Get the last element (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get last element of empty list")
        return self.elements[-1]

    def cons(self, element: AIFPLASTNode) -> 'AIFPLASTList':
        """Prepend an element to the front of the list."""
        return AIFPLASTList((element,) + self.elements)

    def append_list(self, other: 'AIFPLASTList') -> 'AIFPLASTList':
        """Append another list to this one."""
        return AIFPLASTList(self.elements + other.elements)

    def reverse(self) -> 'AIFPLASTList':
        """Return a reversed copy of the list."""
        return AIFPLASTList(tuple(reversed(self.elements)))

    def get(self, index: int) -> AIFPLASTNode:
        """Get element at index (raises IndexError if out of bounds)."""
        return self.elements[index]

    def contains(self, value: AIFPLASTNode) -> bool:
        """Check if the list contains a value (using AIFPL equality)."""
        return value in self.elements

    def remove_all(self, value: AIFPLASTNode) -> 'AIFPLASTList':
        """Remove all occurrences of a value."""
        new_elements = tuple(elem for elem in self.elements if elem != value)
        return AIFPLASTList(new_elements)

    def position(self, value: AIFPLASTNode) -> int | None:
        """Find the first position of a value, or None if not found."""
        for i, elem in enumerate(self.elements):
            if elem == value:
                return i
        return None

    def take(self, n: int) -> 'AIFPLASTList':
        """Take the first n elements."""
        return AIFPLASTList(self.elements[:n])

    def drop(self, n: int) -> 'AIFPLASTList':
        """Drop the first n elements."""
        return AIFPLASTList(self.elements[n:])


@dataclass(frozen=True)
class AIFPLASTAList(AIFPLASTNode):
    """
    Represents association lists (alists) in AST - immutable key-value mappings.

    Internally uses a dict for O(1) lookups while maintaining insertion order.
    Keys must be hashable (strings, numbers, booleans, symbols).
    """
    pairs: Tuple[Tuple[AIFPLASTNode, AIFPLASTNode], ...] = ()
    _lookup: dict = field(default_factory=dict, init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        """Build internal lookup dict after initialization."""
        # Use object.__setattr__ because dataclass is frozen
        lookup = {}
        for key, value in self.pairs:
            hashable_key = self._to_hashable_key(key)
            lookup[hashable_key] = (key, value)
        object.__setattr__(self, '_lookup', lookup)

    def to_runtime_value(self) -> AIFPLAList:
        """Convert to runtime alist (recursively converts elements)."""
        runtime_pairs = tuple(
            (key.to_runtime_value(), value.to_runtime_value())
            for key, value in self.pairs
        )
        return AIFPLAList(runtime_pairs)

    def to_python(self) -> dict:
        """Convert to Python dict."""
        result = {}
        for key, value in self.pairs:
            # Use string representation for Python dict keys
            if isinstance(key, AIFPLASTString):
                py_key = key.value
            elif isinstance(key, AIFPLASTSymbol):
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

    def get(self, key: AIFPLASTNode) -> AIFPLASTNode | None:
        """Get value by key, returns None if not found."""
        hashable_key = self._to_hashable_key(key)
        if hashable_key in self._lookup:
            _, value = self._lookup[hashable_key]
            return value
        return None

    def has_key(self, key: AIFPLASTNode) -> bool:
        """Check if key exists."""
        hashable_key = self._to_hashable_key(key)
        return hashable_key in self._lookup

    def set(self, key: AIFPLASTNode, value: AIFPLASTNode) -> 'AIFPLASTAList':
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
        return AIFPLASTAList(tuple(new_pairs))

    def remove(self, key: AIFPLASTNode) -> 'AIFPLASTAList':
        """Return new alist without key."""
        hashable_key = self._to_hashable_key(key)
        new_pairs = tuple(
            (k, v) for k, v in self.pairs
            if self._to_hashable_key(k) != hashable_key
        )
        return AIFPLASTAList(new_pairs)

    def keys(self) -> Tuple[AIFPLASTNode, ...]:
        """Get all keys in insertion order."""
        return tuple(k for k, _ in self.pairs)

    def values(self) -> Tuple[AIFPLASTNode, ...]:
        """Get all values in insertion order."""
        return tuple(v for _, v in self.pairs)

    def merge(self, other: 'AIFPLASTAList') -> 'AIFPLASTAList':
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
        return AIFPLASTAList(tuple(new_pairs))

    def length(self) -> int:
        """Number of key-value pairs."""
        return len(self.pairs)

    def is_empty(self) -> bool:
        """Check if alist is empty."""
        return len(self.pairs) == 0

    @staticmethod
    def _to_hashable_key(key: AIFPLASTNode) -> Tuple[str, Any]:
        """Convert AIFPL key to hashable Python value."""
        if isinstance(key, AIFPLASTString):
            return ('str', key.value)
        if isinstance(key, (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex)):
            return ('num', key.value)
        if isinstance(key, AIFPLASTBoolean):
            return ('bool', key.value)
        if isinstance(key, AIFPLASTSymbol):
            return ('sym', key.name)
        raise AIFPLEvalError(
            message="AList keys must be strings, numbers, booleans, or symbols",
            received=f"Key type: {key.type_name()}",
            example='(alist ("name" "Alice") ("age" 30))',
            suggestion="Use strings for most keys"
        )
