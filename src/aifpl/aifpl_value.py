"""AIFPL Value hierarchy - immutable value types for the language."""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, List, Tuple, Callable

from aifpl.aifpl_error import AIFPLEvalError


class AIFPLValue(ABC):
    """
    Abstract base class for all AIFPL values.

    All AIFPL values are immutable.
    """

    @abstractmethod
    def is_self_evaluating(self) -> bool:
        """Return True if this value evaluates to itself (doesn't need evaluation)."""

    @abstractmethod
    def to_python(self) -> Any:
        """Convert to Python value for operations."""

    @abstractmethod
    def type_name(self) -> str:
        """Return AIFPL type name for error messages."""


@dataclass(frozen=True)
class AIFPLInteger(AIFPLValue):
    """Represents integer values."""
    value: int

    def is_self_evaluating(self) -> bool:
        return True

    def to_python(self) -> int:
        return self.value

    def type_name(self) -> str:
        return "integer"

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values, allowing cross-type comparison."""
        if isinstance(other, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            return self.value == other.value

        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLFloat(AIFPLValue):
    """Represents floating-point values."""
    value: float

    def is_self_evaluating(self) -> bool:
        return True

    def to_python(self) -> float:
        return self.value

    def type_name(self) -> str:
        return "float"

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values, allowing cross-type comparison."""
        if isinstance(other, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            return self.value == other.value

        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLComplex(AIFPLValue):
    """Represents complex number values."""
    value: complex

    def is_self_evaluating(self) -> bool:
        return True

    def to_python(self) -> complex:
        return self.value

    def type_name(self) -> str:
        return "complex"

    def __eq__(self, other: Any) -> bool:
        """Compare numeric values, allowing cross-type comparison."""
        if isinstance(other, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            return self.value == other.value

        return False

    def __hash__(self) -> int:
        """Hash based on value for use in sets/dicts."""
        return hash(self.value)


@dataclass(frozen=True)
class AIFPLString(AIFPLValue):
    """Represents string values."""
    value: str

    def is_self_evaluating(self) -> bool:
        return True

    def to_python(self) -> str:
        return self.value

    def type_name(self) -> str:
        return "string"


@dataclass(frozen=True)
class AIFPLBoolean(AIFPLValue):
    """Represents boolean values."""
    value: bool

    def is_self_evaluating(self) -> bool:
        return True

    def to_python(self) -> bool:
        return self.value

    def type_name(self) -> str:
        return "boolean"


@dataclass(frozen=True)
class AIFPLSymbol(AIFPLValue):
    """Represents symbols that require environment lookup."""
    name: str
    position: int = 0

    def is_self_evaluating(self) -> bool:
        return False

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

    def is_self_evaluating(self) -> bool:
        return False

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

    def is_self_evaluating(self) -> bool:
        return True

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

    This is a first-class value that can be stored in environments
    and passed around as a value.

    A function can be either:
    - A user-defined lambda with bytecode or AST body
    - A builtin with a native Python implementation
    """
    parameters: Tuple[str, ...]
    body: AIFPLValue | None = None  # AST body for evaluator
    closure_environment: Any = None  # AIFPLEnvironment, avoiding circular import
    name: str | None = None
    bytecode: Any = None  # CodeObject for bytecode-compiled functions
    captured_values: Tuple[Any, ...] = ()  # Captured free variables for closures
    native_impl: Callable | None = None  # Python function for builtins
    is_variadic: bool = False  # True if function accepts variable number of args

    def __post_init__(self) -> None:
        """Validate that function has either body/bytecode or native_impl, but not both."""
        has_body = self.body is not None or self.bytecode is not None
        has_native = self.native_impl is not None

        if has_body and has_native:
            raise ValueError("Function cannot have both body/bytecode and native_impl")

        if not has_body and not has_native:
            raise ValueError("Function must have either body/bytecode or native_impl")

    def is_self_evaluating(self) -> bool:
        return True

    def to_python(self) -> 'AIFPLFunction | str':
        """Functions return themselves (or their name for builtins as string)."""
        return self

    def type_name(self) -> str:
        return "function"

    @property
    def is_native(self) -> bool:
        """Check if this is a native (builtin) function."""
        return self.native_impl is not None

    def describe(self) -> str:
        """Return a human-readable description of this function."""
        param_str = ', '.join(self.parameters)
        if self.is_variadic and len(self.parameters) > 0:
            # Last parameter is variadic (rest parameter)
            regular_params = ', '.join(self.parameters[:-1]) if len(self.parameters) > 1 else ''
            rest_param = self.parameters[-1]
            param_str = f"{regular_params} . {rest_param}".strip(' .')

        if self.is_native:
            return f"<builtin {self.name}({param_str})>"

        return f"<lambda ({param_str})>"


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

    def is_self_evaluating(self) -> bool:
        return False

    def to_python(self) -> Any:
        return self.get_resolved_value().to_python()

    def type_name(self) -> str:
        return f"recursive-placeholder({self._name})"


@dataclass(frozen=True)
class AIFPLTailCall(AIFPLValue):
    """
    Represents a tail call to be optimized.

    This is a special internal value type that represents a deferred function call
    for tail call optimization. It should never be visible to user code and is
    only used internally by the evaluator.
    """
    function: AIFPLValue
    arguments: List[AIFPLValue]
    environment: Any  # AIFPLEnvironment, avoiding circular import

    def to_python(self) -> Any:
        """Tail calls should never be converted to Python values."""
        raise AIFPLEvalError("Internal error: AIFPLTailCall should never be converted to Python value")

    def type_name(self) -> str:
        return "tail-call"

    def is_self_evaluating(self) -> bool:
        return False
