"""Menai Value hierarchy - immutable runtime value types.

These are lightweight runtime values used by the VM and bytecode.
They do NOT carry source location metadata - that's only in MenaiASTNode.
This separation keeps runtime values fast and memory-efficient.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, List, Tuple

from menai.menai_error import MenaiEvalError


@dataclass(slots=True, unsafe_hash=True)
class MenaiValue(ABC):
    """
    Abstract base class for all Menai runtime values.

    All runtime values are immutable and lightweight (no metadata).
    """

    @abstractmethod
    def to_python(self) -> Any:
        """Convert to Python value for operations."""

    @abstractmethod
    def type_name(self) -> str:
        """Return Menai type name for error messages."""

    @abstractmethod
    def describe(self) -> str:
        """Describe the value."""


@dataclass(slots=True, unsafe_hash=True)
class MenaiSymbol(MenaiValue):
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
        return f'MenaiSymbol({self.name!r})'


@dataclass(slots=True, unsafe_hash=True)
class MenaiFunction(MenaiValue):
    """
    Represents a function (both user-defined lambdas and builtins).

    This is a first-class value that can be passed around as a value.

    captured_values is a list so that PATCH_CLOSURE can fill in letrec sibling
    slots after all closures in a mutual-recursion group have been created.
    The dataclass remains frozen; mutation is done via object.__setattr__ in
    the VM (the same pattern used by MenaiDict for its _lookup field).
    """
    parameters: Tuple[str, ...]
    name: str | None = None
    bytecode: Any = None  # CodeObject for bytecode-compiled functions
    captured_values: List[Any] = field(default_factory=list)  # Captured free variables for closures
    is_variadic: bool = False  # True if function accepts variable number of args

    def to_python(self) -> 'MenaiFunction | str':
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


@dataclass(slots=True, unsafe_hash=True)
class MenaiNone(MenaiValue):
    """Represents the absence of a value (#none).

    This is a distinct type from boolean false (#f).  It is returned by
    operations that produce no meaningful result (missing dict key, item not
    found, unparseable string, etc.) so that callers can distinguish between
    a stored #f value and a genuinely absent one.
    """

    def to_python(self) -> None:
        return None

    def type_name(self) -> str:
        return "none"

    def describe(self) -> str:
        return "#none"


@dataclass(slots=True, unsafe_hash=True)
class MenaiBoolean(MenaiValue):
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
        if not isinstance(other, MenaiBoolean):
            return False

        return self.value == other.value


@dataclass(slots=True, unsafe_hash=True)
class MenaiInteger(MenaiValue):
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
        if not isinstance(other, MenaiInteger):
            return False

        return self.value == other.value


@dataclass(slots=True, unsafe_hash=True)
class MenaiFloat(MenaiValue):
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
        if not isinstance(other, MenaiFloat):
            return False

        return self.value == other.value


@dataclass(slots=True, unsafe_hash=True)
class MenaiComplex(MenaiValue):
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
        if not isinstance(other, MenaiComplex):
            return False

        return self.value == other.value


@dataclass(slots=True, unsafe_hash=True)
class MenaiString(MenaiValue):
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
        if not isinstance(other, MenaiString):
            return False

        return self.value == other.value


@dataclass(slots=True, unsafe_hash=True)
class MenaiList(MenaiValue):
    """Represents lists of Menai values."""
    elements: Tuple[MenaiValue, ...] = ()

    def to_python(self) -> List[Any]:
        """Convert to Python list with Python values."""
        return [elem.to_python() for elem in self.elements]

    def type_name(self) -> str:
        return "list"

    def describe(self) -> str:
        # Format list: (element1 element2 ...)
        if len(self.elements) == 0:
            return "()"

        formatted_elements = []
        for element in self.elements:
            formatted_elements.append(element.describe())

        return f"({' '.join(formatted_elements)})"


class MenaiDict(MenaiValue):
    """
    Represents dictionaries - immutable key-value mappings.

    Internally uses a dict for O(1) lookups while maintaining insertion order.
    Keys must be hashable (strings, numbers, booleans, symbols).
    """
    __slots__ = ('pairs', 'lookup')

    def __init__(self, pairs: Tuple[Tuple[MenaiValue, MenaiValue], ...] = ()) -> None:
        self.pairs = pairs
        lookup = {}
        for key, value in pairs:
            hashable_key = self.to_hashable_key(key)
            lookup[hashable_key] = (key, value)

        self.lookup = lookup

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, MenaiDict):
            return False

        return self.pairs == other.pairs

    def to_python(self) -> dict:
        """Convert to Python dict."""
        result = {}
        for key, value in self.pairs:
            # Use string representation for Python dict keys
            if isinstance(key, MenaiString):
                py_key = key.value

            elif isinstance(key, MenaiSymbol):
                py_key = key.name

            else:
                py_key = str(key.to_python())

            result[py_key] = value.to_python()

        return result

    def type_name(self) -> str:
        """Return type name for error messages."""
        return "dict"

    def describe(self) -> str:
        # Format dict with curly braces: {(key1 val1) (key2 val2) ...}
        if len(self.pairs) == 0:
            return "{}"

        formatted_pairs = []
        for key, value in self.pairs:
            formatted_key = key.describe()
            formatted_value = value.describe()
            formatted_pairs.append(f"({formatted_key} {formatted_value})")

        pairs_str = ' '.join(formatted_pairs)
        return f"{{{pairs_str}}}"

    @staticmethod
    def to_hashable_key(key: MenaiValue) -> Tuple[str, Any]:
        """Convert Menai key to hashable Python value."""
        if type(key) is MenaiString:  # pylint: disable=unidiomatic-typecheck
            return ('str', key.value)

        if type(key) is MenaiInteger:  # pylint: disable=unidiomatic-typecheck
            return ('int', key.value)

        if type(key) is MenaiFloat:  # pylint: disable=unidiomatic-typecheck
            return ('flt', key.value)

        if type(key) is MenaiComplex:  # pylint: disable=unidiomatic-typecheck
            return ('cplx', key.value)

        if type(key) is MenaiBoolean:  # pylint: disable=unidiomatic-typecheck
            return ('bool', key.value)

        if type(key) is MenaiSymbol:  # pylint: disable=unidiomatic-typecheck
            return ('sym', key.name)

        raise MenaiEvalError(
            message="Dict keys must be strings, numbers, booleans, or symbols",
            received=f"Key type: {key.type_name()}",
            example='(dict ("name" "Alice") ("age" 30))',
            suggestion="Use strings for most keys"
        )


class MenaiSet(MenaiValue):
    """
    Represents sets - immutable unordered collections of unique hashable values.

    Internally uses a frozenset of hashable keys for O(1) membership testing
    while maintaining insertion order in a tuple for deterministic iteration
    and display.  Duplicate elements are silently dropped on construction.
    Valid element types are the same as dict keys: strings, numbers, booleans,
    and symbols.
    """
    __slots__ = ('elements', 'members')

    def __init__(self, elements: Tuple['MenaiValue', ...] = ()) -> None:
        seen: set = set()
        deduped = []
        for elem in elements:
            hk = MenaiDict.to_hashable_key(elem)
            if hk not in seen:
                seen.add(hk)
                deduped.append(elem)

        self.elements: Tuple['MenaiValue', ...] = tuple(deduped)
        self.members: frozenset = frozenset(
            MenaiDict.to_hashable_key(e) for e in self.elements
        )

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, MenaiSet):
            return False

        return self.members == other.members

    def __hash__(self) -> int:
        return hash(self.members)

    def to_python(self) -> set:
        """Convert to Python set."""
        result = set()
        for elem in self.elements:
            if isinstance(elem, MenaiString):
                result.add(elem.value)

            elif isinstance(elem, MenaiSymbol):
                result.add(elem.name)

            else:
                result.add(elem.to_python())

        return result

    def type_name(self) -> str:
        return "set"

    def describe(self) -> str:
        if not self.elements:
            return "#{}"

        return "#{" + " ".join(e.describe() for e in self.elements) + "}"


# Module-level singletons — there is only one #none value.
Menai_NONE = MenaiNone()
Menai_BOOLEAN_TRUE = MenaiBoolean(True)
Menai_BOOLEAN_FALSE = MenaiBoolean(False)
Menai_LIST_EMPTY = MenaiList(())
Menai_DICT_EMPTY = MenaiDict(())
Menai_SET_EMPTY = MenaiSet(())
