"""Menai AST Node hierarchy - compile-time representation with source location metadata.

This module defines the Abstract Syntax Tree node types used during compilation.
These are separate from runtime MenaiValue types to avoid carrying metadata overhead
into the bytecode and VM execution.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Tuple

from menai.menai_value import (
    MenaiValue, MenaiInteger, MenaiFloat, MenaiComplex,
    MenaiString, MenaiBoolean, MenaiSymbol, MenaiList, MenaiDict, MenaiNone, Menai_NONE
)


@dataclass(frozen=True)
class MenaiASTNode(ABC):
    """
    Abstract base class for all Menai AST nodes.

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
    def to_runtime_value(self) -> MenaiValue:
        """Convert AST node to runtime value (strips metadata)."""

    @abstractmethod
    def type_name(self) -> str:
        """Return Menai type name for error messages."""

    @abstractmethod
    def describe(self) -> str:
        """Describe the value."""


@dataclass(frozen=True)
class MenaiASTNone(MenaiASTNode):
    """Represents the #none literal in the AST."""

    def to_runtime_value(self) -> MenaiNone:
        """Convert to the runtime #none singleton."""
        return Menai_NONE

    def type_name(self) -> str:
        return "none"

    def describe(self) -> str:
        return "#none"

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, MenaiASTNone)


@dataclass(frozen=True)
class MenaiASTBoolean(MenaiASTNode):
    """Represents boolean values in AST."""
    value: bool

    def to_runtime_value(self) -> MenaiBoolean:
        """Convert to runtime boolean (no metadata)."""
        return MenaiBoolean(self.value)

    def type_name(self) -> str:
        return "boolean"

    def describe(self) -> str:
        return "#t" if self.value else "#f"

    def __eq__(self, other: Any) -> bool:
        return self.value == other.value


@dataclass(frozen=True)
class MenaiASTInteger(MenaiASTNode):
    """Represents integer values in AST."""
    value: int

    def to_runtime_value(self) -> MenaiInteger:
        """Convert to runtime integer (no metadata)."""
        return MenaiInteger(self.value)

    def type_name(self) -> str:
        return "integer"

    def describe(self) -> str:
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        return self.value == other.value


@dataclass(frozen=True)
class MenaiASTFloat(MenaiASTNode):
    """Represents floating-point values in AST."""
    value: float

    def to_runtime_value(self) -> MenaiFloat:
        """Convert to runtime float (no metadata)."""
        return MenaiFloat(self.value)

    def type_name(self) -> str:
        return "float"

    def describe(self) -> str:
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        return self.value == other.value


@dataclass(frozen=True)
class MenaiASTComplex(MenaiASTNode):
    """Represents complex number values in AST."""
    value: complex

    def to_runtime_value(self) -> MenaiComplex:
        """Convert to runtime complex (no metadata)."""
        return MenaiComplex(self.value)

    def type_name(self) -> str:
        return "complex"

    def describe(self) -> str:
        return str(self.value).strip('()')

    def __eq__(self, other: Any) -> bool:
        return self.value == other.value


@dataclass(frozen=True)
class MenaiASTString(MenaiASTNode):
    """Represents string values in AST."""
    value: str

    def to_runtime_value(self) -> MenaiString:
        """Convert to runtime string (no metadata)."""
        return MenaiString(self.value)

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
        return self.value == other.value


@dataclass(frozen=True)
class MenaiASTSymbol(MenaiASTNode):
    """Represents symbols that require environment lookup in AST."""
    name: str

    def to_runtime_value(self) -> MenaiValue:
        """
        Convert to runtime symbol (for quoted data).

        Note: This should only be called in quoted contexts where symbols are data.
        In normal code, symbols are resolved to variables at compile time.
        """
        return MenaiSymbol(self.name)

    def type_name(self) -> str:
        return "symbol"

    def describe(self) -> str:
        return self.name

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'MenaiASTSymbol({self.name!r})'


@dataclass(frozen=True)
class MenaiASTList(MenaiASTNode):
    """Represents lists of Menai AST nodes."""
    elements: Tuple[MenaiASTNode, ...] = ()

    def to_runtime_value(self) -> MenaiList:
        """Convert to runtime list (recursively converts elements)."""
        runtime_elements = tuple(elem.to_runtime_value() for elem in self.elements)
        return MenaiList(runtime_elements)

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

    def first(self) -> MenaiASTNode:
        """Get the first element (raises IndexError if empty)."""
        if not self.elements:
            raise IndexError("Cannot get first element of empty list")

        return self.elements[0]

    def get(self, index: int) -> MenaiASTNode:
        """Get element at index (raises IndexError if out of bounds)."""
        return self.elements[index]


@dataclass(frozen=True)
class MenaiASTDict(MenaiASTNode):
    """Represents a dict literal in the AST.

    Carries key-value pairs as parallel tuples of already-converted
    MenaiValue objects (not AST nodes), mirroring MenaiASTList's approach
    of storing runtime-ready elements.

    Currently only the empty case (pairs=()) is synthesised by the desugarer,
    as the seed accumulator for a (dict (list k v) ...) literal fold and for
    zero-argument (dict) calls.  Non-empty dict literals are handled by the
    desugarer as a fold of $dict-set calls over an empty seed.
    """
    pairs: Tuple[Tuple['MenaiASTNode', 'MenaiASTNode'], ...] = ()

    def to_runtime_value(self) -> MenaiDict:
        return MenaiDict(
            tuple((k.to_runtime_value(), v.to_runtime_value()) for k, v in self.pairs)
        )

    def type_name(self) -> str:
        return "dict"

    def describe(self) -> str:
        if not self.pairs:
            return "{}"
        parts = " ".join(f"({k.describe()} {v.describe()})" for k, v in self.pairs)
        return "{" + parts + "}"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, MenaiASTDict) and self.pairs == other.pairs
