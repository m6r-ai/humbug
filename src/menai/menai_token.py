"""Token types and token representation for Menai expressions."""

from dataclasses import dataclass
from enum import Enum
from typing import Any


class MenaiTokenType(Enum):
    """Token types for Menai expressions."""
    LPAREN = "("
    RPAREN = ")"
    QUOTE = "'"
    SYMBOL = "SYMBOL"
    INTEGER = "INTEGER"
    FLOAT = "FLOAT"
    COMPLEX = "COMPLEX"
    STRING = "STRING"
    BOOLEAN = "BOOLEAN"
    COMMENT = "COMMENT"
    NONE = "NONE"


@dataclass
class MenaiToken:
    """Represents a single token in an Menai expression."""
    type: MenaiTokenType
    value: Any
    length: int = 1
    line: int = 1  # Line number (1-indexed)
    column: int = 1  # Column number (1-indexed)

    def __repr__(self) -> str:
        return f"MenaiToken({self.type.name}, {self.value!r}, line={self.line}, col={self.column})"
