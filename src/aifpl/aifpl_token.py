"""Token types and token representation for AIFPL expressions."""

from dataclasses import dataclass
from enum import Enum
from typing import Any


class AIFPLTokenType(Enum):
    """Token types for AIFPL expressions."""
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


@dataclass
class AIFPLToken:
    """Represents a single token in an AIFPL expression."""
    type: AIFPLTokenType
    value: Any
    length: int = 1
    line: int = 1  # Line number (1-indexed)
    column: int = 1  # Column number (1-indexed)

    def __repr__(self) -> str:
        return f"AIFPLToken({self.type.name}, {self.value!r}, line={self.line}, col={self.column})"
