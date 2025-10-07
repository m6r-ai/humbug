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
    NUMBER = "NUMBER"
    STRING = "STRING"
    BOOLEAN = "BOOLEAN"


@dataclass
class AIFPLToken:
    """Represents a single token in an AIFPL expression."""
    type: AIFPLTokenType
    value: Any
    position: int
    length: int = 1

    def __repr__(self) -> str:
        return f"AIFPLToken({self.type.name}, {self.value!r}, pos={self.position})"
