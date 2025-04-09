from dataclasses import dataclass
from enum import IntEnum, auto


class MetaphorTokenType(IntEnum):
    """
    Enum-like class representing different types of tokens in the source file.
    """
    NONE = auto()
    INDENT = auto()
    OUTDENT = auto()
    INCLUDE = auto()
    EMBED = auto()
    KEYWORD_TEXT = auto()
    TEXT = auto()
    ACTION = auto()
    CONTEXT = auto()
    ROLE = auto()
    BAD_INDENT = auto()
    BAD_OUTDENT = auto()
    TAB = auto()
    END_OF_FILE = auto()
    CODE = auto()


@dataclass(frozen=True)
class MetaphorToken:
    """
    Represents a token in the input stream.

    Attributes:
        type (TokenType): The type of the token (e.g., TEXT, ACTION).
        value (str): The actual string value of the token.
        input (str): The entire line of input where the token appears.
        filename (str): The file where the token was read from.
        line (int): The line number in the file where the token is located.
        column (int): The column number where the token starts.
    """
    type: MetaphorTokenType
    value: str
    input: str
    filename: str
    line: int
    column: int
