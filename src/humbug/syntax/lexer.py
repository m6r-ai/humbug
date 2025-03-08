from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import IntEnum, auto
from typing import List, Callable, Optional, Set, ClassVar


class TokenType(IntEnum):
    """Type of lexical token."""
    ADDRESS = auto()
    ANNOTATION = auto()
    ATTRIBUTE = auto()
    BACKTICK = auto()
    BACKTICK_CODE = auto()
    BOOLEAN = auto()
    CHARACTER = auto()
    CODE = auto()
    COMMENT = auto()
    CSS_AT_RULE = auto()
    DIMENSION = auto()
    DIRECTIVE = auto()
    DOC_COMMENT = auto()
    DOCTYPE = auto()
    DOT = auto()
    ELEMENT = auto()
    ERROR = auto()
    FENCE = auto()
    FENCE_END = auto()
    FENCE_START = auto()
    FUNCTION_OR_METHOD = auto()
    GENERIC_END = auto()
    GENERIC_METHOD = auto()
    GENERIC_START = auto()
    GENERIC_TYPE = auto()
    HASH = auto()
    HEADING = auto()
    HEX = auto()
    HTML_ATTRIBUTE = auto()
    HTML_TAG = auto()
    IDENTIFIER = auto()
    KEYWORD = auto()
    LANGUAGE = auto()
    LIFETIME = auto()
    LINQ_KEYWORD = auto()
    LPAREN = auto()
    METHOD_REFERENCE_OPERATOR = auto()
    NEWLINE = auto()
    NUMBER = auto()
    OPERATOR = auto()
    PREPROCESSOR = auto()
    REGEXP = auto()
    RPAREN = auto()
    RUNE = auto()
    SCRIPT = auto()
    STRING = auto()
    STYLE = auto()
    SYMBOL = auto()
    TEXT = auto()
    TYPE = auto()
    TYPE_PARAMETER = auto()
    VECTOR_START = auto()
    WHITESPACE = auto()
    XML_DOC = auto()


@dataclass
class Token:
    type: TokenType
    value: str
    start: int


@dataclass
class LexerState:
    """
    State information for the Lexer.
    """


class Lexer(ABC):
    """
    Base lexer class with optimized implementation.

    Key optimizations:
    - Character lookup via sets instead of range comparisons
    - Common character classification pre-computation
    - Static lookup tables shared across instances
    - Optimized token handling
    """

    # Character lookup tables - shared by all subclasses
    _WHITESPACE_CHARS: ClassVar[Set[str]] = set(" \t\r\v\f\n\u00A0\u1680\u2028\u2029\u202F\u205F\u3000")
    _LETTER_CHARS: ClassVar[Set[str]] = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    _LETTER_DIGIT_CHARS: ClassVar[Set[str]] = set("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    _DIGIT_CHARS: ClassVar[Set[str]] = set("0123456789")
    _HEX_CHARS: ClassVar[Set[str]] = set("0123456789abcdefABCDEF")
    _BINARY_CHARS: ClassVar[Set[str]] = set("01")
    _OCTAL_CHARS: ClassVar[Set[str]] = set("01234567")

    # Add the Unicode whitespace range \u2000-\u200A
    for i in range(0x2000, 0x200B):
        _WHITESPACE_CHARS.add(chr(i))

    def __init__(self):
        self._input: str = ""
        self._input_len: int = 0
        self._position: int = 0
        self._tokens: List[Token] = []
        self._next_token: int = 0

    @abstractmethod
    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """

    @abstractmethod
    def lex(self, prev_lexer_state: Optional[LexerState], input_str: str) -> LexerState:
        """
        Parse the input string

        Args:
            prev_lexer_state: The previous lexer state, if any
            input_str: The input string to lex

        Returns:
            The updated lexer state
        """

    def _inner_lex(self) -> None:
        """
        Lex all the tokens in the input.
        """
        input_len = len(self._input)
        while self._position < input_len:
            ch = self._input[self._position]
            fn = self._get_lexing_function(ch)
            fn()

    def get_next_token(self, filter_list: List = None) -> Optional[Token]:
        """
        Gets the next token from the input that does not have a type found in the filter list.

        Args:
            filter_list: Optional list of token types to skip

        Returns:
            The next Token available or None if there are no tokens left.
        """
        # Fast path when no filtering needed
        if not filter_list:
            if self._next_token >= len(self._tokens):
                return None

            token = self._tokens[self._next_token]
            self._next_token += 1
            return token

        # Path with filtering
        tokens_len = len(self._tokens)
        while self._next_token < tokens_len:
            token = self._tokens[self._next_token]
            self._next_token += 1
            if token.type not in filter_list:
                return token

        return None

    def peek_next_token(self, filter_list: List = None, offset: int = 0) -> Optional[Token]:
        """
        Get the token that is 'offset' positions ahead, skipping filtered tokens.

        Args:
            filter_list: Optional list of token types to skip
            offset: How many non-filtered tokens to look ahead (default 0)

        Returns:
            The token at the specified offset, or None if none found
        """
        current_token_index = self._next_token
        skipped = 0
        token = None

        while skipped <= offset:
            token = self.get_next_token(filter_list)
            if not token:
                break
            skipped += 1

        self._next_token = current_token_index
        return token

    def _read_string(self) -> None:
        """
        Reads a string token.
        """
        quote = self._input[self._position]
        start = self._position
        self._position += 1
        input_len = len(self._input)

        while self._position < input_len and self._input[self._position] != quote:
            if self._input[self._position] == '\\' and (self._position + 1) < input_len:
                self._position += 1  # Skip the escape character

            self._position += 1

        if self._position < input_len:  # Skip the closing quote if found
            self._position += 1

        string_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.STRING, value=string_value, start=start))

    def _read_newline(self) -> None:
        """
        Reads a newline in the input.
        """
        start = self._position
        self._position += 1
        self._tokens.append(Token(type=TokenType.NEWLINE, value='\n', start=start))

    def _read_whitespace(self) -> None:
        """
        Reads whitespace in the input.
        """
        start = self._position
        self._position += 1
        input_len = len(self._input)

        # Fast path using set-based character classification
        while self._position < input_len and self._input[self._position] in self._WHITESPACE_CHARS and self._input[self._position] != '\n':
            self._position += 1

        whitespace_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.WHITESPACE, value=whitespace_value, start=start))

    def _is_letter(self, ch: str) -> bool:
        """
        Determines if a character is a letter.
        """
        return ch in self._LETTER_CHARS
    def _is_octal_digit(self, ch: str) -> bool:
        """
        Determines if a character is an octal digit.
        """
        return ch in self._OCTAL_CHARS


    def _is_binary_digit(self, ch: str) -> bool:
        """
        Determines if a character is a binary digit.
        """
        return ch in self._BINARY_CHARS

    def _is_digit(self, ch: str) -> bool:
        """
        Determines if a character is a digit.
        """
        return ch in self._DIGIT_CHARS

    def _is_hex_digit(self, ch: str) -> bool:
        """
        Determines if a character is a hexadecimal digit.
        """
        return ch in self._HEX_CHARS

    def _is_letter_or_digit(self, ch: str) -> bool:
        """
        Determines if a character is a letter or digit.
        """
        return ch in self._LETTER_DIGIT_CHARS

    def _is_whitespace(self, ch: str) -> bool:
        """
        Determines if a character is a non-newline whitespace.
        """
        return ch in self._WHITESPACE_CHARS and ch != '\n'
