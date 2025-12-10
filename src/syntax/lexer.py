from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import IntEnum, auto
from typing import List, Callable, Set, ClassVar, Dict


class TokenType(IntEnum):
    """Type of lexical token."""
    ADDRESS = auto()
    ANNOTATION = auto()
    ARGUMENT = auto()
    ATTRIBUTE = auto()
    BACKTICK = auto()
    BLOCKQUOTE = auto()
    BOLD = auto()
    BOLD_END = auto()
    BOLD_START = auto()
    BOOLEAN = auto()
    CHARACTER = auto()
    CODE = auto()
    COMMAND = auto()
    COMMENT = auto()
    CSS_AT_RULE = auto()
    DIMENSION = auto()
    DIRECTIVE = auto()
    DOC_COMMENT = auto()
    DIFF_ADDED = auto()
    DIFF_CHANGED = auto()
    DIFF_REMOVED = auto()
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
    JSON_KEY = auto()
    HASH = auto()
    HEADING = auto()
    HEX = auto()
    HORIZONTAL_RULE = auto()
    HTML_ATTRIBUTE = auto()
    HTML_TAG = auto()
    IDENTIFIER = auto()
    IMAGE_ALT_END = auto()
    IMAGE_ALT_TEXT = auto()
    IMAGE_END = auto()
    IMAGE_START = auto()
    IMAGE_URL = auto()
    INLINE_CODE = auto()
    INLINE_CODE_END = auto()
    INLINE_CODE_START = auto()
    ITALIC = auto()
    ITALIC_END = auto()
    ITALIC_START = auto()
    KEYWORD = auto()
    LANGUAGE = auto()
    LIFETIME = auto()
    LINK_END = auto()
    LINK_START = auto()
    LINK_TEXT = auto()
    LINK_TEXT_END = auto()
    LINK_URL = auto()
    LINQ_KEYWORD = auto()
    LIST_MARKER = auto()
    LPAREN = auto()
    METHOD_REFERENCE_OPERATOR = auto()
    NUMBER = auto()
    OPERATOR = auto()
    OPTION = auto()
    OPTION_VALUE = auto()
    PREPROCESSOR = auto()
    QUOTE = auto()
    REGEXP = auto()
    RPAREN = auto()
    RUNE = auto()
    SCRIPT = auto()
    STRING = auto()
    STYLE = auto()
    TABLE = auto()
    TEXT = auto()
    TYPE = auto()
    TYPE_PARAMETER = auto()
    VECTOR_START = auto()
    XML_DOC = auto()

@dataclass
class Token:
    """
    Represents a token in the input stream.

    Attributes:
        type: The type of the token
        value: The string value of the token
        start: The starting position of the token in the input stream
    """
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
    _WHITESPACE_CHARS: ClassVar[Set[str]] = set(" \t\r\v\f\u00A0\u1680\u2028\u2029\u202F\u205F\u3000")
    _LETTER_CHARS: ClassVar[Set[str]] = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    _LETTER_DIGIT_CHARS: ClassVar[Set[str]] = set("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    _LETTER_DIGIT_UNDERSCORE_CHARS: ClassVar[Set[str]] = set("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
    _DIGIT_CHARS: ClassVar[Set[str]] = set("0123456789")
    _HEX_CHARS: ClassVar[Set[str]] = set("0123456789abcdefABCDEF")
    _BINARY_CHARS: ClassVar[Set[str]] = set("01")
    _OCTAL_CHARS: ClassVar[Set[str]] = set("01234567")

    # Add the Unicode whitespace range \u2000-\u200A
    for i in range(0x2000, 0x200B):
        _WHITESPACE_CHARS.add(chr(i))

    # Default empty operator map - to be overridden by subclasses
    _OPERATORS: ClassVar[List[str]] = []
    _OPERATORS_MAP: ClassVar[Dict[str, List[str]]] = {}

    def __init__(self) -> None:
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
    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> LexerState | None:
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
        while self._position < self._input_len:
            ch = self._input[self._position]
            self._get_lexing_function(ch)()

    def get_next_token(self) -> Token | None:
        """
        Gets the next token from the input.

        Returns:
            The next Token available or None if there are no tokens left.
        """
        if self._next_token >= len(self._tokens):
            return None

        token = self._tokens[self._next_token]
        self._next_token += 1
        return token

    def peek_next_token(self, offset: int = 0) -> Token | None:
        """
        Get the token that is 'offset' positions ahead.

        Args:
            offset: How many non-filtered tokens to look ahead (default 0)

        Returns:
            The token at the specified offset, or None if none found
        """
        current_token_index = self._next_token
        skipped = 0
        token = None

        while skipped <= offset:
            token = self.get_next_token()
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

        while self._position < self._input_len and self._input[self._position] != quote:
            if self._input[self._position] == '\\' and (self._position + 1) < self._input_len:
                self._position += 1  # Skip the escape character

            self._position += 1

        if self._position < self._input_len:  # Skip the closing quote if found
            self._position += 1

        string_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.STRING, value=string_value, start=start))

    def _read_whitespace(self) -> None:
        """
        Reads whitespace in the input.
        """
        self._position += 1

        # Fast path using set-based character classification
        while self._position < self._input_len and self._input[self._position] in self._WHITESPACE_CHARS:
            self._position += 1

    def _read_operator(self) -> None:
        """
        Generic implementation of operator reading using the operator map.

        The operator map should be a dictionary where:
        - Keys are the first characters of operators
        - Values are lists of operators starting with that character,
          ordered from longest to shortest to ensure greedy matching
        """
        first_char = self._input[self._position]
        potential_operators = self._OPERATORS_MAP.get(first_char, [])

        # Try to match the longest operator first
        for op in potential_operators:
            if self._input[self._position:].startswith(op):
                start = self._position
                self._position += len(op)
                self._tokens.append(Token(
                    type=TokenType.OPERATOR,
                    value=op,
                    start=start
                ))
                return

        # If no operator matched, it's an error or a single character
        start = self._position
        ch = self._input[self._position]
        self._position += 1
        self._tokens.append(Token(type=TokenType.ERROR, value=ch, start=start))

    @staticmethod
    def build_operator_map(operators: List[str]) -> Dict[str, List[str]]:
        """
        Build an operator map from a list of operators.

        Args:
            operators: List of operator strings

        Returns:
            A dictionary mapping first characters to lists of operators
            starting with that character, sorted by length (longest first)
        """
        operator_map: Dict[str, List[str]] = {}
        for op in operators:
            if not op:
                continue

            first_char = op[0]
            if first_char not in operator_map:
                operator_map[first_char] = []

            operator_map[first_char].append(op)

        # Sort each list by length, longest first to ensure greedy matching
        for _first_char, operators_list in operator_map.items():
            operators_list.sort(key=len, reverse=True)

        return operator_map

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

    def _is_letter_or_digit_or_underscore(self, ch: str) -> bool:
        """
        Determines if a character is a letter, digit, or underscore.
        """
        return ch in self._LETTER_DIGIT_UNDERSCORE_CHARS

    def _is_whitespace(self, ch: str) -> bool:
        """
        Determines if a character is a non-newline whitespace.
        """
        return ch in self._WHITESPACE_CHARS
