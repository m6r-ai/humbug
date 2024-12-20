from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Callable, Optional


@dataclass
class Token:
    type: str
    value: str
    start: int


@dataclass
class LexerState:
    """
    State information for the Lexer.
    """


class Lexer(ABC):
    def __init__(self):
        self._input: str = None
        self._position: int = 0
        self._tokens: List[Token] = []
        self._next_token: int = 0

        # Initialize lexing functions for ASCII characters (0-127)
        self._lexing_functions: List[Callable[[], None]] = [self._get_lexing_function(chr(i)) for i in range(128)]

    @abstractmethod
    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        :param ch: The start character
        :return: The lexing function
        """

    @abstractmethod
    def lex(self, prev_lexer_state: Optional[LexerState], input_str: str) -> LexerState:
        """
        Parse the input string
        """

    def _inner_lex(self) -> None:
        """
        Lex all the tokens in the input.
        """
        while self._position < len(self._input):
            ch = self._input[self._position]
            ch_val = ord(ch)
            if ch_val < 128:
                fn = self._lexing_functions[ch_val]
            else:
                fn = self._get_lexing_function(ch)

            fn()

    def get_next_token(self, filter_list: List=None) -> Optional[Token]:
        """
        Gets the next token from the input that does not have a type found in the filter list.

        :return: The next Token available or None if there are no tokens left.
        """
        while True:
            if self._next_token >= len(self._tokens):
                return None

            token = self._tokens[self._next_token]
            self._next_token += 1
            if (not filter_list) or (token.type not in filter_list):
                return token

    def peek_next_token(self, filter_list: List=None) -> Optional[Token]:
        """
        Get the next token that does not have a type found in the filter list.

        :return: The next syntactic Token or None if none found.
        """
        current_token_index = self._next_token
        token = self.get_next_token(filter_list)
        self._next_token = current_token_index
        return token

    def _read_string(self) -> None:
        """
        Reads a string token.
        """
        quote: str = self._input[self._position]
        start = self._position
        self._position += 1

        while self._position < len(self._input) and self._input[self._position] != quote:
            if self._input[self._position] == '\\' and (self._position + 1) < len(self._input):
                self._position += 1  # Skip the escape character

            self._position += 1

        self._position += 1  # Skip the closing quote
        string_value = self._input[start:self._position]
        self._tokens.append(Token(type='STRING', value=string_value, start=start))

    def _read_newline(self) -> None:
        """
        Reads a newline in the input.
        """
        start = self._position
        self._position += 1
        self._tokens.append(Token(type='NEWLINE', value='\n', start=start))

    def _read_whitespace(self) -> None:
        """
        Reads whitespace in the input.
        """
        start = self._position
        self._position += 1
        while self._position < len(self._input) and self._is_whitespace(self._input[self._position]):
            self._position += 1

        whitespace_value = self._input[start:self._position]
        self._tokens.append(Token(type='WHITESPACE', value=whitespace_value, start=start))

    # Helper Methods

    def _is_letter(self, ch: str) -> bool:
        """
        Determines if a character is a letter.

        :param ch: The character to check.
        :return: True if the character is a letter, False otherwise.
        """
        return ('a' <= ch <= 'z') or ('A' <= ch <= 'Z')

    def _is_octal_digit(self, ch: str) -> bool:
        """
        Determines if a character is an octal digit.

        :param ch: The character to check.
        :return: True if the character is an octal digit, False otherwise.
        """
        return '0' <= ch <= '7'

    def _is_binary_digit(self, ch: str) -> bool:
        """
        Determines if a character is a binary digit.

        :param ch: The character to check.
        :return: True if the character is a binary digit, False otherwise.
        """
        return ch in ('0', '1')

    def _is_digit(self, ch: str) -> bool:
        """
        Determines if a character is a digit.

        :param ch: The character to check.
        :return: True if the character is a digit, False otherwise.
        """
        return '0' <= ch <= '9'

    def _is_hex_digit(self, ch: str) -> bool:
        """
        Determines if a character is a hexadecimal digit.

        :param ch: The character to check.
        :return: True if the character is a hexadecimal digit, False otherwise.
        """
        return self._is_digit(ch) or ('a' <= ch <= 'f') or ('A' <= ch <= 'F')

    def _is_letter_or_digit(self, ch: str) -> bool:
        """
        Determines if a character is a letter or digit.

        :param ch: The character to check.
        :return: True if the character is a letter or digit, False otherwise.
        """
        return self._is_letter(ch) or self._is_digit(ch)

    def _is_whitespace(self, ch: str) -> bool:
        """
        Determines if a character is a non-newline whitespace.

        :param ch: The character to check.
        :return: True if the character is a non-newline whitespace character, False otherwise.
        """
        whitespace_chars = {
            ' ', '\t', '\n', '\r', '\v', '\f',
            '\u00A0', '\u1680',
            # Range \u2000 to \u200A
            *(chr(c) for c in range(0x2000, 0x200B)),
            '\u2028', '\u2029', '\u202F', '\u205F', '\u3000'
        }
        return ch in whitespace_chars
