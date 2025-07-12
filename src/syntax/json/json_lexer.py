from dataclasses import dataclass
from typing import Callable

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class JSONLexerState(LexerState):
    """
    State information for the JSON lexer.
    """


class JSONLexer(Lexer):
    """
    Lexer for JSON.

    This lexer handles JSON-specific syntax including strings, numbers,
    booleans, null, and structural elements.
    """

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> JSONLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            Updated lexer state
        """
        self._input = input_str
        self._input_len = len(input_str)
        self._inner_lex()
        return JSONLexerState()

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if self._is_whitespace(ch):
            return self._read_whitespace

        if ch == '"':
            return self._read_string

        if ch in '{}[],:':
            return self._read_punctuation

        if ch == '-' or self._is_digit(ch):
            return self._read_number

        return self._read_keyword

    def _read_string(self) -> None:
        """
        Read a JSON string token.

        Handles escape sequences within strings.  JSON strings must be single-line.
        """
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\' and self._position + 1 < self._input_len:
                # Handle escape sequences
                next_ch = self._input[self._position + 1]
                if next_ch in '"\\bfnrt/':  # Valid JSON escape sequences
                    self._position += 2
                    continue

                if next_ch == 'u':  # Unicode escape
                    if self._position + 5 < self._input_len:
                        hex_digits = self._input[self._position + 2:self._position + 6]
                        if all(self._is_hex_digit(d) for d in hex_digits):
                            self._position += 6
                            continue

                    # Invalid unicode escape sequence
                    self._tokens.append(Token(
                        type=TokenType.ERROR,
                        value=self._input[start:self._position + 2],
                        start=start
                    ))
                    return

            if ch == '"':
                self._position += 1
                self._tokens.append(Token(
                    type=TokenType.STRING,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            self._position += 1
            if self._position >= self._input_len:
                # Unterminated string - this is an error in JSON
                self._tokens.append(Token(
                    type=TokenType.ERROR,
                    value=self._input[start:self._position],
                    start=start
                ))
                return


        # Unterminated string
        self._tokens.append(Token(
            type=TokenType.ERROR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a JSON number token.

        Handles integers, decimals, and scientific notation according to JSON spec.
        """
        start = self._position

        # Handle negative numbers
        if self._input[self._position] == '-':
            self._position += 1

        # Leading zero must not be followed by another digit
        if (self._position < self._input_len and
                self._input[self._position] == '0' and
                self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            self._position += 2
            self._tokens.append(Token(
                type=TokenType.ERROR,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Read integer part
        if not self._read_digits():
            self._tokens.append(Token(
                type=TokenType.ERROR,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Handle decimal point
        if (self._position < self._input_len and
                self._input[self._position] == '.'):
            self._position += 1
            if not self._read_digits():
                self._tokens.append(Token(
                    type=TokenType.ERROR,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        # Handle exponent
        if (self._position < self._input_len and
                self._input[self._position].lower() == 'e'):
            self._position += 1
            if (self._position < self._input_len and
                    self._input[self._position] in '+-'):
                self._position += 1

            if not self._read_digits():
                self._tokens.append(Token(
                    type=TokenType.ERROR,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_digits(self) -> bool:
        """
        Read a sequence of digits.

        Returns:
            bool: True if at least one digit was read, False otherwise
        """
        if self._position >= self._input_len or not self._is_digit(self._input[self._position]):
            return False

        while (self._position < self._input_len and self._is_digit(self._input[self._position])):
            self._position += 1

        return True

    def _read_keyword(self) -> None:
        """
        Read a JSON keyword token (true, false, or null).
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               self._is_letter(self._input[self._position])):
            self._position += 1

        value = self._input[start:self._position]
        if value in ('true', 'false', 'null'):
            self._tokens.append(Token(
                type=TokenType.KEYWORD,
                value=value,
                start=start
            ))
            return

        self._tokens.append(Token(
            type=TokenType.ERROR,
            value=value,
            start=start
        ))

    def _read_punctuation(self) -> None:
        """
        Read a JSON punctuation token ({, }, [, ], :, or ,).
        """
        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value=self._input[start:self._position],
            start=start
        ))
