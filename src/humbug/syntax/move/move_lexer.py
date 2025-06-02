from dataclasses import dataclass
from typing import Callable

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class MoveLexerState(LexerState):
    """
    State information for the Move lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
    """
    in_block_comment: bool = False


class MoveLexer(Lexer):
    """
    Lexer for Move code.

    This lexer handles Move-specific syntax including:
    - Keywords and type names
    - Address literals (e.g., @0x42)
    - Module access paths (e.g., 0x1::string)
    - Vector literals (e.g., vector<u8>)
    - Numeric literals with type suffixes
    - Comments (// and /* */ style)
    """

    # Operators list
    _OPERATORS = [
        '#[', '::', '==', '!=', '<=', '>=', '=', '<', '>', '+', '-',
        '*', '/', '%', '&', '|', '^', '!', '(', ')', '{', '}',
        '[', ']', ';', ':', ',', '.'
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self) -> None:
        super().__init__()
        self._in_block_comment = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> MoveLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state is not None:
            assert isinstance(prev_lexer_state, MoveLexerState), \
                f"Expected MoveLexerState, got {type(prev_lexer_state).__name__}"
            self._in_block_comment = prev_lexer_state.in_block_comment

        if self._in_block_comment:
            self._read_block_comment(0)

        if not self._in_block_comment:
            self._inner_lex()

        lexer_state = MoveLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        return lexer_state

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

        if ch == '@':
            return self._read_address

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch in ('"', "'"):
            return self._read_string

        if ch == '/':
            return self._read_forward_slash

        return self._read_operator

    def _read_address(self) -> None:
        """
        Read an address literal token (e.g., @0x1).
        """
        start = self._position
        self._position += 1  # Skip @

        # Check for hex literal
        if (self._position + 1 < self._input_len and
                self._input[self._position:self._position + 2].lower() == '0x'):
            self._position += 2
            while (self._position < self._input_len and
                   self._is_hex_digit(self._input[self._position])):
                self._position += 1

        self._tokens.append(Token(
            type=TokenType.ADDRESS,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles:
        - Hexadecimal literals (0x...)
        - Decimal integers
        - Type suffixes (u8, u64, u128)
        """
        start = self._position

        # Check for hex literal
        if (self._input[self._position] == '0' and
                self._position + 1 < self._input_len and
                self._input[self._position + 1].lower() == 'x'):
            self._position += 2
            while (self._position < self._input_len and
                   self._is_hex_digit(self._input[self._position])):
                self._position += 1

            # If followed by ::, this is an address
            if (self._position + 1 < self._input_len and
                    self._input[self._position:self._position + 2] == '::'):
                self._tokens.append(Token(
                    type=TokenType.ADDRESS,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            self._tokens.append(Token(
                type=TokenType.NUMBER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Decimal number
        while (self._position < self._input_len and
               self._is_digit(self._input[self._position])):
            self._position += 1

        # Handle type suffixes
        if (self._position < self._input_len and
                self._input[self._position] == 'u'):
            self._position += 1
            if self._position < self._input_len:
                if self._input[self._position] == '8':
                    self._position += 1
                elif (self._position + 1 < self._input_len and
                      self._input[self._position:self._position + 2] == '64'):
                    self._position += 2
                elif (self._position + 2 < self._input_len and
                      self._input[self._position:self._position + 3] == '128'):
                    self._position += 3

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be a comment or operator.
        """
        if self._position + 1 < self._input_len:
            if self._input[self._position + 1] == '/':
                self._read_comment()
                return

            if self._input[self._position + 1] == '*':
                self._read_block_comment(2)
                return

        self._read_operator()

    def _read_comment(self) -> None:
        """
        Read a single-line comment token.
        """
        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _read_block_comment(self, skip_chars: int) -> None:
        """
        Read a block comment token.
        """
        self._in_block_comment = True
        start = self._position
        self._position += skip_chars  # Skip /*

        while (self._position + 1) < self._input_len:
            if (self._input[self._position] == '*' and
                    self._input[self._position + 1] == '/'):
                self._in_block_comment = False
                self._position += 2
                break

            self._position += 1

        if self._in_block_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_identifier_or_keyword(self) -> None:
        """
        Read an identifier or keyword token.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
                self._is_letter_or_digit_or_underscore(self._input[self._position])):
            self._position += 1

        value = self._input[start:self._position]
        if self._is_keyword(value):
            self._tokens.append(Token(type=TokenType.KEYWORD, value=value, start=start))
            return

        self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a Move keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Move keyword, False otherwise
        """
        keywords = {
            'abort', 'acquires', 'as', 'break', 'const', 'continue',
            'copy', 'else', 'false', 'fun', 'if', 'invariant',
            'let', 'loop', 'module', 'move', 'native', 'public',
            'return', 'script', 'spec', 'struct', 'true', 'use',
            'while', 'friend', 'in', 'mut', 'ref', 'where',
            # Types
            'u8', 'u64', 'u128', 'vector', 'address', 'signer',
            'bool'
        }
        return value in keywords
