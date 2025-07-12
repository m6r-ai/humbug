"""
Solidity Lexer

This module implements a lexer for Solidity smart contract code, extending the functionality of the base lexer.
"""

from dataclasses import dataclass
from typing import Callable

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class SolidityLexerState(LexerState):
    """
    State information for the Solidity lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        in_natspec_comment: Indicates if we're currently parsing a NatSpec comment
    """
    in_block_comment: bool = False
    in_natspec_comment: bool = False


class SolidityLexer(Lexer):
    """
    Lexer for Solidity smart contract code.

    This lexer handles Solidity-specific syntax including keywords, operators, numbers,
    strings, comments, and NatSpec documentation comments.
    """

    # Operators list
    _OPERATORS = [
        '**=', '<<=', '>>=', '&&=', '||=', '!=', '==', '+=', '-=', '*=',
        '/=', '%=', '&=', '|=', '^=', '<=', '>=', '&&', '||', '<<',
        '>>', '++', '--', '=>', '+', '-', '*', '/', '%', '&',
        '~', '!', '|', '^', '=', '<', '>', '(', ')', '{', '}', '[',
        ']', ';', ':', '?', '.', ',', '**'
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self) -> None:
        super().__init__()
        self._in_block_comment = False
        self._in_natspec_comment = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> SolidityLexerState:
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
            assert isinstance(prev_lexer_state, SolidityLexerState), \
                f"Expected SolidityLexerState, got {type(prev_lexer_state).__name__}"
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._in_natspec_comment = prev_lexer_state.in_natspec_comment

        if self._in_block_comment:
            self._read_block_comment(0)

        if self._in_natspec_comment:
            self._read_natspec_block_comment(0)

        if not self._in_block_comment and not self._in_natspec_comment:
            self._inner_lex()

        lexer_state = SolidityLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.in_natspec_comment = self._in_natspec_comment
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

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch in ('"', "'"):
            return self._read_string

        if ch == '.':
            return self._read_dot

        if ch == '/':
            return self._read_forward_slash

        return self._read_operator

    def _read_dot(self) -> None:
        """
        Read a dot operator or decimal point in a number.
        """
        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        self._read_operator()

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be a comment, block comment,
        NatSpec comment, or division operator.
        """
        if self._position + 1 < self._input_len:
            if self._input[self._position + 1] == '/':
                # Check for NatSpec comment (///)
                if (self._position + 2 < self._input_len and
                        self._input[self._position + 2] == '/'):
                    self._read_natspec_line_comment()
                    return

                # Regular single line comment
                self._read_comment()
                return

            if self._input[self._position + 1] == '*':
                # Check for NatSpec comment (/**)
                if (self._position + 2 < self._input_len and
                        self._input[self._position + 2] == '*'):
                    self._read_natspec_block_comment(3)
                    return

                # Regular block comment
                self._read_block_comment(2)
                return

        self._read_operator()

    def _read_comment(self) -> None:
        """
        Read a single-line comment token.
        """
        start = self._position
        self._position = self._input_len  # Skip to the end of the line

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_natspec_line_comment(self) -> None:
        """
        Read a NatSpec single-line comment token.
        """
        start = self._position
        self._position = self._input_len  # Skip to the end of the line

        self._tokens.append(Token(
            type=TokenType.DOC_COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_block_comment(self, skip_chars: int) -> None:
        """
        Read a block comment token.
        """
        self._in_block_comment = True
        start = self._position
        self._position += skip_chars
        while (self._position + 1) < self._input_len:
            if self._input[self._position] == '*' and self._input[self._position + 1] == '/':
                self._in_block_comment = False
                self._position += 2
                break

            self._position += 1

        # If we're still in a block comment, consume the rest of the line
        if self._in_block_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_natspec_block_comment(self, skip_chars: int) -> None:
        """
        Read a NatSpec block comment token.
        """
        self._in_natspec_comment = True
        start = self._position
        self._position += skip_chars
        while (self._position + 1) < self._input_len:
            if self._input[self._position] == '*' and self._input[self._position + 1] == '/':
                self._in_natspec_comment = False
                self._position += 2
                break

            self._position += 1

        # If we're still in a NatSpec comment, consume the rest of the line
        if self._in_natspec_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.DOC_COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_string(self) -> None:
        """
        Read a string token.
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

    def _read_number(self) -> None:
        """
        Read a numeric literal.

        Handles decimal, hexadecimal, wei/ether units, and scientific notation.
        """
        start = self._position

        if (self._input[self._position] == '0' and
                self._position + 1 < self._input_len):
            next_char = self._input[self._position + 1].lower()
            if next_char == 'x':  # Hexadecimal
                self._position += 2
                while (self._position < self._input_len and
                       self._is_hex_digit(self._input[self._position])):
                    self._position += 1
            else:  # Decimal or floating-point
                self._read_decimal_number()
        else:
            self._read_decimal_number()

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_decimal_number(self) -> None:
        """
        Read a decimal or floating-point number.
        """
        while (self._position < self._input_len and
               self._is_digit(self._input[self._position])):
            self._position += 1

        if (self._position < self._input_len and
                self._input[self._position] == '.'):
            self._position += 1
            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

        # Check for scientific notation
        if (self._position < self._input_len and
                self._input[self._position].lower() == 'e'):
            self._position += 1
            if self._position < self._input_len and self._input[self._position] in ('+', '-'):
                self._position += 1

            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

        # Check for wei/ether unit suffixes (e.g., wei, gwei, ether)
        if self._position < self._input_len and self._is_letter(self._input[self._position]):
            while (self._position < self._input_len and
                   (self._is_letter(self._input[self._position]) or
                    self._input[self._position] == '_')):
                self._position += 1

    def _read_identifier_or_keyword(self) -> None:
        """
        Read an identifier or keyword token.
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]))):
            self._position += 1

        value = self._input[start:self._position]
        if self._is_keyword(value):
            self._tokens.append(Token(type=TokenType.KEYWORD, value=value, start=start))
            return

        self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a Solidity keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Solidity keyword, False otherwise
        """
        keywords = {
            # Current keywords
            'abstract', 'anonymous', 'as', 'assembly', 'break', 'calldata',
            'catch', 'constant', 'constructor', 'continue', 'contract',
            'do', 'else', 'emit', 'enum', 'event', 'external', 'fallback',
            'for', 'function', 'hex', 'if', 'immutable', 'import', 'indexed',
            'interface', 'internal', 'is', 'library', 'mapping', 'memory', 'modifier',
            'new', 'override', 'payable', 'pragma', 'private', 'public', 'pure',
            'receive', 'return', 'returns', 'storage', 'struct', 'throw',
            'try', 'type', 'unchecked', 'unicode', 'using', 'view', 'virtual', 'while',

            # Ether subdemonination keywords
            'wei', 'gwei', 'ether', 'seconds', 'minutes', 'hours', 'days', 'weeks', 'years',

            # Type keywords
            'address', 'bool', 'bytes', 'fixed', 'int', 'string', 'uint',
            'int8', 'int16', 'int32', 'int64', 'int128', 'int256',
            'ufixed', 'uint8', 'uint16', 'uint32', 'uint64', 'uint128', 'uint256',

            # Literal keywords
            'false', 'true'
        }
        return value in keywords
