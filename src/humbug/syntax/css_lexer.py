from dataclasses import dataclass
from typing import Callable, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token


@dataclass
class CSSLexerState(LexerState):
    """
    State information for the CSS lexer.

    Attributes:
        in_comment: Indicates if we're currently parsing a comment
    """
    in_comment: bool = False


class CSSLexer(Lexer):
    """
    Lexer for CSS code.

    This lexer handles CSS-specific syntax including identifiers, dimensions,
    at-rules, hex colors, and CSS-specific operators.
    """

    def __init__(self):
        super().__init__()
        self._in_comment = False

    def lex(self, prev_lexer_state: Optional[CSSLexerState], input_str: str) -> CSSLexerState:
        """
        Lex all the tokens in the input.
        """
        self._input = input_str
        if prev_lexer_state:
            self._in_comment = prev_lexer_state.in_comment

        if self._in_comment:
            self._read_comment(0)

        if not self._in_comment:
            self._inner_lex()

        lexer_state = CSSLexerState(in_comment=False)
        lexer_state.in_comment = self._in_comment
        return lexer_state

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if ch == '\n':
            return self._read_newline

        if self._is_whitespace(ch):
            return self._read_whitespace

        if self._is_letter(ch):
            return self._read_identifier

        if self._is_digit(ch):
            return self._read_number

        if ch in ('"', "'"):
            return self._read_string

        if ch == '/':
            return self._read_forward_slash

        if ch == '#':
            return self._read_hex_or_id

        if ch == '.':
            return self._read_dot

        if ch == '-':
            return self._read_minus

        if ch == '@':
            return self._read_at_rule

        return self._read_operator

    def _read_dot(self) -> None:
        """
        Read a dot operator or start of a class selector.
        """
        if (self._position + 1 < len(self._input) and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        self._read_identifier()

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash operator or start of a comment.
        """
        if (self._position + 1 < len(self._input) and
                self._input[self._position + 1] == '*'):
            self._read_comment(2)
            return

        self._read_operator()

    def _read_minus(self) -> None:
        """
        Read a minus operator, negative number, or identifier starting with a dash.
        """
        if (self._position + 1 < len(self._input) and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        if (self._position + 1 < len(self._input) and
                (self._is_letter(self._input[self._position + 1]) or
                 self._input[self._position + 1] == '-')):
            self._read_identifier()
            return

        self._read_operator()

    def _read_identifier(self) -> None:
        """
        Read a CSS identifier token.
        """
        start = self._position
        while (self._position < len(self._input) and
               self._input[self._position] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.#[]=- '):
            self._position += 1

        self._tokens.append(Token(
            type='IDENTIFIER',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_comment(self, skip_chars: int) -> None:
        """
        Read a CSS /* */ style comment token.
        """
        self._in_comment = True
        start = self._position
        self._position += skip_chars  # Skip /*
        while (self._position + 1) < len(self._input):
            if self._input[self._position] == '*' and self._input[self._position + 1] == '/':
                self._in_comment = False
                self._position += 2
                break

            self._position += 1

        # If we're still in a block comment we've got one character left on this line and
        # we need to include it in the comment too.
        if self._in_comment:
            self._position = len(self._input)

        self._tokens.append(Token(
            type='COMMENT',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_at_rule(self) -> None:
        """
        Read a CSS at-rule token (e.g. @media, @import).
        """
        start = self._position
        self._position += 1
        while (self._position < len(self._input) and
               self._input[self._position] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-'):
            self._position += 1

        self._tokens.append(Token(
            type='CSS_AT_RULE',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric token, which may include dimensions (e.g. px, em, %).
        """
        start = self._position
        if self._input[self._position] == '-':
            self._position += 1

        while (self._position < len(self._input) and
               self._input[self._position] in '0123456789.'):
            self._position += 1

        if (self._position < len(self._input) and
                self._input[self._position] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ%'):
            self._read_dimension(start)
            return

        self._tokens.append(Token(
            type='NUMBER',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_dimension(self, start: int) -> None:
        """
        Read a CSS dimension token (e.g. 12px, 2em, 50%).

        Args:
            start: The starting position of the dimension value
        """
        while (self._position < len(self._input) and
               self._input[self._position] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ%'):
            self._position += 1

        self._tokens.append(Token(
            type='DIMENSION',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_hex_or_id(self) -> None:
        """
        Read a hexadecimal color value or ID selector.
        """
        start = self._position
        self._position += 1

        # Peek ahead to determine if this is a hex value or an ID
        is_hex = (self._position < len(self._input) and
                 self._is_hex_digit(self._input[self._position]))

        while (self._position < len(self._input) and
               self._is_hex_digit(self._input[self._position])):
            self._position += 1

        if is_hex and (self._position - start == 4 or self._position - start == 7):
            self._tokens.append(Token(
                type='HEX',
                value=self._input[start:self._position],
                start=start
            ))
            return

        # If not a valid hex, treat as ID selector
        while (self._position < len(self._input) and
               self._input[self._position] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-'):
            self._position += 1

        self._tokens.append(Token(
            type='HASH',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_operator(self) -> None:
        """
        Read an operator or punctuation token.
        """
        operators = [
            '~=', '$=', '^=', '|=', '*=', '-', '+', '*', '|', '=',
            '>', '(', ')', '{', '}', '[', ']', ';', ':', ','
        ]

        for operator in operators:
            if self._input[self._position:].startswith(operator):
                start = self._position
                self._position += len(operator)
                self._tokens.append(Token(
                    type='OPERATOR',
                    value=operator,
                    start=start
                ))
                return

        start = self._position
        ch = self._input[self._position]
        self._position += 1
        self._tokens.append(Token(type='ERROR', value=ch, start=start))
