from dataclasses import dataclass
from typing import Callable, cast

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class PythonLexerState(LexerState):
    """
    State information for the Python lexer.

    Attributes:
        in_docstring: Indicates if we're currently parsing a docstring
    """
    in_docstring: bool = False
    docstring_quote: str = ""


class PythonLexer(Lexer):
    """
    Lexer for Python code.

    This lexer handles Python-specific syntax including keywords, operators, numbers,
    strings, docstrings, and comments.
    """

    # Operators list
    _OPERATORS = [
        '...', '>>=', '<<=', '**=', '//=', '@=', ':=', '!=', '==',
        '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=',
        '<=', '>=', '<<', '>>', '++', '--', '**', '//',
        '->', '@', '+', '-', '*', '/', '%', '&', '~', '|',
        '^', '=', '<', '>', '(', ')', '{', '}', '[', ']',
        ':', '.', ','
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self) -> None:
        super().__init__()
        self._in_docstring = False
        self._docstring_quote = ""

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> PythonLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing

        Raises:
            TypeError: If the previous lexer state is not None and not a PythonLexerState instance
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state:
            prev_lexer_state = cast(PythonLexerState, prev_lexer_state)
            self._in_docstring = prev_lexer_state.in_docstring
            self._docstring_quote = prev_lexer_state.docstring_quote

        if self._in_docstring:
            self._read_docstring(self._docstring_quote, 0)

        if not self._in_docstring:
            self._inner_lex()

        lexer_state = PythonLexerState()
        lexer_state.in_docstring = self._in_docstring
        lexer_state.docstring_quote = self._docstring_quote
        return lexer_state

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_whitespace(ch):
            return self._read_whitespace

        if self._is_digit(ch):
            return self._read_number

        if ch == '.':
            return self._read_dot

        if ch in ('"', "'"):
            return self._read_quote

        if ch == '#':
            return self._read_comment

        return self._read_operator

    def _read_quote(self) -> None:
        """
        Read a string or docstring token.

        Handles both single/double quoted strings and triple-quoted docstrings.
        """
        ch: str = self._input[self._position]
        if (self._position + 2 < self._input_len and
                self._input[self._position + 1] == ch and
                self._input[self._position + 2] == ch):
            self._docstring_quote = ch
            self._read_docstring(ch, 3)
            return

        self._read_string()

    def _read_dot(self) -> None:
        """
        Read a dot operator or decimal point in a number.
        """
        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        self._read_operator()

    def _read_number(self) -> None:
        """
        Read a numeric literal.

        Handles decimal, hexadecimal, binary, octal, and complex number literals.
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
            elif next_char == 'b':  # Binary
                self._position += 2
                while (self._position < self._input_len and
                       self._is_binary_digit(self._input[self._position])):
                    self._position += 1
            elif next_char == 'o':  # Octal
                self._position += 2
                while (self._position < self._input_len and
                       self._is_octal_digit(self._input[self._position])):
                    self._position += 1
            else:  # Decimal or floating-point
                self._read_decimal_number()
        else:
            self._read_decimal_number()

        # Check for complex number 'j' suffix
        if (self._position < self._input_len and
                self._input[self._position] == 'j'):
            self._position += 1

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

        if (self._position < self._input_len and
                self._input[self._position].lower() == 'e'):
            self._position += 1
            if self._input[self._position] in ('+', '-'):
                self._position += 1

            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

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

    def _read_docstring(self, quote_char: str, skip_chars: int) -> None:
        """
        Read a docstring token.

        Args:
            quote_char: The quote character used (single or double quote)
        """
        self._in_docstring = True
        start = self._position
        self._position += skip_chars  # Skip /*
        while (self._position + 2) < self._input_len:
            if (self._input[self._position] == quote_char and
                    self._input[self._position + 1] == quote_char and
                    self._input[self._position + 2] == quote_char):
                self._in_docstring = False
                self._position += 3
                break

            self._position += 1

        # If we're still in a docstring we need to consume the whole line
        if self._in_docstring:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a Python keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Python keyword, False otherwise
        """
        keywords = {
            'and', 'as', 'assert', 'async', 'await', 'break', 'class',
            'continue', 'def', 'del', 'elif', 'else', 'except', 'False',
            'finally', 'for', 'from', 'global', 'if', 'import', 'in',
            'is', 'lambda', 'None', 'nonlocal', 'not', 'or', 'pass',
            'raise', 'return', 'True', 'try', 'while', 'with', 'yield'
        }
        return value in keywords
