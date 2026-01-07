"""
C Lexer

This module implements a lexer for C code, extending the functionality of the base lexer.
"""

from dataclasses import dataclass
from typing import Callable

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class CLexerState(LexerState):
    """
    State information for the C lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
    """
    in_block_comment: bool = False


class CLexer(Lexer):
    """
    Lexer for C code.

    This lexer handles C-specific syntax including keywords, operators, numbers,
    strings, comments, and preprocessor directives.
    """

    # Operators list
    _OPERATORS = [
        '>>=', '<<=', '&&=', '||=', '!=', '==', '+=', '-=', '*=',
        '/=', '%=', '&=', '|=', '^=', '<=', '>=', '&&', '||', '<<',
        '>>', '++', '--', '->', '+', '-', '*', '/', '%', '&',
        '~', '!', '|', '^', '=', '<', '>', '(', ')', '{', '}', '[',
        ']', ';', ':', '?', '.', ','
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self) -> None:
        super().__init__()
        self._in_block_comment = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> CLexerState:
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
            assert isinstance(prev_lexer_state, CLexerState), \
                f"Expected CLexerState, got {type(prev_lexer_state).__name__}"
            self._in_block_comment = prev_lexer_state.in_block_comment

        if self._in_block_comment:
            self._read_block_comment(0)

        if not self._in_block_comment:
            self._inner_lex()

        lexer_state = CLexerState()
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

        if ch == 'L':
            return self._read_l

        if ch == 'u':
            return self._read_u

        if ch == 'U':
            return self._read_U

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

        if ch == '#':
            return self._read_preprocessor_directive

        return self._read_operator

    def _read_l(self) -> None:
        """
        Read an L character, which could be the start of a wide string/character literal
        (L"..." or L'...') or an identifier.
        """
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] in ('"', "'")):
            self._read_string()
            return

        self._read_identifier_or_keyword()

    def _read_u(self) -> None:
        """
        Read a u character, which could be the start of a UTF-16 string/character literal
        (u"..." or u'...'), a UTF-8 string literal (u8"..."), or an identifier.
        """
        if self._position + 1 < self._input_len:
            next_char = self._input[self._position + 1]

            # Check for u8"..." or u8'...'
            if next_char == '8' and self._position + 2 < self._input_len:
                third_char = self._input[self._position + 2]
                if third_char in ('"', "'"):
                    self._read_string()
                    return

            # Check for u"..." or u'...'
            if next_char in ('"', "'"):
                self._read_string()
                return

        self._read_identifier_or_keyword()

    def _read_U(self) -> None:  # pylint: disable=invalid-name
        """
        Read a U character, which could be the start of a UTF-32 string/character literal
        (U"..." or U'...') or an identifier.
        """
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] in ('"', "'")):
            self._read_string()
            return

        self._read_identifier_or_keyword()

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
        Read a forward slash, which could be the start of a comment or an operator.
        """
        if self._position + 1 < self._input_len:
            if self._input[self._position + 1] == '/':
                self._read_comment()
                return

            if self._input[self._position + 1] == '*':
                self._read_block_comment(2)
                return

        self._read_operator()

    def _read_string(self) -> None:
        """
        Read a string or character literal token, including prefixed literals.

        Handles C11 prefixes: L, u, U, u8 for both strings and character literals.

        Handles escape sequences within strings.
        """
        start = self._position

        # Skip over any prefix (L, u, U, u8)
        if self._position < self._input_len:
            ch = self._input[self._position]
            if ch == 'L':
                self._position += 1

            elif ch == 'U':
                self._position += 1

            elif ch == 'u':
                self._position += 1
                # Check for u8 prefix
                if (self._position < self._input_len and
                        self._input[self._position] == '8'):
                    self._position += 1

        # Now we should be at the quote character
        if self._position >= self._input_len:
            return

        quote = self._input[self._position]
        if quote not in ('"', "'"):
            # This shouldn't happen if called correctly, but handle it
            self._read_identifier_or_keyword()
            return

        self._position += 1  # Skip opening quote

        while self._position < self._input_len and self._input[self._position] != quote:
            if (self._input[self._position] == '\\' and
                    self._position + 1 < self._input_len):
                self._position += 2
                continue

            self._position += 1

        if self._position < self._input_len and self._input[self._position] == quote:
            self._position += 1  # Skip closing quote

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles decimal, hexadecimal, binary numbers, and their suffixes.
        """
        start = self._position
        has_suffix = False

        if (self._position + 1 < self._input_len and
                self._input[self._position] == '0'):
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

            else:  # Decimal or floating-point
                self._read_decimal_number()

        else:
            self._read_decimal_number()

        # Handle suffixes
        suffix_start = self._position
        while (self._position < self._input_len and
               self._input[self._position].lower() in 'ulfj'):
            self._position += 1
            has_suffix = True

        # Validate suffix combination
        if has_suffix:
            suffix = self._input[suffix_start:self._position].lower()
            valid_integer_suffixes = {'u', 'ul', 'ull', 'lu', 'llu'}
            valid_float_suffixes = {'f', 'l'}
            if suffix not in valid_integer_suffixes and suffix not in valid_float_suffixes:
                self._position = suffix_start

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
            if self._input[self._position] == '*' and self._input[self._position + 1] == '/':
                self._in_block_comment = False
                self._position += 2
                break

            self._position += 1

        # If we're still in a block comment we've got one character left on this line and
        # we need to include it in the comment too.
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

    def _read_preprocessor_directive(self) -> None:
        """
        Read a preprocessor directive token.
        """
        self._tokens.append(Token(
            type=TokenType.PREPROCESSOR,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a C keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a C keyword, False otherwise
        """
        keywords = {
            'auto', 'break', 'case', 'char', 'const', 'continue',
            'default', 'do', 'double', 'else', 'enum', 'extern',
            'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
            'register', 'restrict', 'return', 'short', 'signed',
            'sizeof', 'static', 'struct', 'switch', 'typedef',
            'union', 'unsigned', 'void', 'volatile', 'while',
            '_Bool', '_Complex', '_Imaginary'
        }
        return value in keywords
