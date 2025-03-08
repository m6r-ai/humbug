from dataclasses import dataclass
from typing import Optional, Callable

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


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

    def __init__(self):
        super().__init__()
        self._in_block_comment = False

    def lex(self, prev_lexer_state: Optional[CLexerState], input_str: str) -> CLexerState:
        """
        Lex all the tokens in the input.
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state:
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
        if ch == '\n':
            return self._read_newline

        if self._is_whitespace(ch):
            return self._read_whitespace

        if ch == 'L':
            return self._read_l

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
        Read an L character, which could be the start of a wide string literal
        or an identifier.
        """
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] == '"'):
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
        Read a string literal token, including wide string literals.

        Handles escape sequences within strings.
        """
        start = self._position
        quote = self._input[self._position]
        self._position += 1

        if quote == 'L':
            self._position += 1
            quote = '"'

        while self._position < self._input_len and self._input[self._position] != quote:
            if (self._input[self._position] == '\\' and
                    self._position + 1 < self._input_len):
                self._position += 2
                continue

            self._position += 1

        if self._position < self._input_len:
            self._position += 1

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
        start = self._position
        self._position += 2  # Skip //
        while (self._position < self._input_len and
               self._input[self._position] != '\n'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

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
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               self._input[self._position] != '\n'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.PREPROCESSOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_operator(self) -> None:
        """
        Read an operator or punctuation token.
        """
        operators = [
            '>>=', '<<=', '&&=', '||=', '!=', '==', '+=', '-=', '*=',
            '/=', '%=', '&=', '|=', '^=', '<=', '>=', '&&', '||', '<<',
            '>>', '++', '--', '->', '+', '-', '*', '/', '%', '&', '~',
            '!', '|', '^', '=', '<', '>', '(', ')', '{', '}', '[', ']',
            ';', ':', '?', '.', ','
        ]

        for operator in operators:
            if self._input[self._position:].startswith(operator):
                start = self._position
                self._position += len(operator)
                self._tokens.append(Token(
                    type=TokenType.OPERATOR,
                    value=operator,
                    start=start
                ))
                return

        start = self._position
        ch = self._input[self._position]
        self._position += 1
        self._tokens.append(Token(type=TokenType.ERROR, value=ch, start=start))

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
