from dataclasses import dataclass
from typing import Callable, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType
from humbug.syntax.programming_language import ProgrammingLanguage

# Add Go to the ProgrammingLanguage enum if not already present
if not hasattr(ProgrammingLanguage, 'GO'):
    ProgrammingLanguage.GO = 14  # Assuming the last enum value was 13


@dataclass
class GoLexerState(LexerState):
    """
    State information for the Go lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        in_raw_string: Indicates if we're currently parsing a raw string literal
    """
    in_block_comment: bool = False
    in_raw_string: bool = False


class GoLexer(Lexer):
    """
    Lexer for Go code.

    This lexer handles Go-specific syntax including:
    - Keywords and operators
    - Regular and raw string literals
    - Rune literals
    - Numeric literals with various bases and imaginary numbers
    - Comments (line and block)
    - Special identifiers like the blank identifier (_)
    """

    def __init__(self):
        super().__init__()
        self._in_block_comment = False
        self._in_raw_string = False

    def lex(self, prev_lexer_state: Optional[GoLexerState], input_str: str) -> GoLexerState:
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
        if prev_lexer_state:
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._in_raw_string = prev_lexer_state.in_raw_string

        if self._in_block_comment:
            self._read_block_comment(0)

        if not self._in_block_comment:
            self._inner_lex()

        lexer_state = GoLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.in_raw_string = self._in_raw_string
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

        if ch == '`':
            return self._read_raw_string

        if ch in ('"', "'"):
            return self._read_string

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch == '/':
            return self._read_forward_slash

        if ch == '.':
            return self._read_dot

        if ch == '<':
            return self._read_less_than

        return self._read_operator

    def _read_raw_string(self) -> None:
        """
        Read a raw string literal token (delimited by backticks).
        """
        self._in_raw_string = True
        start = self._position
        self._position += 1  # Skip opening backtick

        while self._position < self._input_len:
            if self._input[self._position] == '`':
                self._in_raw_string = False
                self._position += 1
                break
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_string(self) -> None:
        """
        Read a string or rune literal token.
        """
        start = self._position
        quote = self._input[self._position]
        self._position += 1

        while self._position < self._input_len and self._input[self._position] != quote:
            if self._input[self._position] == '\\':
                self._position += 2  # Skip escape sequence
                continue
            self._position += 1

        if self._position < self._input_len:
            self._position += 1  # Skip closing quote

        value = self._input[start:self._position]
        token_type = TokenType.RUNE if quote == "'" else TokenType.STRING
        self._tokens.append(Token(type=token_type, value=value, start=start))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles:
        - Decimal integers and floats
        - Hexadecimal integers (0x prefix)
        - Octal integers (0o prefix)
        - Binary integers (0b prefix)
        - Imaginary numbers (i suffix)
        """
        start = self._position

        if self._input[self._position] == '0' and self._position + 1 < self._input_len:
            next_char = self._input[self._position + 1].lower()
            if next_char == 'x':  # Hexadecimal
                self._position += 2
                while (self._position < self._input_len and
                       self._is_hex_digit(self._input[self._position])):
                    self._position += 1
            elif next_char == 'o':  # Octal
                self._position += 2
                while (self._position < self._input_len and
                       self._is_octal_digit(self._input[self._position])):
                    self._position += 1
            elif next_char == 'b':  # Binary
                self._position += 2
                while (self._position < self._input_len and
                       self._is_binary_digit(self._input[self._position])):
                    self._position += 1
            else:
                self._read_decimal_number()
        else:
            self._read_decimal_number()

        # Check for imaginary number suffix
        if (self._position < self._input_len and
                self._input[self._position].lower() == 'i'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_decimal_number(self) -> None:
        """
        Read a decimal integer or floating-point number.
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

    def _read_dot(self) -> None:
        """
        Read a dot operator or decimal point in a number.

        Handles:
        - Floating point numbers starting with dot (.123)
        - Package member access (pkg.member)
        - Variadic expressions (...) used in function parameters

        In Go, a dot can be:
        1. Start of a floating point number: .123
        2. Package member access: fmt.Println
        3. Part of variadic expression: func(args...)
        """
        start = self._position

        # Check if it's the start of a number
        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            self._position += 1  # Move past the dot
            while (self._position < self._input_len and
                self._is_digit(self._input[self._position])):
                self._position += 1

            # Handle scientific notation if present
            if (self._position < self._input_len and
                    self._input[self._position].lower() == 'e'):
                self._position += 1
                if self._position < self._input_len and self._input[self._position] in ('+', '-'):
                    self._position += 1
                while (self._position < self._input_len and
                    self._is_digit(self._input[self._position])):
                    self._position += 1

            self._tokens.append(Token(
                type=TokenType.NUMBER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # It's a regular dot operator (for package member access)
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value='.',
            start=start
        ))

    def _read_less_than(self) -> None:
        """
        Read a less than operator, which could be part of a channel operation.
        """
        start = self._position
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] == '-'):  # Channel operation
            self._position += 2
            self._tokens.append(Token(
                type=TokenType.OPERATOR,
                value='<-',
                start=start
            ))
            return

        self._read_operator()

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash, which could be a comment or operator.
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

    def _read_operator(self) -> None:
        """
        Read an operator or punctuation token.
        """
        operators = [
            '<<=', '>>=', '...', '&^=', ':=', '<-', '++', '--', '&&', '||', '<=',
            '>=', '==', '!=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=',
            '<<', '>>', '&^', '+', '-', '*', '/', '%', '&', '|', '^', '<', '>',
            '!', '=', ':', '.', ',', ';', '(', ')', '{', '}', '[', ']', '~'
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
        Check if a given value is a Go keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Go keyword, False otherwise
        """
        keywords = {
            'break', 'case', 'chan', 'const', 'continue', 'default', 'defer',
            'else', 'fallthrough', 'for', 'func', 'go', 'goto', 'if', 'import',
            'interface', 'map', 'package', 'range', 'return', 'select',
            'struct', 'switch', 'type', 'var'
        }
        return value in keywords
