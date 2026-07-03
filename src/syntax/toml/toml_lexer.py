from collections.abc import Callable
from dataclasses import dataclass

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class TOMLLexerState(LexerState):
    """
    State information for the TOML lexer.

    Attributes:
        in_multiline_string: Whether we are currently consuming a multi-line
            triple-quoted string.
        string_quote: The quote character of the active multi-line string
            ('"' for double, "'" for single).
    """
    in_multiline_string: bool = False
    string_quote: str = ""


class TOMLLexer(Lexer):
    """
    Lexer for TOML.

    This lexer handles TOML-specific syntax including table headers, keys,
    strings (single, double, and triple-quoted), numbers, booleans, comments,
    and structural punctuation.
    """

    def __init__(self) -> None:
        super().__init__()
        self._in_multiline_string = False
        self._string_quote = ""

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> TOMLLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state
        """
        self._input = input_str
        self._input_len = len(input_str)

        if prev_lexer_state is not None:
            assert isinstance(prev_lexer_state, TOMLLexerState), \
                f"Expected TOMLLexerState, got {type(prev_lexer_state).__name__}"

            self._in_multiline_string = prev_lexer_state.in_multiline_string
            self._string_quote = prev_lexer_state.string_quote

        if self._in_multiline_string:
            self._continue_multiline_string()

        if not self._in_multiline_string:
            self._inner_lex()

        lexer_state = TOMLLexerState()
        lexer_state.in_multiline_string = self._in_multiline_string
        lexer_state.string_quote = self._string_quote
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

        if ch == '#':
            return self._read_comment

        if ch == '"':
            return self._read_double_quote

        if ch == "'":
            return self._read_single_quote

        if ch == '[':
            return self._read_table_header_or_array

        if ch == '-' or ch == '+' or self._is_digit(ch):
            return self._read_number

        if ch == '.':
            return self._read_dot

        if ch in '={}':
            return self._read_punctuation

        return self._read_key_or_value

    def _read_punctuation(self) -> None:
        """
        Read a single-character punctuation token (=, {, }, or ,).
        """
        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_comment(self) -> None:
        """
        Read a comment token (# to end of line).
        """
        start = self._position
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_double_quote(self) -> None:
        """
        Read a double-quoted string or triple-double-quoted multi-line string.
        """
        if self._input[self._position:].startswith('"""'):
            self._read_multiline_string('"')
            return

        self._read_simple_string('"')

    def _read_single_quote(self) -> None:
        """
        Read a single-quoted string or triple-single-quoted multi-line string.
        """
        if self._input[self._position:].startswith("'''"):
            self._read_multiline_string("'")
            return

        self._read_simple_string("'")

    def _read_simple_string(self, quote: str) -> None:
        """
        Read a single-line quoted string.

        Args:
            quote: The quote character ('"' or "'")
        """
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\' and quote == '"' and (self._position + 1) < self._input_len:
                self._position += 2
                continue

            if ch == quote:
                self._position += 1
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_multiline_string(self, quote: str) -> None:
        """
        Start reading a triple-quoted multi-line string.

        If the closing triple quote is not found on this line, the lexer enters
        multi-line mode and the continuation is handled on subsequent lines.

        Args:
            quote: The quote character ('"' or "'")
        """
        self._in_multiline_string = True
        self._string_quote = quote

        start = self._position
        self._position += 3

        if self._position < self._input_len and self._input[self._position] == '\n':
            self._position += 1

        while self._position + 2 < self._input_len:
            if (self._input[self._position] == quote and
                    self._input[self._position + 1] == quote and
                    self._input[self._position + 2] == quote):
                self._position += 3
                self._in_multiline_string = False
                self._string_quote = ""
                break

            if self._input[self._position] == '\\' and quote == '"' and self._position + 1 < self._input_len:
                self._position += 2
                continue

            self._position += 1

        if self._in_multiline_string and self._position + 2 == self._input_len:
            if (self._input[self._position] == quote and
                    self._input[self._position + 1] == quote):
                self._position = self._input_len

        if self._in_multiline_string:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _continue_multiline_string(self) -> None:
        """
        Continue reading a multi-line string from a previous line.
        """
        quote = self._string_quote
        start = self._position

        while self._position + 2 < self._input_len:
            if (self._input[self._position] == quote and
                    self._input[self._position + 1] == quote and
                    self._input[self._position + 2] == quote):
                self._position += 3
                self._in_multiline_string = False
                self._string_quote = ""
                break

            if self._input[self._position] == '\\' and quote == '"' and self._position + 1 < self._input_len:
                self._position += 2
                continue

            self._position += 1

        if self._in_multiline_string and self._position + 2 == self._input_len:
            if (self._input[self._position] == quote and
                    self._input[self._position + 1] == quote):
                self._position = self._input_len

        if self._in_multiline_string:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_table_header_or_array(self) -> None:
        """
        Read a table header ([section]) or array of tables ([[section]]).

        The entire header is emitted as a single ELEMENT token.
        """
        start = self._position
        self._position += 1

        if self._position < self._input_len and self._input[self._position] == '[':
            self._position += 1

        depth = 1
        while self._position < self._input_len and depth > 0:
            ch = self._input[self._position]

            if ch == ']' and self._position + 1 < self._input_len and self._input[self._position + 1] == ']':
                self._position += 2
                depth -= 1
                if depth == 0:
                    break

                continue

            if ch == ']':
                self._position += 1
                depth -= 1
                if depth == 0:
                    break

                continue

            if ch == '#':
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.ELEMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles integers, floats, negative/positive numbers, hex, octal,
        binary, infinity, NaN, and datetime-like patterns.
        """
        start = self._position

        if self._input[self._position] in ('+', '-'):
            self._position += 1

        if (self._position + 1 < self._input_len and
                self._input[self._position] == '0'):
            next_ch = self._input[self._position + 1].lower()
            if next_ch == 'x':
                self._position += 2
                while (self._position < self._input_len and
                       (self._is_hex_digit(self._input[self._position]) or
                        self._input[self._position] == '_')):
                    self._position += 1

                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            if next_ch == 'o':
                self._position += 2
                while (self._position < self._input_len and
                       (self._input[self._position] in '01234567' or
                        self._input[self._position] == '_')):
                    self._position += 1

                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            if next_ch == 'b':
                self._position += 2
                while (self._position < self._input_len and
                       (self._input[self._position] in '01' or
                        self._input[self._position] == '_')):
                    self._position += 1

                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        lower_remaining = self._input[self._position:].lower()
        if lower_remaining.startswith('inf') or lower_remaining.startswith('nan'):
            self._position += 3
            self._tokens.append(Token(
                type=TokenType.NUMBER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        self._read_decimal_or_datetime(start)

    def _read_decimal_or_datetime(self, start: int) -> None:
        """
        Read a decimal number, float, or datetime-like value.

        Args:
            start: The starting position
        """
        while (self._position < self._input_len and
               (self._is_digit(self._input[self._position]) or
                self._input[self._position] == '_')):
            self._position += 1

        if (self._position < self._input_len and
                self._input[self._position] == ':'):
            self._read_time(start)
            return

        if (self._position < self._input_len and
                self._input[self._position] == '-'):
            if self._position + 3 < self._input_len:
                check = self._position + 1
                if (check + 1 < self._input_len and
                        self._is_digit(self._input[check]) and
                        self._is_digit(self._input[check + 1])):
                    self._read_date_or_datetime(start)
                    return

        if (self._position < self._input_len and
                self._input[self._position] == '.'):
            self._position += 1
            while (self._position < self._input_len and
                   (self._is_digit(self._input[self._position]) or
                    self._input[self._position] == '_')):
                self._position += 1

        if (self._position < self._input_len and
                self._input[self._position].lower() == 'e'):
            self._position += 1
            if (self._position < self._input_len and
                    self._input[self._position] in ('+', '-')):
                self._position += 1

            while (self._position < self._input_len and
                   (self._is_digit(self._input[self._position]) or
                    self._input[self._position] == '_')):
                self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_time(self, start: int) -> None:
        """
        Read a time value (HH:MM:SS).

        Args:
            start: The starting position
        """
        while self._position < self._input_len:
            ch = self._input[self._position]
            if self._is_digit(ch) or ch in ':.':
                self._position += 1

            elif ch in ('Z', 'z', '+', '-'):
                self._position += 1

            else:
                break

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_date_or_datetime(self, start: int) -> None:
        """
        Read a date or datetime value (YYYY-MM-DD or YYYY-MM-DDTHH:MM:SSZ).

        Args:
            start: The starting position
        """
        while self._position < self._input_len:
            ch = self._input[self._position]
            if self._is_digit(ch) or ch in '-:.':
                self._position += 1

            elif ch in ('T', 't', 'Z', 'z', ' ', '+'):
                self._position += 1

            else:
                break

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_dot(self) -> None:
        """
        Read a dot operator (part of a dotted key) or a number starting with dot.
        """
        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            start = self._position
            self._position += 1
            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

            if (self._position < self._input_len and
                    self._input[self._position].lower() == 'e'):
                self._position += 1
                if (self._position < self._input_len and
                        self._input[self._position] in ('+', '-')):
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

        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_key_or_value(self) -> None:
        """
        Read a bare key, boolean keyword, or plain text value.
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] == '-')):
            self._position += 1

        value = self._input[start:self._position]

        lower = value.lower()
        if lower in ('inf', 'nan'):
            self._tokens.append(Token(
                type=TokenType.NUMBER,
                value=value,
                start=start
            ))
            return

        if value in ('true', 'false'):
            self._tokens.append(Token(
                type=TokenType.BOOLEAN,
                value=value,
                start=start
            ))
            return

        self._tokens.append(Token(
            type=TokenType.TEXT,
            value=value,
            start=start
        ))
