from dataclasses import dataclass
from typing import Callable

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class JavaScriptLexerState(LexerState):
    """
    State information for the JavaScript lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        in_template_literal: Indicates if we're currently parsing a template literal
    """
    in_block_comment: bool = False
    in_template_literal: bool = False


class JavaScriptLexer(Lexer):
    """
    Lexer for JavaScript code.

    This lexer handles JavaScript-specific syntax including keywords, operators,
    strings, regular expressions, comments, and numeric literals.
    """

    # Operators list
    _OPERATORS = [
        '>>>=', '>>=', '<<=', '&&=', '||=', '??=', '**=',
        '!==', '===', '>>>', '...', '!=', '==', '+=', '-=',
        '*=', '/=', '%=', '&=', '|=', '^=', '<=', '>=', '&&',
        '||', '??', '?.', '<<', '>>', '**', '++', '--', '+',
        '-', '*', '/', '%', '&', '~', '!', '|', '^', '=', '<',
        '>', '(', ')', '{', '}', '[', ']', ';', ':', '?', '.',
        ','
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self) -> None:
        super().__init__()
        self._in_block_comment = False
        self._in_template_literal = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> JavaScriptLexerState:
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
            assert isinstance(prev_lexer_state, JavaScriptLexerState), \
                f"Expected JavaScriptLexerState, got {type(prev_lexer_state).__name__}"
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._in_template_literal = prev_lexer_state.in_template_literal

        if self._in_block_comment:
            self._read_block_comment(0)
        elif self._in_template_literal:
            self._read_template_literal_continuation()

        if not self._in_block_comment and not self._in_template_literal:
            self._inner_lex()

        lexer_state = JavaScriptLexerState(
            in_block_comment=False,
            in_template_literal=False
        )
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.in_template_literal = self._in_template_literal
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

        if self._is_letter(ch) or ch == '_' or ch == '$':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch in ('"', "'", '`'):
            return self._read_string

        if ch == '.':
            return self._read_dot

        if ch == '/':
            return self._read_forward_slash

        if ch == '#':
            return self._read_hash

        return self._read_operator

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be a comment, block comment,
        or regular expression.
        """
        if self._position + 1 < self._input_len:
            # Check for /= operator first
            if self._input[self._position + 1] == '=':
                self._read_operator()
                return

            if self._input[self._position + 1] == '/':
                self._read_comment()
                return

            if self._input[self._position + 1] == '*':
                self._read_block_comment(2)
                return

        self._read_regexp_or_divide()

    def _read_dot(self) -> None:
        """
        Read a dot operator or decimal point in a number.
        """
        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        self._read_operator()

    def _read_hash(self) -> None:
        """
        Read a hash token, which could be a hashbang or an error.
        Hashbang is only valid at the very start of the file (position 0).
        """
        if (self._position == 0 and
                self._position + 1 < self._input_len and
                self._input[self._position + 1] == '!'):
            self._read_hashbang()
            return

        start = self._position
        self._position += 1
        self._tokens.append(Token(type=TokenType.ERROR, value='#', start=start))

    def _read_hashbang(self) -> None:
        """
        Read a hashbang token.
        """
        self._tokens.append(Token(
            type=TokenType.PREPROCESSOR,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _read_number(self) -> None:
        """
        Read a numeric literal.

        Handles decimal, hexadecimal, binary, octal, and BigInt literals.
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

        # Check for BigInt 'n' suffix
        if (self._position < self._input_len and
                self._input[self._position] == 'n'):
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
            if (self._position < self._input_len and
                    self._input[self._position] in ('+', '-')):
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
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] == '$')):
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


    def _read_regexp_or_divide(self) -> None:
        """
        Read a regular expression literal or divide operator.
        """
        start = self._position
        self._position += 1  # Skip initial /

        # Try to find the closing / for a regex
        index = self._position
        escaped = False
        found_closing_slash = False

        while index < self._input_len:
            ch = self._input[index]

            if ch == '\\':
                escaped = not escaped
                index += 1
                continue

            if escaped:
                escaped = False
                index += 1
                continue

            if ch == '/':
                found_closing_slash = True
                index += 1
                break

            index += 1

        # If we didn't find a closing slash, it's a division operator
        if not found_closing_slash:
            self._tokens.append(Token(type=TokenType.OPERATOR, value='/', start=start))
            return

        # Check if the next characters seem to be valid regexp flags
        while index < self._input_len and self._input[index] in 'dgimsuy':
            index += 1

        regexp = self._input[start:index]
        self._position = index
        self._tokens.append(Token(type=TokenType.REGEXP, value=regexp, start=start))

    def _read_string(self) -> None:
        """
        Reads a string token, with special handling for template literals.
        """
        quote = self._input[self._position]

        # Handle template literals (backticks) specially
        if quote == '`':
            self._read_template_literal()
            return

        # Regular string handling (single or double quotes)
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

    def _read_template_literal(self) -> None:
        """
        Read a template literal (backtick string), which can span multiple lines.
        """
        start = self._position
        self._position += 1  # Skip opening backtick
        self._in_template_literal = True

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\' and (self._position + 1) < self._input_len:
                self._position += 2  # Skip escape sequence
                continue

            if ch == '`':
                self._position += 1  # Include closing backtick
                self._in_template_literal = False
                break

            self._position += 1

        # Emit the token for this line
        string_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.STRING, value=string_value, start=start))

    def _read_template_literal_continuation(self) -> None:
        """
        Continue reading a template literal from a previous line.
        """
        start = 0
        # Read from start of line, looking for closing backtick
        self._read_template_literal_content(start)

        # After reading, continue with normal lexing if template is closed
        if not self._in_template_literal:
            self._inner_lex()

    def _read_template_literal_content(self, start: int) -> None:
        """
        Read template literal content (for continuation lines).
        """
        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\' and (self._position + 1) < self._input_len:
                self._position += 2
                continue

            if ch == '`':
                self._position += 1
                self._in_template_literal = False
                break

            self._position += 1

        # Emit token for this line's content
        string_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.STRING, value=string_value, start=start))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a JavaScript keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a JavaScript keyword, False otherwise
        """
        keywords = {
            'abstract', 'async', 'await', 'boolean', 'break', 'byte',
            'case', 'catch', 'char', 'class', 'const', 'continue',
            'debugger', 'default', 'delete', 'do', 'double', 'else',
            'enum', 'export', 'extends', 'false', 'final', 'finally',
            'float', 'for', 'from', 'function', 'goto', 'if',
            'implements', 'import', 'in', 'instanceof', 'int',
            'interface', 'let', 'long', 'native', 'new', 'null',
            'of', 'package', 'private', 'protected', 'public',
            'return', 'short', 'static', 'super', 'switch',
            'synchronized', 'this', 'throw', 'throws', 'transient',
            'true', 'try', 'typeof', 'var', 'void', 'volatile',
            'while', 'with', 'yield'
        }
        return value in keywords
