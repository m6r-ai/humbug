from dataclasses import dataclass
from typing import Callable, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class RustLexerState(LexerState):
    """
    State information for the Rust lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        block_comment_depth: Tracks nested block comment depth
    """
    in_block_comment: bool = False
    block_comment_depth: int = 0


class RustLexer(Lexer):
    """
    Lexer for Rust code.

    This lexer handles Rust-specific syntax including:
    - Raw string literals (r#"..."#)
    - Raw identifiers (r#ident#)
    - Generic type parameters
    - Standard language tokens (keywords, operators, etc.)
    """

    # Operators list
    _OPERATORS = [
        '<<=', '>>=', '...', '..=',
        '&&', '||', '<<', '>>', '+=', '-=', '*=', '/=',
        '%=', '^=', '&=', '|=', '==', '!=', '>=', '<=',
        '..', '::', '->', '=>',
        '+', '-', '*', '/', '%', '^', '!', '&', '|',
        '=', '>', '<', '@', '_', '.', ',', ';', ':',
        '#', '$', '?',
        '(', ')', '[', ']', '{', '}'
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self):
        super().__init__()
        self._in_block_comment = False
        self._block_comment_depth = 0

    def lex(self, prev_lexer_state: Optional[RustLexerState], input_str: str) -> RustLexerState:
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
            self._block_comment_depth = prev_lexer_state.block_comment_depth

        if self._in_block_comment:
            self._read_block_comment(0)

        if not self._in_block_comment:
            self._inner_lex()

        lexer_state = RustLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.block_comment_depth = self._block_comment_depth
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

        if ch == 'r':
            return self._read_raw_token

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch == '"':
            return self._read_string

        if ch == '\'':
            return self._read_quote

        if ch == '/':
            return self._read_forward_slash

        if ch == '<':
            return self._read_angle_bracket

        return self._read_operator

    def _read_raw_token(self) -> None:
        """
        Read a raw token, which could be:
        - A raw string literal (r#"..."#)
        - A raw identifier (r#ident#)
        - A regular identifier starting with 'r'
        """
        start = self._position
        hash_count = 0

        # Check if this is a raw token
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] == '#'):
            self._position += 2
            hash_count = 1

            # Count additional # symbols
            while (self._position < self._input_len and
                   self._input[self._position] == '#'):
                hash_count += 1
                self._position += 1

            if self._position < self._input_len:
                if self._input[self._position] == '"':
                    self._read_raw_string(hash_count, start)
                    return

                # Raw identifier
                self._read_raw_identifier(hash_count, start)
                return

        # Not a raw token, handle as regular identifier
        self._position = start
        self._read_identifier_or_keyword()

    def _read_raw_string(self, hash_count: int, start: int) -> None:
        """
        Read a raw string literal token.
        """
        self._position += 1  # Skip the quote
        end_sequence = '"' + '#' * hash_count

        while self._position < self._input_len:
            if (self._position + len(end_sequence) <= self._input_len and
                    self._input[self._position:self._position + len(end_sequence)] == end_sequence):
                self._position += len(end_sequence)
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_raw_identifier(self, hash_count: int, start: int) -> None:
        """
        Read a raw identifier token.
        """
        while self._position < self._input_len:
            ch = self._input[self._position]
            if (not self._is_letter_or_digit_or_underscore(ch) and
                    ch != '#'):
                break

            self._position += 1

        # Ensure proper closing with matching number of #
        closing_hashes = 0
        while (self._position > 0 and
               self._position - 1 < self._input_len and
               self._input[self._position - 1] == '#'):
            closing_hashes += 1
            self._position -= 1

        if closing_hashes != hash_count:
            # Invalid raw identifier, treat as error
            self._tokens.append(Token(
                type=TokenType.ERROR,
                value=self._input[start:self._position + closing_hashes],
                start=start
            ))
            return

        self._position += closing_hashes
        value = self._input[start:self._position]

        # Check if the raw identifier is actually a keyword
        inner_value = value[2:-hash_count]  # Strip r# and trailing #
        if self._is_keyword(inner_value):
            self._tokens.append(Token(type=TokenType.KEYWORD, value=value, start=start))
            return

        self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))

    def _read_angle_bracket(self) -> None:
        """
        Read an angle bracket, which could be:
        - A generic type parameter
        - A comparison operator
        - A bit shift operator
        """
        start = self._position
        self._position += 1

        # Check for <= or <<
        if self._position < self._input_len:
            ch = self._input[self._position]
            if ch == '=':
                self._position += 1
                self._tokens.append(Token(
                    type=TokenType.OPERATOR,
                    value='<=',
                    start=start
                ))
                return
            if ch == '<':
                self._position += 1
                # Check for <<=
                if (self._position < self._input_len and
                        self._input[self._position] == '='):
                    self._position += 1
                    self._tokens.append(Token(
                        type=TokenType.OPERATOR,
                        value='<<=',
                        start=start
                    ))
                    return
                self._tokens.append(Token(
                    type=TokenType.OPERATOR,
                    value='<<',
                    start=start
                ))
                return

        # Single < for generic parameter or comparison
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value='<',
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
        Check if a given value is a Rust keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Rust keyword, False otherwise
        """
        keywords = {
            # Reserved keywords
            'as', 'async', 'await', 'break', 'const', 'continue', 'crate', 'dyn',
            'else', 'enum', 'extern', 'false', 'fn', 'for', 'if', 'impl', 'in', 'let',
            'loop', 'match', 'mod', 'move', 'mut', 'pub', 'ref', 'return',
            'self', 'Self', 'static', 'struct', 'super', 'trait', 'true',
            'type', 'unsafe', 'use', 'where', 'while',

            # Reserved for future use
            'abstract', 'become', 'box', 'do', 'final', 'macro',
            'override', 'priv', 'try', 'typeof', 'unsized', 'virtual',
            'yield'
        }
        return value in keywords

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles:
        - Decimal integers (123, 123_456)
        - Hex integers (0xff, 0xFF_FF)
        - Octal integers (0o77, 0o77_77)
        - Binary integers (0b1111, 0b1111_0000)
        - Floating point (123.456, 1.23e-4, 1_234.567_89)
        - Type suffixes (u8, i32, f64, etc.)
        """
        start = self._position

        # Check for hex, octal, or binary prefix
        if (self._input[self._position] == '0' and
                self._position + 1 < self._input_len):
            prefix = self._input[self._position + 1].lower()
            if prefix == 'x':  # Hexadecimal
                self._position += 2
                self._read_digits(self._is_hex_digit, '_')
                self._read_type_suffix()
                self._add_number_token(start)
                return
            elif prefix == 'o':  # Octal
                self._position += 2
                self._read_digits(self._is_octal_digit, '_')
                self._read_type_suffix()
                self._add_number_token(start)
                return
            elif prefix == 'b':  # Binary
                self._position += 2
                self._read_digits(self._is_binary_digit, '_')
                self._read_type_suffix()
                self._add_number_token(start)
                return

        # Decimal integer or float
        self._read_digits(self._is_digit, '_')

        # Check for decimal point
        if (self._position < self._input_len and
                self._input[self._position] == '.'):
            next_pos = self._position + 1
            # Ensure it's not the range operator (..)
            if (next_pos < self._input_len and
                    self._input[next_pos] != '.'):
                self._position += 1
                self._read_digits(self._is_digit, '_')

        # Check for exponent
        if self._position < self._input_len:
            exp = self._input[self._position].lower()
            if exp == 'e':
                self._position += 1
                if (self._position < self._input_len and
                        self._input[self._position] in '+-'):
                    self._position += 1
                self._read_digits(self._is_digit, '_')

        # Read type suffix
        self._read_type_suffix()
        self._add_number_token(start)

    def _read_digits(self, is_valid_digit: callable, separator: str = None) -> None:
        """
        Read a sequence of digits with optional separator.

        Args:
            is_valid_digit: Function to check if a character is a valid digit
            separator: Optional separator character allowed between digits
        """
        has_digit = False
        while self._position < self._input_len:
            ch = self._input[self._position]
            if is_valid_digit(ch):
                has_digit = True
                self._position += 1
            elif separator and ch == separator and has_digit:
                # Ensure we've seen at least one digit before a separator
                self._position += 1
            else:
                break

    def _read_type_suffix(self) -> None:
        """
        Read a numeric type suffix.
        Handles integer suffixes (u8, i32, etc.) and float suffixes (f32, f64).
        """
        if self._position >= self._input_len:
            return

        # Check for float suffix
        if self._input[self._position].lower() == 'f':
            suffix_start = self._position
            self._position += 1
            if (self._position < self._input_len and
                    self._is_digit(self._input[self._position])):
                self._position += 1
                if (self._position < self._input_len and
                        self._is_digit(self._input[self._position])):
                    self._position += 1
            else:
                # Invalid float suffix, reset position
                self._position = suffix_start
            return

        # Check for integer suffix
        suffix_chars = {'i', 'u'}
        if self._input[self._position].lower() not in suffix_chars:
            return

        suffix_start = self._position
        self._position += 1

        # Read optional size (8, 16, 32, 64, 128, size)
        if self._position < self._input_len:
            if self._input[self._position:].startswith('size'):
                self._position += 4
            elif (self._position + 1 < self._input_len and
                    self._is_digit(self._input[self._position]) and
                    self._is_digit(self._input[self._position + 1])):
                self._position += 2
                if (self._position < self._input_len and
                        self._is_digit(self._input[self._position])):
                    self._position += 1
            else:
                # Invalid suffix, reset position
                self._position = suffix_start

    def _is_octal_digit(self, ch: str) -> bool:
        """
        Determines if a character is an octal digit.

        Args:
            ch: The character to check
        Returns:
            True if the character is an octal digit, False otherwise
        """
        return '0' <= ch <= '7'

    def _add_number_token(self, start: int) -> None:
        """
        Add a number token to the token list.

        Args:
            start: Starting position of the number in the input
        """
        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_quote(self) -> None:
        """
        Read a character literal or lifetime token.

        Handles:
        - Character literals
        - Lifetime annotations
        """
        start = self._position
        self._position += 1  # Skip the quote

        # Check if this is potentially a lifetime
        if (self._position < self._input_len and
                (self._is_letter(self._input[self._position]) or
                self._input[self._position] == '_')):

            self._tokens.append(Token(
                type=TokenType.LIFETIME,
                value=self._input[start:self._position],
                start=start
            ))

            self._read_identifier_or_keyword()
            return

        # Character literal
        if self._position >= self._input_len:
            self._tokens.append(Token(type=TokenType.ERROR, value="'", start=start))
            return

        if self._input[self._position] == '\\':
            self._position += 1
            if self._position >= self._input_len:
                self._tokens.append(Token(type=TokenType.ERROR, value=self._input[start:self._position], start=start))
                return

            # Handle escape sequences
            ch = self._input[self._position]
            if ch in ('n', 'r', 't', '\\', '0', '\'', '"'):
                self._position += 1
            elif ch == 'x':  # \xHH
                self._position += 1
                for _ in range(2):
                    if (self._position >= self._input_len or
                            not self._is_hex_digit(self._input[self._position])):
                        self._tokens.append(Token(type=TokenType.ERROR, value=self._input[start:self._position], start=start))
                        return

                    self._position += 1
            elif ch == 'u':  # \u{HHHHHH}
                self._position += 1
                if (self._position >= self._input_len or
                        self._input[self._position] != '{'):
                    self._tokens.append(Token(type=TokenType.ERROR, value=self._input[start:self._position], start=start))
                    return

                self._position += 1

                # Read 1-6 hex digits
                hex_digits = 0
                while (hex_digits < 6 and
                    self._position < self._input_len and
                    self._is_hex_digit(self._input[self._position])):
                    hex_digits += 1
                    self._position += 1

                if (self._position >= self._input_len or
                        self._input[self._position] != '}' or
                        hex_digits == 0):
                    self._tokens.append(Token(type=TokenType.ERROR, value=self._input[start:self._position], start=start))
                    return
                self._position += 1
            else:
                self._tokens.append(Token(type=TokenType.ERROR, value=self._input[start:self._position], start=start))
                return
        else:
            # Single character
            self._position += 1

        # Expect closing quote
        if (self._position >= self._input_len or
                self._input[self._position] != '\''):
            self._tokens.append(Token(type=TokenType.ERROR, value=self._input[start:self._position], start=start))
            return

        self._position += 1
        self._tokens.append(Token(
            type=TokenType.CHARACTER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be:
        - A line comment (//)
        - A block comment (/* ... */)
        - A division operator (/)
        - A division-assignment operator (/=)
        """
        if self._position + 1 < self._input_len:
            next_char = self._input[self._position + 1]
            if next_char == '/':
                self._read_line_comment()
                return
            if next_char == '*':
                self._read_block_comment(2)
                return
            if next_char == '=':
                start = self._position
                self._position += 2
                self._tokens.append(Token(
                    type=TokenType.OPERATOR,
                    value='/=',
                    start=start
                ))
                return

        # Single division operator
        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value='/',
            start=start
        ))

    def _read_line_comment(self) -> None:
        """
        Read a line comment token (//).
        Handles both regular comments (//) and doc comments (///).
        """
        start = self._position
        self._position += 2  # Skip //

        # Check for doc comment
        is_doc = False
        if self._position < self._input_len and self._input[self._position] == '/':
            is_doc = True
            self._position += 1

        while self._position < self._input_len and self._input[self._position] != '\n':
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.DOC_COMMENT if is_doc else TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_block_comment(self, skip_chars: int) -> None:
        """
        Read a block comment token.
        Handles nested block comments and doc block comments.

        Args:
            skip_chars: Number of characters to skip at start
        """
        self._in_block_comment = True
        self._block_comment_depth = 1
        start = self._position
        self._position += skip_chars  # Skip /*

        # Check for doc comment
        is_doc = False
        if self._position < self._input_len and self._input[self._position] == '*':
            is_doc = True
            self._position += 1

        while self._position + 1 < self._input_len:
            if (self._input[self._position] == '/' and
                    self._input[self._position + 1] == '*'):
                self._block_comment_depth += 1
                self._position += 2
                continue

            if (self._input[self._position] == '*' and
                    self._input[self._position + 1] == '/'):
                self._block_comment_depth -= 1
                if self._block_comment_depth == 0:
                    self._in_block_comment = False
                    self._position += 2
                    break
                self._position += 2
                continue

            self._position += 1

        if self._in_block_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.DOC_COMMENT if is_doc else TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))
