from dataclasses import dataclass
from typing import Callable, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token


@dataclass
class SwiftLexerState(LexerState):
    """
    State information for the Swift lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
    """
    in_block_comment: bool = False


class SwiftLexer(Lexer):
    """
    Lexer for Swift code.

    This lexer handles Swift-specific syntax including:
    - Keywords and context-sensitive keywords
    - Numbers with underscores and type inference
    - Operators (including custom operators)
    - Attributes and property wrappers
    """

    def __init__(self):
        super().__init__()
        self._in_block_comment = False

    def lex(self, prev_lexer_state: Optional[SwiftLexerState], input_str: str) -> SwiftLexerState:
        """
        Lex all the tokens in the input.
        """
        self._input = input_str
        if prev_lexer_state:
            self._in_block_comment = prev_lexer_state.in_block_comment

        if self._in_block_comment:
            self._read_block_comment(0)

        if not self._in_block_comment:
            self._inner_lex()

        lexer_state = SwiftLexerState()
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

        if ch == '@':
            return self._read_attribute

        if ch == '#':
            return self._read_directive_or_string

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch == '"':
            return self._read_string

        if ch == '/':
            return self._read_forward_slash

        if ch == '.':
            return self._read_dot

        return self._read_operator

    def _read_attribute(self) -> None:
        """
        Read an attribute token (e.g., @available, @propertyWrapper).
        """
        start = self._position
        self._position += 1  # Skip @

        # Read the attribute name
        while (self._position < len(self._input) and
               (self._is_letter_or_digit(self._input[self._position]) or
                self._input[self._position] == '_')):
            self._position += 1

        self._tokens.append(Token(
            type='ATTRIBUTE',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_directive_or_string(self) -> None:
        """
        Read a compiler directive or string delimiter token.
        """
        start = self._position
        self._position += 1  # Skip #

        # Check for string delimiter
        if self._position < len(self._input) and self._input[self._position] == '"':
            self._read_raw_string(start)
            return

        # Read directive name
        while (self._position < len(self._input) and
               (self._is_letter_or_digit(self._input[self._position]) or
                self._input[self._position] == '_')):
            self._position += 1

        self._tokens.append(Token(
            type='DIRECTIVE',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_raw_string(self, start: int) -> None:
        """
        Read a raw string literal token.
        """
        self._position += 1  # Skip "
        while self._position < len(self._input):
            if (self._input[self._position] == '"' and
                    self._position + 1 < len(self._input) and
                    self._input[self._position + 1] == '#'):
                self._position += 2
                break
            self._position += 1

        self._tokens.append(Token(
            type='STRING',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_string(self) -> None:
        """
        Read a string literal token.
        """
        start = self._position
        self._position += 1  # Skip "

        while self._position < len(self._input):
            ch = self._input[self._position]
            self._position += 1

            if ch == '"':
                break

        self._tokens.append(Token(
            type='STRING',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles:
        - Decimal, hexadecimal, octal, and binary literals
        - Floating point numbers
        - Underscores in numbers
        - Type suffixes
        """
        start = self._position

        # Handle hex, octal, or binary prefix
        if self._input[self._position] == '0' and self._position + 1 < len(self._input):
            next_char = self._input[self._position + 1].lower()
            if next_char == 'x':  # Hexadecimal
                self._position += 2
                while self._position < len(self._input):
                    ch = self._input[self._position]
                    if not (self._is_hex_digit(ch) or ch == '_'):
                        break
                    self._position += 1
            elif next_char == 'o':  # Octal
                self._position += 2
                while self._position < len(self._input):
                    ch = self._input[self._position]
                    if not (self._is_octal_digit(ch) or ch == '_'):
                        break
                    self._position += 1
            elif next_char == 'b':  # Binary
                self._position += 2
                while self._position < len(self._input):
                    ch = self._input[self._position]
                    if not (self._is_binary_digit(ch) or ch == '_'):
                        break
                    self._position += 1
            else:
                self._read_decimal_number()
        else:
            self._read_decimal_number()

        self._tokens.append(Token(
            type='NUMBER',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_decimal_number(self) -> None:
        """
        Read a decimal or floating-point number.
        """
        while self._position < len(self._input):
            ch = self._input[self._position]
            if not (self._is_digit(ch) or ch == '_'):
                break
            self._position += 1

        # Handle decimal point and fractional part
        if (self._position < len(self._input) and
                self._input[self._position] == '.'):
            self._position += 1
            while self._position < len(self._input):
                ch = self._input[self._position]
                if not (self._is_digit(ch) or ch == '_'):
                    break
                self._position += 1

        # Handle exponent
        if self._position < len(self._input):
            ch = self._input[self._position].lower()
            if ch == 'e':
                self._position += 1
                if self._input[self._position] in ('+', '-'):
                    self._position += 1
                while self._position < len(self._input):
                    ch = self._input[self._position]
                    if not (self._is_digit(ch) or ch == '_'):
                        break
                    self._position += 1

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be a comment or operator.
        """
        if self._position + 1 < len(self._input):
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
        while (self._position < len(self._input) and
               self._input[self._position] != '\n'):
            self._position += 1

        self._tokens.append(Token(
            type='COMMENT',
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

        while (self._position + 1) < len(self._input):
            if (self._input[self._position] == '*' and
                    self._input[self._position + 1] == '/'):
                self._in_block_comment = False
                self._position += 2
                break

            self._position += 1

        if self._in_block_comment:
            self._position = len(self._input)

        self._tokens.append(Token(
            type='COMMENT',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_operator(self) -> None:
        """
        Read an operator token.

        Handles both standard operators and custom operators.
        """
        operators = [
            '===', '!==', '...', '..<', '<<=', '>>=',
            '+=', '-=', '*=', '/=', '%=', '==', '!=', '>=', '<=', '??', '&&',
            '||', '<<', '>>', '&+', '&-', '&*', '&=', '|=', '^=', '->',
            '=', '+', '-', '*', '/', '%', '>', '<', '?', ':', '!', '~',
            '&', '|', '^', ',', '(', ')', '{', '}', '[', ']', ';', '.'
        ]

        # Try to match standard operators first
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

        # Handle custom operators
        start = self._position
        while self._position < len(self._input):
            ch = self._input[self._position]
            if not self._is_operator_char(ch):
                break
            self._position += 1

        if self._position > start:
            self._tokens.append(Token(
                type='OPERATOR',
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Single character that's not a valid operator
        ch = self._input[self._position]
        self._position += 1
        self._tokens.append(Token(
            type='ERROR',
            value=ch,
            start=start
        ))

    def _is_operator_char(self, ch: str) -> bool:
        """
        Check if a character is valid in an operator.
        """
        operator_chars = {
            '/', '=', '-', '+', '!', '*', '%', '<', '>', '&',
            '|', '^', '~', '?', ':'
        }
        return ch in operator_chars

    def _read_dot(self) -> None:
        """
        Read a dot operator, which could be:
        - Member access operator
        - Float literal starting with .
        - Range operator (..<, ...)
        """
        if (self._position + 1 < len(self._input) and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        self._read_operator()

    def _read_identifier_or_keyword(self) -> None:
        """
        Read an identifier or keyword token.
        
        Swift identifiers can contain:
        - Letters and digits
        - Underscores
        - Dollar signs (implicit parameter names)
        - Backticks (escaped identifiers)
        """
        start = self._position

        # Handle backtick-escaped identifiers
        if self._input[self._position] == '`':
            self._position += 1
            while self._position < len(self._input):
                if self._input[self._position] == '`':
                    self._position += 1
                    break
                self._position += 1

            self._tokens.append(Token(
                type='IDENTIFIER',
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Regular identifier or keyword
        self._position += 1
        while self._position < len(self._input):
            ch = self._input[self._position]
            if not (self._is_letter_or_digit(ch) or ch in ('_', '$')):
                break
            self._position += 1

        value = self._input[start:self._position]
        if self._is_keyword(value):
            self._tokens.append(Token(
                type='KEYWORD',
                value=value,
                start=start
            ))
            return

        self._tokens.append(Token(
            type='IDENTIFIER',
            value=value,
            start=start
        ))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a Swift keyword.
        """
        keywords = {
            # Declaration keywords
            'associatedtype', 'borrowing', 'class', 'consuming', 'deinit', 'enum',
            'extension', 'fileprivate', 'func', 'import', 'init', 'inout', 'internal',
            'let', 'nonisolated', 'open', 'operator', 'private', 'precedencegroup',
            'protocol', 'public', 'rethrows', 'static', 'struct',
            'subscript', 'typealias', 'var',

            # Statement keywords
            'break', 'case', 'catch', 'continue', 'default', 'defer',
            'do', 'else', 'fallthrough', 'for', 'guard', 'if', 'in',
            'repeat', 'return', 'throw', 'switch', 'where', 'while',

            # Expression keywords
            'Any', 'as', 'await', 'false', 'is', 'nil',
            'self', 'Self', 'super', 'throws', 'true', 'try',

            # Special keywords
            'associativity', 'convenience', 'didSet', 'dynamic',
            'final', 'get', 'indirect', 'infix', 'lazy', 'left',
            'mutating', 'none', 'nonmutating', 'optional', 'override',
            'package', 'postfix', 'precedence', 'prefix', 'Protocol', 'required',
            'right', 'set', 'Type', 'unowned', 'weak', 'willSet'
        }
        return value in keywords
