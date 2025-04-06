from dataclasses import dataclass
from typing import Callable, cast

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class JavaLexerState(LexerState):
    """
    State information for the Java lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        in_javadoc: Indicates if we're currently parsing a JavaDoc comment
        in_text_block: Indicates if we're currently parsing a text block
        in_annotation: Indicates if we're currently parsing an annotation
        text_block_quotes: Number of quotes in text block delimiter
    """
    in_block_comment: bool = False
    in_javadoc: bool = False
    in_text_block: bool = False
    in_annotation: bool = False
    text_block_quotes: int = 0


class JavaLexer(Lexer):
    """
    Lexer for Java code.

    This lexer handles Java-specific syntax including:
    - Keywords and modifiers
    - String literals (including text blocks)
    - Numbers with underscores and suffixes
    - Comments (including JavaDoc)
    - Annotations
    - Generics
    - Lambda expressions
    - Module declarations
    """

    # Operators list
    _OPERATORS = [
        '>>>=', '>>=', '<<=', '...', '->', '::', '++', '--',
        '&&', '||', '>=', '<=', '==', '!=', '+=', '-=', '*=',
        '/=', '&=', '|=', '^=', '%=', '>>', '<<', '>>>', '>',
        '<', '!', '~', '?', ':', '.', '&', '|', '^', '+', '-',
        '*', '/', '%', '=', '(', ')', '{', '}', '[', ']', ';',
        ',', '@'
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self):
        super().__init__()
        self._in_block_comment = False
        self._in_javadoc = False
        self._in_text_block = False
        self._in_annotation = False
        self._text_block_quotes = 0

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> JavaLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing

        Raises:
            TypeError: If the previous lexer state is not None and not a JavaLexerState instance
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state is not None:
            prev_lexer_state = cast(JavaLexerState, prev_lexer_state)
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._in_javadoc = prev_lexer_state.in_javadoc
            self._in_text_block = prev_lexer_state.in_text_block
            self._in_annotation = prev_lexer_state.in_annotation
            self._text_block_quotes = prev_lexer_state.text_block_quotes

        if self._in_block_comment or self._in_javadoc:
            self._read_block_comment(0)

        if self._in_text_block:
            self._read_text_block(0)

        if not (self._in_block_comment or self._in_javadoc or self._in_text_block):
            self._inner_lex()

        lexer_state = JavaLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.in_javadoc = self._in_javadoc
        lexer_state.in_text_block = self._in_text_block
        lexer_state.in_annotation = self._in_annotation
        lexer_state.text_block_quotes = self._text_block_quotes
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

        if ch == '@':
            return self._read_annotation

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

    def _read_string(self) -> None:
        """
        Read a string literal token.

        Handles both regular string literals and text blocks (triple-quoted strings).
        Also handles escape sequences within strings.
        """
        start = self._position
        quote = self._input[self._position]

        # Check for text block (triple-quoted string)
        if (quote == '"' and
            self._position + 2 < self._input_len and
            self._input[self._position + 1] == '"' and
            self._input[self._position + 2] == '"'):
            self._read_text_block(3)
            return

        # Regular string literal
        self._position += 1
        while self._position < self._input_len and self._input[self._position] != quote:
            if self._input[self._position] == '\\':
                self._position += 2  # Skip escape sequence
                continue
            self._position += 1

        if self._position < self._input_len:
            self._position += 1  # Include closing quote

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_text_block(self, skip_chars: int) -> None:
        """
        Read a text block (triple-quoted string) token.

        Args:
            skip_chars: Number of characters to skip at start
        """
        self._in_text_block = True
        start = self._position
        self._position += skip_chars

        while self._position + 2 < self._input_len:
            if (self._input[self._position] == '"' and
                self._input[self._position + 1] == '"' and
                self._input[self._position + 2] == '"'):
                self._in_text_block = False
                self._position += 3
                break

            if self._input[self._position] == '\\':
                self._position += 2  # Skip escape sequence
                continue

            self._position += 1

        if self._in_text_block:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_dot(self) -> None:
        """
        Read a dot operator or decimal point.
        Handles:
        - Method/field access operator (.)
        - Start of float literal (.123)
        - Varargs ellipsis (...)
        """
        # Check for varargs ellipsis
        if (self._position + 2 < self._input_len and
            self._input[self._position + 1] == '.' and
            self._input[self._position + 2] == '.'):
            start = self._position
            self._position += 3
            self._tokens.append(Token(
                type=TokenType.OPERATOR,
                value='...',
                start=start
            ))
            return

        # Check if this is the start of a float literal
        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        # Just a regular dot operator
        self._read_operator()

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles:
        - Decimal integers with optional underscore separators
        - Hexadecimal numbers (0x prefix)
        - Binary numbers (0b prefix)
        - Octal numbers (0 prefix)
        - Floating point numbers
        - Number suffixes (L, F, D)
        """
        start = self._position

        # Check for hex, binary, or octal prefix
        if self._input[self._position] == '0' and self._position + 1 < self._input_len:
            next_char = self._input[self._position + 1].lower()
            if next_char == 'x':  # Hexadecimal
                self._position += 2
                self._read_hex_digits()
            elif next_char == 'b':  # Binary
                self._position += 2
                self._read_binary_digits()
            else:  # Octal or decimal
                self._read_decimal_number()
        else:
            self._read_decimal_number()

        # Handle suffixes
        if self._position < self._input_len:
            suffix = self._input[self._position].lower()
            if suffix in ('l', 'f', 'd'):
                self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_hex_digits(self) -> None:
        """Read hexadecimal digits, allowing underscores between digits."""
        while self._position < self._input_len:
            ch = self._input[self._position]
            if not (self._is_hex_digit(ch) or ch == '_'):
                break
            self._position += 1

    def _read_binary_digits(self) -> None:
        """Read binary digits, allowing underscores between digits."""
        while self._position < self._input_len:
            ch = self._input[self._position]
            if not (self._is_binary_digit(ch) or ch == '_'):
                break
            self._position += 1

    def _read_decimal_number(self) -> None:
        """
        Read a decimal number, which could be an integer or floating point.
        Handles underscores between digits and scientific notation.
        """
        # Read integral part
        while self._position < self._input_len:
            ch = self._input[self._position]
            if not (self._is_digit(ch) or ch == '_'):
                break
            self._position += 1

        # Check for decimal point
        if (self._position < self._input_len and
            self._input[self._position] == '.'):
            self._position += 1
            while self._position < self._input_len:
                ch = self._input[self._position]
                if not (self._is_digit(ch) or ch == '_'):
                    break
                self._position += 1

        # Check for exponent
        if self._position < self._input_len:
            ch = self._input[self._position].lower()
            if ch == 'e':
                self._position += 1
                if self._position < self._input_len and self._input[self._position] in ('+', '-'):
                    self._position += 1
                while self._position < self._input_len:
                    ch = self._input[self._position]
                    if not (self._is_digit(ch) or ch == '_'):
                        break
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
        else:
            self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))

    def _read_annotation(self) -> None:
        """
        Read an annotation token.
        """
        start = self._position
        self._position += 1  # Skip @

        # Read annotation name
        while self._position < self._input_len:
            ch = self._input[self._position]
            if not (self._is_letter_or_digit_or_underscore(ch) or ch == '.'):
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.ANNOTATION,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be:
        - Start of a single-line comment (//)
        - Start of a block comment (/*)
        - Start of a JavaDoc comment (/**)
        - Division operator (/)
        """
        if self._position + 1 >= self._input_len:
            self._read_operator()
            return

        next_char = self._input[self._position + 1]
        if next_char == '/':
            self._read_line_comment()
        elif next_char == '*':
            if (self._position + 2 < self._input_len and
                self._input[self._position + 2] == '*'):
                self._in_javadoc = True
                self._read_block_comment(3)
            else:
                self._read_block_comment(2)
        else:
            self._read_operator()

    def _read_line_comment(self) -> None:
        """Read a single-line comment token."""
        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _read_block_comment(self, skip_chars: int) -> None:
        """
        Read a block comment token (either regular or JavaDoc).

        Args:
            skip_chars: Number of characters to skip at start
        """
        self._in_block_comment = True
        start = self._position
        self._position += skip_chars

        while self._position + 1 < self._input_len:
            if (self._input[self._position] == '*' and
                self._input[self._position + 1] == '/'):
                self._in_block_comment = False
                self._in_javadoc = False
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

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a Java keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Java keyword, False otherwise
        """
        keywords = {
            # Regular keywords
            'abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch',
            'char', 'class', 'const', 'continue', 'default', 'do', 'double',
            'else', 'enum', 'extends', 'final', 'finally', 'float', 'for',
            'goto', 'if', 'implements', 'import', 'instanceof', 'int',
            'interface', 'long', 'native', 'new', 'package', 'private',
            'protected', 'public', 'return', 'short', 'static', 'strictfp',
            'super', 'switch', 'synchronized', 'this', 'throw', 'throws',
            'transient', 'try', 'void', 'volatile', 'while',

            # Literal values
            'true', 'false', 'null',

            # Module system keywords (Java 9+)
            'module', 'requires', 'exports', 'opens', 'uses', 'provides', 'with',
            'to', 'transitive',

            # Context keywords
            'var',  # Local variable type inference (Java 10+)
            'yield', # Switch expressions (Java 13+)
            'record',  # Record classes (Java 14+)
            'sealed', 'permits', # Sealed classes (Java 15+)
            'non-sealed'  # Sealed classes (Java 15+)
        }
        return value in keywords
