from dataclasses import dataclass
from typing import Callable, cast

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class KotlinLexerState(LexerState):
    """
    State information for the Kotlin lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        in_string_template: Indicates if we're in a string template expression
        string_template_braces: Number of nested braces in string template
    """
    in_block_comment: bool = False
    in_string_template: bool = False
    string_template_braces: int = 0


class KotlinLexer(Lexer):
    """
    Lexer for Kotlin code.

    This lexer handles Kotlin-specific syntax including:
    - Keywords and soft keywords
    - String templates with nested expressions
    - Multi-line strings
    - Operators and property accessors
    - Numbers with underscores and type suffixes
    """

    # Operators list
    _OPERATORS = [
        '..<', '===', '!==', '..', '::', '?.', '?:', '!!', '++', '--',
        '&&', '||', '+=', '-=', '*=', '/=', '%=', '==', '!=', '<=',
        '>=', '->', '=>', '+', '-', '*', '/', '%', '=', '<', '>', '_'
        '@', '$', '!', '?', ':', '.', '(', ')', '{', '}', '[', ']', ',', ';'
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def __init__(self) -> None:
        super().__init__()
        self._in_block_comment = False
        self._in_string_template = False
        self._string_template_braces = 0
        self._raw_string_quotes = 0

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> KotlinLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing

        Raises:
            TypeError: If the previous lexer state is not None and not a KotlinLexerState instance
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state is not None:
            prev_lexer_state = cast(KotlinLexerState, prev_lexer_state)
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._in_string_template = prev_lexer_state.in_string_template
            self._string_template_braces = prev_lexer_state.string_template_braces

        if self._in_block_comment:
            self._read_block_comment(0)

        if not self._in_block_comment:
            self._inner_lex()

        lexer_state = KotlinLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.in_string_template = self._in_string_template
        lexer_state.string_template_braces = self._string_template_braces
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

        if ch == 'a':
            return self._read_a_token

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch in ('"', "'"):
            return self._read_string

        if ch == '`':
            return self._read_backtick_identifier

        if ch == '@':
            return self._read_annotation

        if ch == '/':
            return self._read_forward_slash

        if ch == '!':
            return self._read_exclamation

        if ch == '.':
            return self._read_dot

        return self._read_operator

    def _read_exclamation(self) -> None:
        """
        Read an exclamation mark token which could be:
        - !is keyword
        - !in keyword
        - !! not-null assertion operator
        - ! regular not operator
        """
        start = self._position

        if self._position + 2 < self._input_len:
            if (self._input[self._position + 1] == 'i' and
                    self._input[self._position + 2] == 's'):
                self._position += 3
                self._tokens.append(Token(
                    type=TokenType.KEYWORD,
                    value='!is',
                    start=start
                ))
                return

            if (self._input[self._position + 1] == 'i' and
                    self._input[self._position + 2] == 'n'):
                self._position += 3
                self._tokens.append(Token(
                    type=TokenType.KEYWORD,
                    value='!in',
                    start=start
                ))
                return

            if self._input[self._position + 1] == '!':
                self._position += 2
                self._tokens.append(Token(
                    type=TokenType.OPERATOR,
                    value='!!',
                    start=start
                ))
                return

        # Simple negation operator
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value='!',
            start=start
        ))

    def _read_dot(self) -> None:
        """
        Read a dot operator or decimal point in a number.
        Also handles range operator (..).
        """
        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        # Check for range operator
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] == '.'):
            start = self._position
            self._position += 2
            self._tokens.append(Token(
                type=TokenType.OPERATOR,
                value='..',
                start=start
            ))
            return

        self._read_operator()

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be a comment or operator.
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
        Read a string literal token, handling string templates and multi-line strings.
        """
        start = self._position
        quote = self._input[start]
        is_triple_quoted = False
        self._position += 1

        # Check for triple quote
        if (self._position + 1 < self._input_len and
                self._input[self._position] == '"' and
                self._input[self._position + 1] == '"'):
            is_triple_quoted = True
            self._position += 2

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\':
                self._position += 2
                continue

            if ch == '$' and self._position + 1 < self._input_len:
                next_ch = self._input[self._position + 1]
                if next_ch == '{':
                    self._in_string_template = True
                    self._string_template_braces += 1
                    self._position += 2
                    self._tokens.append(Token(
                        type=TokenType.STRING,
                        value=self._input[start:self._position],
                        start=start
                    ))
                    return
                if self._is_letter(next_ch):
                    self._position += 1
                    continue

            if ch == quote:
                if is_triple_quoted:
                    if (self._position + 2 < self._input_len and
                            self._input[self._position + 1] == '"' and
                            self._input[self._position + 2] == '"'):
                        self._position += 3
                        break
                else:
                    self._position += 1
                    break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_backtick_identifier(self) -> None:
        """
        Read a backtick-quoted identifier token.
        """
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            if self._input[self._position] == '`':
                self._position += 1
                break
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.IDENTIFIER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_annotation(self) -> None:
        """
        Read an annotation token.
        """
        start = self._position
        self._position += 1

        # Handle use-site target if present
        if self._position < self._input_len:
            ch = self._input[self._position]
            if ch in ('f', 'g', 'p', 's', 'r', 'v', 'd', 'i'):
                self._position += 1
                if self._position < self._input_len and self._input[self._position] == ':':
                    self._position += 1

        # Read the annotation name
        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] == '.')):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.ANNOTATION,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles:
        - Decimal, hexadecimal, and binary literals
        - Underscores in numbers
        - Type suffixes (L, F, etc.)
        """
        start = self._position

        if (self._input[self._position] == '0' and
                self._position + 1 < self._input_len):
            next_char = self._input[self._position + 1].lower()
            if next_char == 'x':  # Hexadecimal
                self._position += 2
                while self._position < self._input_len:
                    ch = self._input[self._position]
                    if not (self._is_hex_digit(ch) or ch == '_'):
                        break
                    self._position += 1
            elif next_char == 'b':  # Binary
                self._position += 2
                while self._position < self._input_len:
                    ch = self._input[self._position]
                    if not (self._is_binary_digit(ch) or ch == '_'):
                        break
                    self._position += 1
            else:
                self._read_decimal_number()
        else:
            self._read_decimal_number()

        # Handle type suffixes
        if self._position < self._input_len:
            ch = self._input[self._position].lower()
            if ch in ('u', 'l', 'f'):
                self._position += 1
                if ch == 'u' and self._position < self._input_len:
                    if self._input[self._position].lower() == 'l':
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
        while self._position < self._input_len:
            ch = self._input[self._position]
            if not (self._is_digit(ch) or ch == '_'):
                break
            self._position += 1

        if (self._position < self._input_len and
                self._input[self._position] == '.'):
            self._position += 1
            while self._position < self._input_len:
                ch = self._input[self._position]
                if not (self._is_digit(ch) or ch == '_'):
                    break
                self._position += 1

        if (self._position < self._input_len and
                self._input[self._position].lower() == 'e'):
            self._position += 1
            if self._input[self._position] in ('+', '-'):
                self._position += 1

            while self._position < self._input_len:
                ch = self._input[self._position]
                if not (self._is_digit(ch) or ch == '_'):
                    break
                self._position += 1

    def _read_comment(self) -> None:
        """
        Read a single-line comment token.
        Handles comments starting with // until the end of line.
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
        self._position += skip_chars

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

    def _read_a_token(self) -> None:
        """
        Read a token starting with 'a', handling the special case of 'as?'.
        """
        # Special case: check for 'as?' operator
        if (self._position + 2 < self._input_len and
                self._input[self._position:self._position+2] == 'as' and
                self._input[self._position+2] == '?'):
            start = self._position
            self._position += 3
            self._tokens.append(Token(
                type=TokenType.KEYWORD,
                value='as?',
                start=start
            ))
            return

        # Not 'as?', treat as normal identifier/keyword
        self._read_identifier_or_keyword()

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
        Check if a given value is a Kotlin keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Kotlin keyword, False otherwise
        """
        keywords = {
            # Hard keywords
            'as', 'as?', 'break', 'class', 'continue', 'do', 'else', 'false',
            'for', 'fun', 'if', 'in', '!in', 'interface', '!is', 'is', 'null', 'object',
            'package', 'return', 'super', 'this', 'throw', 'true', 'try',
            'typealias', 'typeof', 'val', 'var', 'when', 'while',

            # Soft keywords
            'by', 'catch', 'constructor', 'delegate', 'dynamic', 'field',
            'file', 'finally', 'get', 'import', 'init', 'param', 'property',
            'receiver', 'set', 'setparam', 'where',

            # Modifier keywords
            'abstract', 'actual', 'annotation', 'companion', 'const',
            'crossinline', 'data', 'enum', 'expect', 'external', 'final',
            'infix', 'inline', 'inner', 'internal', 'lateinit', 'noinline',
            'open', 'operator', 'out', 'override', 'private', 'protected',
            'public', 'reified', 'sealed', 'suspend', 'tailrec', 'vararg'
        }
        return value in keywords
