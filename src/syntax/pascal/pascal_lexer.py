from collections.abc import Callable
from dataclasses import dataclass

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class PascalLexerState(LexerState):
    """
    State information for the Pascal lexer.

    Attributes:
        in_brace_comment: Indicates if we're currently parsing a { ... } comment
        in_paren_comment: Indicates if we're currently parsing a (* ... *) comment
        in_string: Indicates if we're currently parsing a multi-line string literal
    """
    in_brace_comment: bool = False
    in_paren_comment: bool = False
    in_string: bool = False


class PascalLexer(Lexer):
    """
    Lexer for Pascal code.

    This lexer handles Pascal-specific syntax including case-insensitive keywords,
    operators, numbers, strings, and comments.  Pascal comments come in three forms:
    ``{ ... }`` (nesting is not allowed), ``(* ... *)``, and ``//`` line comments.
    String literals are single-quoted and may span multiple lines, with ``''`` used
    to embed a single quote.
    """

    # Operators list - ordered by length for greedy matching
    _OPERATORS = [
        ':=', '..', '<=', '>=', '<>', '><',
        '+', '-', '*', '/', '=', '<', '>',
        '(', ')', '[', ']', ',', ';', ':', '^', '@', '.'
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    # Pascal keywords (case-insensitive)
    _KEYWORDS = {
        'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
        'automated', 'begin', 'case', 'cdecl', 'class', 'const', 'constructor',
        'destructor', 'dispinterface', 'div', 'do', 'downto', 'dynamic', 'else',
        'end', 'except', 'export', 'exports', 'external', 'far', 'file',
        'finalization', 'finally', 'for', 'forward', 'function', 'goto', 'if',
        'implementation', 'in', 'inherited', 'initialization', 'inline',
        'interface', 'is', 'label', 'library', 'message', 'mod', 'near', 'nil',
        'not', 'object', 'of', 'on', 'or', 'out', 'overload', 'override',
        'packed', 'private', 'procedure', 'program', 'property', 'protected',
        'public', 'published', 'raise', 'record', 'register', 'reintroduce',
        'repeat', 'resourcestring', 'safecall', 'set', 'shl', 'shr', 'stdcall',
        'string', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
        'uses', 'var', 'virtual', 'while', 'with', 'xor'
    }

    # Boolean literals (case-insensitive)
    _BOOLEANS = {'true', 'false'}

    def __init__(self) -> None:
        super().__init__()
        self._in_brace_comment = False
        self._in_paren_comment = False
        self._in_string = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> PascalLexerState:
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
        self._position = 0
        self._tokens = []
        self._next_token = 0

        if prev_lexer_state is not None:
            assert isinstance(prev_lexer_state, PascalLexerState), \
                f"Expected PascalLexerState, got {type(prev_lexer_state).__name__}"

            self._in_brace_comment = prev_lexer_state.in_brace_comment
            self._in_paren_comment = prev_lexer_state.in_paren_comment
            self._in_string = prev_lexer_state.in_string

        if self._in_brace_comment:
            self._read_brace_comment(0)

        if self._in_paren_comment:
            self._read_paren_comment(0)

        if self._in_string:
            self._continue_string()

        if not self._in_brace_comment and not self._in_paren_comment and not self._in_string:
            self._inner_lex()

        lexer_state = PascalLexerState()
        lexer_state.in_brace_comment = self._in_brace_comment
        lexer_state.in_paren_comment = self._in_paren_comment
        lexer_state.in_string = self._in_string
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

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch == "'":
            return self._read_string

        if ch == '{':
            return self._read_brace_comment_start

        if ch == '(':
            return self._read_lparen

        if ch == '/':
            return self._read_forward_slash

        if ch == '$':
            return self._read_hex_number

        if ch == '#':
            return self._read_character_code

        if ch == '%':
            return self._read_binary_number

        if ch == '&':
            return self._read_octal_number

        return self._read_operator

    def _read_lparen(self) -> None:
        """
        Read a left parenthesis, which could be the start of a ``(* ... *)`` comment.
        """
        if self._position + 1 < self._input_len and self._input[self._position + 1] == '*':
            self._read_paren_comment(2)
            return

        self._read_operator()

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash, which could be the start of a ``//`` line comment.
        """
        if self._position + 1 < self._input_len and self._input[self._position + 1] == '/':
            self._read_comment()
            return

        self._read_operator()

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

    def _read_brace_comment_start(self) -> None:
        """
        Read the start of a ``{ ... }`` block comment.
        """
        self._read_brace_comment(1)

    def _read_brace_comment(self, skip_chars: int) -> None:
        """
        Read a ``{ ... }`` block comment token.

        Args:
            skip_chars: Number of characters to skip at the start
        """
        self._in_brace_comment = True
        start = self._position
        self._position += skip_chars

        while self._position < self._input_len:
            if self._input[self._position] == '}':
                self._in_brace_comment = False
                self._position += 1
                break

            self._position += 1

        if self._in_brace_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_paren_comment(self, skip_chars: int) -> None:
        """
        Read a ``(* ... *)`` block comment token.

        Args:
            skip_chars: Number of characters to skip at the start
        """
        self._in_paren_comment = True
        start = self._position
        self._position += skip_chars

        while self._position + 1 < self._input_len:
            if self._input[self._position] == '*' and self._input[self._position + 1] == ')':
                self._in_paren_comment = False
                self._position += 2
                break

            self._position += 1

        if self._in_paren_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_string(self) -> None:
        """
        Read a single-quoted string literal token.

        Pascal escapes an embedded single quote by doubling it (``''``).  A string
        literal may span multiple lines.
        """
        start = self._position
        self._position += 1
        self._in_string = True

        while self._position < self._input_len:
            if self._input[self._position] == "'":
                # A doubled quote is an escaped quote, not the end of the string.
                if (self._position + 1 < self._input_len and
                        self._input[self._position + 1] == "'"):
                    self._position += 2
                    continue

                self._in_string = False
                self._position += 1
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _continue_string(self) -> None:
        """
        Continue reading a string literal that began on a previous line.
        """
        start = self._position

        while self._position < self._input_len:
            if self._input[self._position] == "'":
                if (self._position + 1 < self._input_len and
                        self._input[self._position + 1] == "'"):
                    self._position += 2
                    continue

                self._in_string = False
                self._position += 1
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_hex_number(self) -> None:
        """
        Read a hexadecimal number literal of the form ``$FF``.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               self._is_hex_digit(self._input[self._position])):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_binary_number(self) -> None:
        """
        Read a binary number literal of the form ``%1010``.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               self._is_binary_digit(self._input[self._position])):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_octal_number(self) -> None:
        """
        Read an octal number literal of the form ``&77``.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               self._is_octal_digit(self._input[self._position])):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_character_code(self) -> None:
        """
        Read a character code of the form ``#65`` or ``#$41``.
        """
        start = self._position
        self._position += 1

        if self._position < self._input_len and self._input[self._position] == '$':
            self._position += 1
            while (self._position < self._input_len and
                   self._is_hex_digit(self._input[self._position])):
                self._position += 1

        else:
            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

        self._tokens.append(Token(
            type=TokenType.CHARACTER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles decimal integers, floating-point numbers, and scientific notation.
        """
        start = self._position

        while (self._position < self._input_len and
               self._is_digit(self._input[self._position])):
            self._position += 1

        if (self._position < self._input_len and
                self._input[self._position] == '.' and
                self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
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

    def _read_identifier_or_keyword(self) -> None:
        """
        Read an identifier, keyword, or boolean literal token.

        Keyword and boolean matching is case-insensitive; the original casing is
        preserved in the token value.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]))):
            self._position += 1

        value = self._input[start:self._position]
        lower = value.lower()

        if lower in self._BOOLEANS:
            self._tokens.append(Token(type=TokenType.BOOLEAN, value=value, start=start))
            return

        if lower in self._KEYWORDS:
            self._tokens.append(Token(type=TokenType.KEYWORD, value=value, start=start))
            return

        self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))
