from dataclasses import dataclass
from typing import Callable, cast

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class SchemeLexerState(LexerState):
    """
    State information for the Scheme lexer.

    Attributes:
        in_comment: Indicates if we're currently parsing a multi-line comment
    """
    in_comment: bool = False


class SchemeLexer(Lexer):
    """
    Lexer for R5RS Scheme code.

    This lexer handles Scheme-specific syntax including identifiers, numbers,
    strings, and special forms.
    """

    def __init__(self) -> None:
        super().__init__()
        self._in_comment = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> SchemeLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing

        Raises:
            TypeError: If the previous lexer state is not None and not a SchemeLexerState instance
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state is not None:
            prev_lexer_state = cast(SchemeLexerState, prev_lexer_state)
            self._in_comment = prev_lexer_state.in_comment

        if self._in_comment:
            self._read_multiline_comment(0)

        if not self._in_comment:
            self._inner_lex()

        lexer_state = SchemeLexerState()
        lexer_state.in_comment = self._in_comment
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

        if ch == '"':
            return self._read_string

        if ch == ';':
            return self._read_comment

        if ch == '#':
            return self._read_hash_token

        if ch in ('(', ')'):
            return self._read_delimiter

        if ch == '.':
            return self._read_dot_or_number

        if ch in ('+', '-'):
            return self._read_number_or_identifier

        if self._is_digit(ch):
            return self._read_number

        return self._read_identifier

    def _read_hash_token(self) -> None:
        """
        Read a hash token which could be:
        - Boolean (#t, #f)
        - Character (#\\x)
        - Vector (#(...))
        - Binary/octal/decimal/hex (#b, #o, #d, #x)
        - Block comment (#| ... |#)
        """
        start = self._position
        if self._position + 1 >= self._input_len:
            self._tokens.append(Token(type=TokenType.ERROR, value='#', start=start))
            return

        ch = self._input[self._position + 1].lower()

        # Handle booleans
        if ch in ('t', 'f'):
            self._position += 2
            self._tokens.append(Token(
                type=TokenType.BOOLEAN,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Handle block comments
        if ch == '|':
            self._read_multiline_comment(2)
            return

        # Handle characters
        if ch == '\\':
            self._position += 2
            while (self._position < self._input_len and
                   not self._is_delimiter(self._input[self._position])):
                self._position += 1
            self._tokens.append(Token(
                type=TokenType.CHARACTER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Handle vectors
        if ch == '(':
            self._position += 2
            self._tokens.append(Token(type=TokenType.VECTOR_START, value='#(', start=start))
            return

        # Handle number bases
        if ch in ('b', 'o', 'd', 'x'):
            self._read_based_number(ch)
            return

        self._position += 1
        self._tokens.append(Token(type=TokenType.ERROR, value='#', start=start))

    def _read_based_number(self, base: str) -> None:
        """
        Read a number with an explicit base.

        Args:
            base: The base indicator ('b', 'o', 'd', 'x')
        """
        start = self._position
        self._position += 2  # Include the #base prefix

        # Read digits according to base
        while self._position < self._input_len:
            ch = self._input[self._position].lower()
            valid = False

            if base == 'b' and ch in '01':
                valid = True
            elif base == 'o' and '0' <= ch <= '7':
                valid = True
            elif base == 'd' and self._is_digit(ch):
                valid = True
            elif base == 'x' and self._is_hex_digit(ch):
                valid = True

            if not valid:
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_multiline_comment(self, skip_chars: int) -> None:
        """
        Read a multi-line comment token.
        """
        self._in_comment = True
        start = self._position
        self._position += skip_chars
        while self._position + 1 < self._input_len:
            if (self._input[self._position] == '|' and
                    self._input[self._position + 1] == '#'):
                self._in_comment = False
                self._position += 2
                break

            self._position += 1

        # If we're still in a block comment we've got one character left on this line and
        # we need to include it in the comment too.
        if self._in_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_identifier(self) -> None:
        """
        Read an identifier token.

        In Scheme, identifiers can contain letters, digits, and special characters.
        They cannot start with a digit and are case-insensitive.
        """
        start = self._position
        while self._position < self._input_len:
            ch = self._input[self._position]
            if self._is_delimiter(ch):
                break

            self._position += 1

        value = self._input[start:self._position]

        if self._is_special_form(value):
            self._tokens.append(Token(
                type=TokenType.KEYWORD,
                value=value,
                start=start
            ))
            return

        self._tokens.append(Token(
            type=TokenType.IDENTIFIER,
            value=value,
            start=start
        ))

    def _read_number_or_identifier(self) -> None:
        """
        Read an operator as identifier or start of number.
        """
        if (self._position + 1 < self._input_len and
            self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        self._read_identifier()

    def _read_dot_or_number(self) -> None:
        """
        Read a dot token or start of decimal number.
        """
        if (self._position + 1 < self._input_len and
            self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.DOT,
            value='.',
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal.

        Handles:
        - Integers: 42
        - Decimals: 3.14
        - Scientific: 1e10
        - Complex: 3+4i
        - Rationals: 22/7
        - Signed: +42, -42
        """
        start = self._position

        # Handle sign
        if self._input[self._position] in ('+', '-'):
            self._position += 1

        # Read integer part
        while (self._position < self._input_len and
               self._is_digit(self._input[self._position])):
            self._position += 1

        # Handle decimal point
        if (self._position < self._input_len and
                self._input[self._position] == '.'):
            self._position += 1
            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

        # Handle exponent
        if (self._position < self._input_len and
                self._input[self._position].lower() == 'e'):
            self._position += 1
            if self._input[self._position] in ('+', '-'):
                self._position += 1
            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

        # Handle rational
        if (self._position < self._input_len and
                self._input[self._position] == '/'):
            self._position += 1
            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

        # Handle complex
        if (self._position < self._input_len and
                self._input[self._position].lower() == 'i'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_delimiter(self) -> None:
        """
        Read a delimiter token (parentheses or brackets).
        """
        start = self._position
        ch = self._input[self._position]
        self._position += 1

        token_type = TokenType.LPAREN if ch == '(' else TokenType.RPAREN

        self._tokens.append(Token(type=token_type, value=ch, start=start))

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

    def _is_delimiter(self, ch: str) -> bool:
        """
        Determines if a character is a delimiter.

        Args:
            ch: The character to check

        Returns:
            True if the character is a delimiter, False otherwise
        """
        return (self._is_whitespace(ch) or ch in ('(', ')'))

    def _is_special_form(self, value: str) -> bool:
        """
        Check if a given value is a Scheme special form.

        Args:
            value: The string to check

        Returns:
            True if the value is a special form, False otherwise
        """
        special_forms = {
            'let-syntax', 'letrec-syntax', 'syntax-rules', 'define-syntax',
            'else', '=>', 'define', 'unquote-splicing', 'unquote',
            'quote', 'lambda', 'if', 'set!', 'begin', 'cond', 'and', 'or', 'case',
            'let', 'let*', 'letrec', 'do', 'delay', 'quasiquote'
        }
        return value.lower() in special_forms
