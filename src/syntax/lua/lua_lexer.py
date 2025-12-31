from dataclasses import dataclass
from typing import Callable

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class LuaLexerState(LexerState):
    """
    State information for the Lua lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        in_multiline_string: Indicates if we're currently parsing a multi-line string
        string_bracket_level: Number of = signs in [=[...]=] style strings
    """
    in_block_comment: bool = False
    in_multiline_string: bool = False
    string_bracket_level: int = 0


class LuaLexer(Lexer):
    """
    Lexer for Lua code.

    This lexer handles Lua-specific syntax including keywords, operators, numbers,
    strings (including multi-line bracket strings), and comments (including block comments).
    """

    # Operators list - ordered by length for greedy matching
    _OPERATORS = [
        '...', '==', '~=', '<=', '>=', '..',
        '+', '-', '*', '/', '%', '^', '#',
        '(', ')', '{', '}', '[', ']', ';',
        ':', ',', '.', '=', '<', '>'
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    # Lua keywords
    _KEYWORDS = {
        'and', 'break', 'do', 'else', 'elseif', 'end', 'false', 'for',
        'function', 'if', 'in', 'local', 'nil', 'not', 'or', 'repeat',
        'return', 'then', 'true', 'until', 'while'
    }

    # Boolean literals
    _BOOLEANS = {'true', 'false', 'nil'}

    def __init__(self) -> None:
        super().__init__()
        self._in_block_comment = False
        self._in_multiline_string = False
        self._string_bracket_level = 0

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> LuaLexerState:
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
            assert isinstance(prev_lexer_state, LuaLexerState), \
                f"Expected LuaLexerState, got {type(prev_lexer_state).__name__}"
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._in_multiline_string = prev_lexer_state.in_multiline_string
            self._string_bracket_level = prev_lexer_state.string_bracket_level

        if self._in_block_comment:
            self._read_block_comment(0)

        if self._in_multiline_string:
            self._read_multiline_string(0)

        if not self._in_block_comment and not self._in_multiline_string:
            self._inner_lex()

        lexer_state = LuaLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.in_multiline_string = self._in_multiline_string
        lexer_state.string_bracket_level = self._string_bracket_level
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

        if ch in ('"', "'"):
            return self._read_string

        if ch == '[':
            return self._read_bracket

        if ch == ']':
            return self._read_close_bracket

        if ch == '-':
            return self._read_minus_or_comment

        if ch == '.':
            return self._read_dot

        return self._read_operator

    def _read_minus_or_comment(self) -> None:
        """
        Read a minus sign or comment.

        Handles:
        - Single-line comments: --
        - Block comments: --[[ ... ]]
        """
        if self._position + 1 < self._input_len and self._input[self._position + 1] == '-':
            # It's a comment
            if (self._position + 3 < self._input_len and
                    self._input[self._position + 2] == '[' and
                    self._input[self._position + 3] == '['):
                # Block comment start: --[[
                self._read_block_comment(4)

            # Single-line comment
            self._read_comment()
            return

        # It's just a minus operator
        self._read_operator()

    def _read_dot(self) -> None:
        """
        Read a dot operator.

        Handles: . (field access), .. (concatenation), ... (varargs)
        These are all handled by the operator map's greedy matching.
        """
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

    def _read_block_comment(self, skip_chars: int) -> None:
        """
        Read a block comment token.

        Args:
            skip_chars: Number of characters to skip at the start
        """
        self._in_block_comment = True
        start = self._position
        self._position += skip_chars

        while self._position + 1 < self._input_len:
            if self._input[self._position] == ']' and self._input[self._position + 1] == ']':
                self._in_block_comment = False
                self._position += 2
                break

            self._position += 1

        # If we're still in a block comment, we've reached end of line
        if self._in_block_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_bracket(self) -> None:
        """
        Read a bracket, which could be:
        - Start of table indexing: [
        - Start of multi-line string: [[ or [=[ or [==[ etc.
        """
        if self._position + 1 < self._input_len and self._input[self._position + 1] == '[':
            # Multi-line string start: [[
            self._read_multiline_string(2)
            return

        if self._position + 1 < self._input_len and self._input[self._position + 1] == '=':
            # Check for [=[...]=] style string
            bracket_level = 0
            pos = self._position + 1

            while pos < self._input_len and self._input[pos] == '=':
                bracket_level += 1
                pos += 1

            if pos < self._input_len and self._input[pos] == '[':
                # Multi-line string with = signs: [=[ or [==[ etc.
                self._read_multiline_string(bracket_level + 2)
                return

        # It's just a regular bracket (table indexing)
        self._read_operator()

    def _read_close_bracket(self) -> None:
        """
        Read a closing bracket.

        If we're in a multi-line string, check if this closes it.
        """
        if self._in_multiline_string:
            # Check if this closes the multi-line string
            bracket_level = 0
            pos = self._position + 1

            while pos < self._input_len and self._input[pos] == '=':
                bracket_level += 1
                pos += 1

            if pos < self._input_len and self._input[pos] == ']':
                # Check if bracket levels match
                if bracket_level == self._string_bracket_level:
                    # This closes the multi-line string
                    start = self._position
                    self._position = pos + 1
                    self._in_multiline_string = False
                    self._string_bracket_level = 0
                    self._tokens.append(Token(
                        type=TokenType.STRING,
                        value=self._input[start:self._position],
                        start=start
                    ))
                    return

        # Not closing a multi-line string, treat as operator
        self._read_operator()

    def _read_multiline_string(self, skip_chars: int) -> None:
        """
        Read a multi-line string token.

        Args:
            skip_chars: Number of characters to skip at the start (includes the brackets)
        """
        self._in_multiline_string = True

        # Calculate bracket level from the opening
        if skip_chars > 2:
            self._string_bracket_level = skip_chars - 2
        else:
            self._string_bracket_level = 0

        start = self._position
        self._position += skip_chars

        # Look for the closing bracket sequence
        while self._position + 1 < self._input_len:
            if self._input[self._position] == ']':
                # Check if this could be the closing bracket
                bracket_level = 0
                pos = self._position + 1

                while pos < self._input_len and self._input[pos] == '=':
                    bracket_level += 1
                    pos += 1

                if pos < self._input_len and self._input[pos] == ']':
                    # Check if bracket levels match
                    if bracket_level == self._string_bracket_level:
                        self._in_multiline_string = False
                        self._string_bracket_level = 0
                        self._position = pos + 1
                        break

            self._position += 1

        # If we're still in a multi-line string, we've reached end of line
        if self._in_multiline_string:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal.

        Handles:
        - Decimal integers: 42
        - Decimal floats: 3.14
        - Hexadecimal: 0x2A
        - Hexadecimal floats: 0x1.fp3 (Lua 5.2+)
        - Scientific notation: 1e10, 0x1p4
        """
        start = self._position

        # Check for hexadecimal
        if (self._input[self._position] == '0' and
                self._position + 1 < self._input_len and
                self._input[self._position + 1].lower() == 'x'):
            # Hexadecimal number
            self._position += 2
            while (self._position < self._input_len and
                   self._is_hex_digit(self._input[self._position])):
                self._position += 1

            # Check for hexadecimal fractional part
            if self._position < self._input_len and self._input[self._position] == '.':
                self._position += 1
                while (self._position < self._input_len and
                       self._is_hex_digit(self._input[self._position])):
                    self._position += 1

            # Check for hexadecimal exponent (p or P)
            if (self._position < self._input_len and
                    self._input[self._position].lower() == 'p'):
                self._position += 1
                if self._position < self._input_len and self._input[self._position] in ('+', '-'):
                    self._position += 1
                while (self._position < self._input_len and
                       self._is_digit(self._input[self._position])):
                    self._position += 1
        else:
            # Decimal number
            # Read integer part
            while (self._position < self._input_len and
                   self._is_digit(self._input[self._position])):
                self._position += 1

            # Check for fractional part
            if (self._position < self._input_len and
                    self._input[self._position] == '.'):
                self._position += 1
                while (self._position < self._input_len and
                       self._is_digit(self._input[self._position])):
                    self._position += 1

            # Check for exponent (e or E)
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

    def _read_identifier_or_keyword(self) -> None:
        """
        Read an identifier, keyword, or boolean literal.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] == '_')):
            self._position += 1

        value = self._input[start:self._position]

        # Check if it's a boolean literal
        if value in self._BOOLEANS:
            self._tokens.append(Token(type=TokenType.BOOLEAN, value=value, start=start))
            return

        # Check if it's a keyword
        if value in self._KEYWORDS:
            self._tokens.append(Token(type=TokenType.KEYWORD, value=value, start=start))
            return

        # It's an identifier
        self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))
