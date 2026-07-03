from collections.abc import Callable
from dataclasses import dataclass

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class YAMLLexerState(LexerState):
    """
    State information for the YAML lexer.

    The YAML lexer is stateless across lines — all cross-line state
    (block scalars, indentation context) is managed by the parser.
    """


class YAMLLexer(Lexer):
    """
    Lexer for YAML.

    This lexer handles YAML-specific syntax including keys, values, strings,
    numbers, booleans, null, comments, anchors, aliases, tags, and flow-style
    constructs.  It is stateless across lines; the parser tracks block scalar
    state and other cross-line context.
    """

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> YAMLLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state (unused)
            input_str: The input string to parse

        Returns:
            The updated lexer state
        """
        self._input = input_str
        self._input_len = len(input_str)
        self._inner_lex()
        return YAMLLexerState()

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

        if ch in ('"', "'"):
            return self._read_string

        if ch == '-' or self._is_digit(ch):
            return self._read_number_or_dash

        if ch == '&':
            return self._read_anchor

        if ch == '*':
            return self._read_alias

        if ch == '!':
            return self._read_tag

        if ch in '{}[]:,':
            return self._read_punctuation

        if ch == '|':
            return self._read_block_scalar_indicator

        if ch == '>':
            return self._read_folded_scalar_indicator

        if ch == '.':
            return self._read_dot_or_document_marker

        return self._read_plain_scalar

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

    def _read_string(self) -> None:
        """
        Read a quoted string token.

        Handles both single-quoted and double-quoted YAML strings.
        """
        quote = self._input[self._position]
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\' and quote == '"' and (self._position + 1) < self._input_len:
                self._position += 2
                continue

            if ch == quote:
                if quote == "'" and (self._position + 1) < self._input_len and self._input[self._position + 1] == "'":
                    self._position += 2
                    continue

                self._position += 1
                break

            self._position += 1

        string_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.STRING, value=string_value, start=start))

    def _read_number_or_dash(self) -> None:
        """
        Read a number, a negative number, or a dash (list marker or document marker).

        A standalone '-' followed by whitespace is a list marker.
        '---' is a document start marker.
        '...' is a document end marker (handled by _read_dot_or_document_marker).
        """
        if self._input[self._position] == '-':
            remaining = self._input[self._position:]

            if remaining.startswith('---'):
                start = self._position
                self._position = self._input_len
                self._tokens.append(Token(
                    type=TokenType.DOC_COMMENT,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            if (len(remaining) < 2 or remaining[1] in (' ', '\t')) and not (
                len(remaining) >= 2 and self._is_digit(remaining[1])
            ):
                self._read_punctuation()
                return

        self._read_number()

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles integers, floats, negative numbers, hex, octal, binary,
        infinity, and NaN.
        """
        start = self._position

        if self._input[self._position] == '-':
            self._position += 1

        if (self._position + 1 < self._input_len and
                self._input[self._position] == '0'):
            next_ch = self._input[self._position + 1].lower()
            if next_ch == 'x':
                self._position += 2
                while (self._position < self._input_len and
                       self._is_hex_digit(self._input[self._position])):
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
                       self._input[self._position] in '01234567'):
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
                       self._input[self._position] in '01'):
                    self._position += 1

                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        if self._position < self._input_len:
            word = self._input[self._position:].lower()
            if word.startswith('.inf') or word.startswith('.nan'):
                self._position += 4
                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        self._read_decimal_number(start)

    def _read_decimal_number(self, start: int) -> None:
        """
        Read a decimal or floating-point number.

        Args:
            start: The starting position of the number
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

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_anchor(self) -> None:
        """
        Read an anchor token (&name).
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] in '-.')):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.HASH,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_alias(self) -> None:
        """
        Read an alias token (*name).
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] in '-.')):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.HASH,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_tag(self) -> None:
        """
        Read a tag token (e.g., !!str, !my_tag).
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               self._input[self._position] not in ' \t#:,\n\r'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.DIRECTIVE,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_punctuation(self) -> None:
        """
        Read a YAML punctuation token ({, }, [, ], :, or ,).
        """
        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_block_scalar_indicator(self) -> None:
        """
        Read a block scalar indicator (|, |-, |+, etc.).
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               self._input[self._position] in '-+0123456789'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_folded_scalar_indicator(self) -> None:
        """
        Read a folded scalar indicator (>, >-, >+, etc.).
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               self._input[self._position] in '-+0123456789'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_dot_or_document_marker(self) -> None:
        """
        Read a dot (number start) or document end marker (...).
        """
        if self._input[self._position:].startswith('...'):
            start = self._position
            self._position = self._input_len
            self._tokens.append(Token(
                type=TokenType.DOC_COMMENT,
                value=self._input[start:self._position],
                start=start
            ))
            return

        if (self._position + 1 < self._input_len and
                self._input[self._position + 1:].lower().startswith('inf')):
            start = self._position
            self._position += 4
            self._tokens.append(Token(
                type=TokenType.NUMBER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        if (self._position + 1 < self._input_len and
                self._input[self._position + 1:].lower().startswith('nan')):
            start = self._position
            self._position += 4
            self._tokens.append(Token(
                type=TokenType.NUMBER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        if (self._position + 1 < self._input_len and
                self._is_digit(self._input[self._position + 1])):
            start = self._position
            if self._input[self._position] == '-':
                self._position += 1

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

        self._read_plain_scalar()

    def _read_plain_scalar(self) -> None:
        """
        Read a plain scalar token.

        This handles unquoted text that is not a keyword.  Plain scalars
        can be map keys or bare values depending on context (determined
        by the parser).
        """
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]
            if ch in ':#{}[],\n\r':
                if ch == ':' and (self._position + 1 < self._input_len and
                                  self._input[self._position + 1] not in ' \t\n\r'):
                    self._position += 1
                    continue

                break

            if ch in (' ', '\t'):
                break

            self._position += 1

        value = self._input[start:self._position]
        token_type = self._classify_plain_scalar(value)
        self._tokens.append(Token(type=token_type, value=value, start=start))

    def _classify_plain_scalar(self, value: str) -> TokenType:
        """
        Classify a plain scalar as a keyword (true/false/null) or plain text.

        Args:
            value: The scalar value to classify

        Returns:
            KEYWORD if the value is a YAML boolean or null, TEXT otherwise
        """
        lower = value.lower()
        if lower in ('true', 'false', 'null'):
            return TokenType.KEYWORD

        if lower in ('yes', 'no', 'on', 'off', '~'):
            return TokenType.KEYWORD

        return TokenType.TEXT
