from typing import Callable

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


class MarkdownLexer(Lexer):
    """
    Lexer for Markdown content.

    This lexer handles Markdown-specific syntax including code blocks,
    headings, blockquotes, lists, and inline formatting.
    """

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> None:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to lex

        Returns:
            Updated lexer state after lexing
        """
        self._input = input_str
        self._input_len = len(input_str)
        self._inner_lex()

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Args:
            ch: The start character

        Returns:
            The lexing function
        """
        if self._is_whitespace(ch):
            return self._read_whitespace

        # Check for block elements
        if ch == '>':
            return self._read_blockquote

        if ch == '#':
            return self._read_heading

        if ch in ('*', '-', '+'):
            return self._read_unordered_list

        if self._is_digit(ch):
            return self._read_ordered_list

        if ch == '|':
            return self._read_table

        if ch == '`':
            return self._read_backtick

        return self._read_text

    def _read_blockquote(self) -> None:
        """
        Read a blockquote token.
        """
        self._tokens.append(Token(
            type=TokenType.BLOCKQUOTE,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _read_heading(self) -> None:
        """
        Read a heading token.
        """
        self._tokens.append(Token(
            type=TokenType.HEADING,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _read_unordered_list(self) -> None:
        """
        Read an unordered list item token.
        """
        pos = self._position + 1

        # Do we have whitespace after the marker?  If yes, it's a valid list marker
        if pos < self._input_len and self._is_whitespace(self._input[pos]):
            self._tokens.append(Token(
                type=TokenType.LIST_MARKER,
                value=self._input[self._position:],
                start=self._position
            ))
            self._position = self._input_len
            return

        # Do we have 3 or more of the same marker?  If yes, it's a horizontal rule
        marker = self._input[self._position]
        if (pos + 1 < self._input_len and self._input[pos] == marker and self._input[pos + 1] == marker):
            self._tokens.append(Token(
                type=TokenType.HORIZONTAL_RULE,
                value=self._input[self._position:],
                start=self._position
            ))
            self._position = self._input_len
            return

        # Not a valid list marker, treat as text
        self._read_text()

    def _read_ordered_list(self) -> None:
        """
        Check if the current position contains an ordered list marker.
        If yes, read it; otherwise read as text.
        """
        pos = self._position + 1

        # Read digits
        while (pos < self._input_len and self._is_digit(self._input[pos])):
            pos += 1

        # Check for . or ) followed by whitespace
        if (pos < self._input_len and
                self._input[pos] in ('.', ')') and
                (pos + 1) < self._input_len and
                self._is_whitespace(self._input[pos + 1])):
            # It's a valid ordered list marker
            self._tokens.append(Token(
                type=TokenType.LIST_MARKER,
                value=self._input[self._position:],
                start=self._position
            ))
            self._position = self._input_len
            return

        # Not a valid ordered list marker, reset and read as text
        self._read_text()

    def _read_table(self) -> None:
        """
        Read a table token.
        """
        self._tokens.append(Token(
            type=TokenType.TABLE,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _read_text(self) -> None:
        """
        Read a text line.
        """
        self._tokens.append(Token(
            type=TokenType.TEXT,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _read_backtick(self) -> None:
        """
        Read a backtick token, which could be inline code or a code fence.
        """
        start = self._position

        # Do we have 3 backticks? If yes, we have code fence and the next word after that
        # is the (optional) name of the language
        if self._input[self._position:].startswith('```'):
            self._position += 3
            self._tokens.append(Token(type=TokenType.FENCE, value='```', start=start))
            if self._position < self._input_len:
                self._tokens.append(Token(type=TokenType.TEXT, value=self._input[self._position:], start=self._position))

            return

        # Not a code fence, so treat as text at this level
        self._read_text()

    def _read_whitespace(self) -> None:
        """
        Read whitespace characters.
        """
        self._position += 1

        # Skip consecutive whitespace
        while (self._position < self._input_len and
               self._is_whitespace(self._input[self._position])):
            self._position += 1
