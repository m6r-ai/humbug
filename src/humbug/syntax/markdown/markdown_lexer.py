from typing import Callable, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


class MarkdownLexer(Lexer):
    """
    Lexer for Markdown content.

    This lexer handles Markdown-specific syntax including code blocks,
    headings, blockquotes, lists, and inline formatting.
    """

    def __init__(self):
        super().__init__()
        self._first_token = True

    def lex(self, _prev_lexer_state: Optional[LexerState], input_str: str) -> LexerState:
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
        self._first_token = True
        self._inner_lex()
        return None

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

        # Check for block elements only if this is the first non-whitespace token on the line
        # Note: Since newlines are stripped, we're only checking first_token flag
        if self._first_token:
            # First check for indentation
            # Now check for block elements
            if ch == '>':
                return self._read_blockquote

            if ch == '#':
                return self._read_heading

            if ch in ('*', '-', '+'):
                # Check if this might be a list marker
                if ((self._position + 1) < self._input_len and
                    self._is_whitespace(self._input[self._position + 1])):
                    return self._read_unordered_list

            if self._is_digit(ch):
                # Check if this might be an ordered list marker
                return self._read_ordered_list

            # If we get here, it's not a block element, so mark that we've seen the first token
            self._first_token = False

        if ch == '`':
            return self._read_backtick

        return self._read_text

    def _read_blockquote(self) -> None:
        """
        Read a blockquote token.
        """
        start = self._position
        self._position += 1  # Skip the > character

        # Skip one whitespace after > if present
        if self._position < self._input_len and self._is_whitespace(self._input[self._position]):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.BLOCKQUOTE,
            value=">",
            start=start
        ))

    def _read_heading(self) -> None:
        """
        Read a heading token.
        """
        start = self._position
        level = 0

        # Count # characters (up to 6)
        while (self._position < self._input_len and
               self._input[self._position] == '#' and
               level < 6):
            level += 1
            self._position += 1

        # A heading must be followed by a space to be valid
        if self._position < self._input_len and self._is_whitespace(self._input[self._position]):
            # Skip the whitespace
            self._position += 1

            self._tokens.append(Token(
                type=TokenType.HEADING,
                value="#" * level,
                start=start
            ))
        else:
            # Not a valid heading, treat as text
            self._position = start
            self._read_text()

    def _read_unordered_list(self) -> None:
        """
        Read an unordered list item token.
        """
        start = self._position
        marker = self._input[self._position]
        self._position += 1  # Skip the marker character (* - +)

        # Skip the whitespace after the marker
        if self._position < self._input_len and self._is_whitespace(self._input[self._position]):
            self._position += 1

            self._tokens.append(Token(
                type=TokenType.LIST_MARKER,
                value=marker,
                start=start
            ))
        else:
            # Not a valid list marker, treat as text
            self._position = start
            self._read_text()

    def _read_ordered_list(self) -> None:
        """
        Check if the current position contains an ordered list marker.
        If yes, read it; otherwise read as text.
        """
        # Save the current position to backtrack if needed
        start_pos = self._position

        # Read digits
        while (self._position < self._input_len and
               self._is_digit(self._input[self._position])):
            self._position += 1

        # Check for . or ) followed by whitespace
        if (self._position < self._input_len and
                self._input[self._position] in ('.', ')') and
                (self._position + 1) < self._input_len and
                self._is_whitespace(self._input[self._position + 1])):
            # It's a valid ordered list marker
            self._position += 1  # Move past . or )
            marker_value = self._input[start_pos:self._position]

            # Skip the whitespace after the marker
            self._position += 1

            self._tokens.append(Token(
                type=TokenType.LIST_MARKER,
                value=marker_value,
                start=start_pos
            ))
        else:
            # Not a valid ordered list marker, reset and read as text
            self._position = start_pos
            self._read_text()

    def _read_text(self) -> None:
        """
        Read a text token.
        """
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '`':
                break

            if (not self._is_letter_or_digit_or_underscore(ch)) and (ch != '-') and (ch != '+'):
                break

            self._position += 1

        text_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.TEXT, value=text_value, start=start))

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
            return

        # Single backtick - could be inline code
        self._position += 1
        self._tokens.append(Token(type=TokenType.BACKTICK, value='`', start=start))

    def _read_whitespace(self) -> None:
        """
        Read whitespace characters.
        """
        self._position += 1

        # Skip consecutive whitespace
        while (self._position < self._input_len and
               self._is_whitespace(self._input[self._position])):
            self._position += 1
