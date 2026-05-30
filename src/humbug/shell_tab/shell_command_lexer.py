"""
Command line lexer for tokenizing system command input.

This lexer handles command syntax including command names, options, and arguments.
"""

from typing import Callable, Set

from syntax import Lexer, LexerState, Token, TokenType


class ShellCommandLexer(Lexer):
    """
    Lexer for command line input.

    This lexer handles command-specific syntax including command names, options,
    and arguments. It doesn't attempt to distinguish between option values
    and regular arguments, as that requires semantic knowledge beyond lexical analysis.
    """

    # Option prefix characters
    _OPTION_PREFIX_CHARS: Set[str] = {'-'}

    def __init__(self) -> None:
        super().__init__()
        self._in_quotes = False
        self._quote_char = ""
        self._current_token_start = 0
        self._is_first_token = True

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> None:
        """
        Lex the command line input.

        Args:
            input_str: The input string to parse
        """
        self._input = input_str
        self._input_len = len(input_str)
        self._position = 0
        self._tokens = []
        self._next_token = 0
        self._is_first_token = True

        self._inner_lex()

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given character.

        Args:
            ch: The current character

        Returns:
            The appropriate lexing function for the character
        """
        if self._in_quotes:
            return self._read_quoted_text

        if ch in {'"', "'"}:
            return self._start_quoted_text

        if ch in self._WHITESPACE_CHARS:
            return self._read_whitespace

        if ch == '-':
            return self._read_option

        return self._read_word

    def _start_quoted_text(self) -> None:
        """
        Start reading a quoted text segment.
        """
        self._quote_char = self._input[self._position]
        self._in_quotes = True
        self._current_token_start = self._position
        self._position += 1

        # Continue reading the quoted text
        self._read_quoted_text()

    def _read_quoted_text(self) -> None:
        """
        Read a quoted text segment until the matching quote is found.
        """
        start = self._current_token_start

        while self._position < self._input_len:
            if self._input[self._position] == '\\' and self._position + 1 < self._input_len:
                # Handle escape sequence
                self._position += 2
                continue

            if self._input[self._position] == self._quote_char:
                # End of quoted text
                self._position += 1
                self._in_quotes = False

                # All non-command, non-option tokens are arguments
                token_type = TokenType.ARGUMENT
                self._tokens.append(Token(
                    type=token_type,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            self._position += 1

        # If we reach the end without finding the closing quote, the string is still open
        # We'll treat what we have as a token anyway but maintain the quote state
        if self._position > start:
            # All non-command, non-option tokens are arguments
            token_type = TokenType.ARGUMENT
            self._tokens.append(Token(
                type=token_type,
                value=self._input[start:self._position],
                start=start
            ))

    def _read_option(self) -> None:
        """
        Read a command-line option (like -f or --file).
        """
        start = self._position
        self._position += 1  # Skip the initial dash

        # Check for long option (double dash)
        if self._position < self._input_len and self._input[self._position] == '-':
            self._position += 1

        # Read option name
        while (self._position < self._input_len and
               not self._is_whitespace(self._input[self._position]) and
               self._input[self._position] not in {'=', '"', "'"}):
            self._position += 1

        option_value = self._input[start:self._position]

        # Add the option token
        self._tokens.append(Token(
            type=TokenType.OPTION,
            value=option_value,
            start=start
        ))

        # Check for equals sign indicating an inline option value
        if self._position < self._input_len and self._input[self._position] == '=':
            # Skip equals sign
            self._position += 1

            # Parse the value part
            value_start = self._position

            # If the value starts with a quote, handle quoted value
            if self._position < self._input_len and self._input[self._position] in {'"', "'"}:
                self._start_quoted_text()
                return

            # Otherwise read until whitespace
            while (self._position < self._input_len and
                   not self._is_whitespace(self._input[self._position])):
                self._position += 1

            if self._position > value_start:
                # Add as an argument
                self._tokens.append(Token(
                    type=TokenType.ARGUMENT,
                    value=self._input[value_start:self._position],
                    start=value_start
                ))

    def _read_word(self) -> None:
        """
        Read a word token, which could be a command name or an argument.
        """
        start = self._position

        while self._position < self._input_len:
            # Check for escape sequence
            if self._input[self._position] == '\\' and self._position + 1 < self._input_len:
                # Move past the backslash and the escaped character
                self._position += 2
                continue

            # Check for end of token
            if self._is_whitespace(self._input[self._position]):
                break

            self._position += 1

        if self._position > start:
            # First token is the command, everything else is an argument
            token_type = TokenType.COMMAND if self._is_first_token else TokenType.ARGUMENT

            self._tokens.append(Token(
                type=token_type,
                value=self._input[start:self._position],
                start=start
            ))

            self._is_first_token = False
