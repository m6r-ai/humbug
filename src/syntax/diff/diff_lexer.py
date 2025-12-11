"""
Diff/Patch Lexer

This module implements a lexer for diff/patch files, supporting multiple formats:
- Unified diff (diff -u)
- Context diff (diff -c)
- Normal diff (default diff)
- Git-enhanced diff
"""

from dataclasses import dataclass
from typing import Callable

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class DiffLexerState(LexerState):
    """
    State information for the Diff lexer.

    Since diff files are line-oriented and each line can be independently
    classified by its starting characters, no state needs to be carried
    between lines.
    """


class DiffLexer(Lexer):
    """
    Lexer for diff/patch files.

    This lexer handles diff-specific syntax including file headers, hunk headers,
    added/removed/changed lines, and metadata. It supports unified, context, normal,
    and git-enhanced diff formats.

    The lexer is stateless - each line is independently classified based on its
    starting characters/patterns.
    """

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> DiffLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state (unused for diff)
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing
        """
        self._input = input_str
        self._input_len = len(input_str)
        self._inner_lex()
        return DiffLexerState()

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Diff files are line-oriented, so we dispatch based on the first
        character(s) of the line.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if ch == '@':
            return self._read_unified_hunk_header

        if ch == '+':
            return self._read_plus_line

        if ch == '-':
            return self._read_minus_line

        if ch == '*':
            return self._read_star_line

        if ch == '!':
            return self._read_changed_line

        if ch == '<':
            return self._read_removed_line

        if ch == '>':
            return self._read_added_line

        if ch == ' ':
            return self._read_context_line

        if self._is_digit(ch):
            return self._read_normal_command_or_text

        # Check for git/metadata keywords
        if self._input.startswith('diff '):
            return self._read_metadata_line

        if self._input.startswith('index '):
            return self._read_metadata_line

        if self._input.startswith('new file mode'):
            return self._read_metadata_line

        if self._input.startswith('deleted file mode'):
            return self._read_metadata_line

        if self._input.startswith('similarity index'):
            return self._read_metadata_line

        if self._input.startswith('rename from'):
            return self._read_metadata_line

        if self._input.startswith('rename to'):
            return self._read_metadata_line

        if self._input.startswith('Binary files'):
            return self._read_metadata_line

        # Default: treat as text/context
        return self._read_text_line

    def _read_unified_hunk_header(self) -> None:
        """
        Read a unified diff hunk header.

        Format: @@ -start,count +start,count @@ optional context
        """
        start = self._position

        # Check if it's really a hunk header (should have two @@)
        if self._input.startswith('@@'):
            # It's a hunk header
            self._position = self._input_len
            self._tokens.append(Token(
                type=TokenType.DIFF_HEADING,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Not a hunk header, treat as text
        self._read_text_line()

    def _read_plus_line(self) -> None:
        """
        Read a line starting with '+'.

        Could be:
        - +++ file header (unified/git diff)
        - + added line (unified/context diff)
        """
        start = self._position

        if self._input.startswith('+++'):
            # File header
            self._position = self._input_len
            self._tokens.append(Token(
                type=TokenType.DIFF_METADATA,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Added line
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.DIFF_ADDED,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_minus_line(self) -> None:
        """
        Read a line starting with '-'.

        Could be:
        - --- file header (unified/context diff)
        - --- separator (normal diff within change hunk)
        - --- range marker (context diff)
        - - removed line (unified/context diff)
        """
        start = self._position

        if self._input.startswith('---'):
            # Need to distinguish between file header and separator
            # File header: "--- filename" (has content after ---)
            # Separator: "---" (alone or with trailing spaces in normal diff)
            # Range marker: "--- N,M ----" (context diff)

            # Check for context diff range marker
            if ' ----' in self._input or self._input.endswith('----'):
                self._position = self._input_len
                self._tokens.append(Token(
                    type=TokenType.DIFF_HEADING,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            # Check if there's a filename (file header) or just separator
            rest = self._input[3:].lstrip()
            if rest and not rest.isspace():
                # File header (has content after ---)
                self._position = self._input_len
                self._tokens.append(Token(
                    type=TokenType.DIFF_METADATA,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            # Separator line (normal diff)
            self._position = self._input_len
            self._tokens.append(Token(
                type=TokenType.DIFF_HEADING,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Removed line
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.DIFF_REMOVED,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_star_line(self) -> None:
        """
        Read a line starting with '*'.

        Could be:
        - *** file header (context diff)
        - *************** separator (context diff)
        - *** N,M **** range marker (context diff)
        - Regular text
        """
        start = self._position

        if self._input.startswith('***************'):
            # Separator line
            self._position = self._input_len
            self._tokens.append(Token(
                type=TokenType.DIFF_HEADING,
                value=self._input[start:self._position],
                start=start
            ))
            return

        if self._input.startswith('***'):
            # Could be file header or range marker
            # Range marker: "*** N,M ****"
            # File header: "*** filename"

            if ' ****' in self._input or self._input.endswith('****'):
                # Range marker
                self._position = self._input_len
                self._tokens.append(Token(
                    type=TokenType.DIFF_HEADING,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            # File header
            self._position = self._input_len
            self._tokens.append(Token(
                type=TokenType.DIFF_METADATA,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Regular text
        self._read_text_line()

    def _read_changed_line(self) -> None:
        """
        Read a line starting with '!'.

        This indicates a changed line in context diff format.
        """
        start = self._position
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.DIFF_CHANGED,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_removed_line(self) -> None:
        """
        Read a line starting with '<'.

        This indicates a removed/old line in normal diff format.
        """
        start = self._position
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.DIFF_REMOVED,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_added_line(self) -> None:
        """
        Read a line starting with '>'.

        This indicates an added/new line in normal diff format.
        """
        start = self._position
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.DIFF_ADDED,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_context_line(self) -> None:
        """
        Read a line starting with a space.

        This indicates a context line (unchanged) in unified or context diff.
        """
        start = self._position
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.TEXT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_normal_command_or_text(self) -> None:
        """
        Read a line starting with a digit.

        Could be a normal diff command (e.g., "5,7d3", "8a10", "12c15")
        or just regular text.
        """
        start = self._position

        # Try to match normal diff command pattern: N(,N)?[adc]N(,N)?
        # Examples: 5d3, 5,7d3, 8a10, 8a10,12, 12c15, 12,14c15,17
        pos = self._position

        # Read first number or range
        while pos < self._input_len and self._is_digit(self._input[pos]):
            pos += 1

        if pos < self._input_len and self._input[pos] == ',':
            pos += 1
            while pos < self._input_len and self._is_digit(self._input[pos]):
                pos += 1

        # Check for command character
        if pos < self._input_len and self._input[pos] in ('a', 'd', 'c'):
            pos += 1

            # Read second number or range
            while pos < self._input_len and self._is_digit(self._input[pos]):
                pos += 1

            if pos < self._input_len and self._input[pos] == ',':
                pos += 1
                while pos < self._input_len and self._is_digit(self._input[pos]):
                    pos += 1

            # If we've consumed most/all of the line, it's a command
            remaining = self._input[pos:].strip()
            if not remaining or remaining.isspace():
                self._position = self._input_len
                self._tokens.append(Token(
                    type=TokenType.DIFF_HEADING,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        # Not a command, treat as text
        self._read_text_line()

    def _read_metadata_line(self) -> None:
        """
        Read a metadata line (git diff metadata, etc.).

        Examples:
        - diff --git a/file b/file
        - index abc123..def456 100644
        - new file mode 100644
        - Binary files differ
        """
        start = self._position
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.KEYWORD,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_text_line(self) -> None:
        """
        Read a line as plain text.

        This is the fallback for lines that don't match any specific pattern.
        """
        start = self._position
        self._position = self._input_len
        self._tokens.append(Token(
            type=TokenType.TEXT,
            value=self._input[start:self._position],
            start=start
        ))
