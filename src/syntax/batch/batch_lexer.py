from collections.abc import Callable
from dataclasses import dataclass

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class BatchLexerState(LexerState):
    """
    State information for the Batch lexer.

    Batch has no multi-line string constructs, so no cross-line state is
    needed beyond what the base ``LexerState`` provides.
    """


class BatchLexer(Lexer):
    """
    Lexer for Windows Batch scripts (.bat, .cmd).

    Handles case-insensitive keywords, REM/:: comments, labels, percent-sign
    variables (``%VAR%``, ``!VAR!``, ``%0``-``%9``, ``%%a``), command-line
    switches (``/S``), strings, numbers, and operators.
    """

    _OPERATORS = [
        '&&', '||', '>>', '2>&1', '1>&2',
        '>', '<', '|', '&', '=', '(', ')', '+', '-', '*', '/', '%', ','
    ]

    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    _KEYWORDS = {
        'if', 'else', 'for', 'in', 'do', 'goto', 'call', 'exit',
        'not', 'exist', 'defined', 'errorlevel', 'cmdextversion',
        'neq', 'equ', 'lss', 'leq', 'gtr', 'geq',
    }

    _COMMANDS = {
        'echo', 'set', 'setlocal', 'endlocal', 'shift', 'cd', 'chdir',
        'cls', 'copy', 'del', 'erase', 'dir', 'md', 'mkdir', 'move',
        'rd', 'rmdir', 'ren', 'rename', 'type', 'start', 'title',
        'color', 'prompt', 'pushd', 'popd', 'path', 'assoc', 'ftype',
        'attrib', 'break', 'cacls', 'chkntfs', 'cmd', 'comp', 'compact',
        'convert', 'date', 'diskpart', 'doskey', 'fc', 'find', 'findstr',
        'format', 'fsutil', 'ftp', 'graftabl', 'help', 'icacls',
        'label', 'mode', 'more', 'openfiles', 'powercfg', 'print',
        'recover', 'replace', 'robocopy', 'sc', 'schtasks', 'shutdown',
        'sort', 'subst', 'systeminfo', 'taskkill', 'tasklist', 'timeout',
        'tree', 'ver', 'verify', 'vol', 'xcopy', 'where', 'wmic',
        'choice', 'cmdkey', 'bitsadmin',
    }

    _BOOLEANS = {'true', 'false'}

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> BatchLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state
        """
        self._input = input_str
        self._input_len = len(input_str)
        self._position = 0
        self._tokens = []
        self._next_token = 0

        self._inner_lex()

        return BatchLexerState()

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

        if ch == ':':
            return self._read_colon

        if ch == '%':
            return self._read_percent

        if ch == '!':
            return self._read_bang

        if ch == '@':
            return self._read_at

        if ch == '"':
            return self._read_string

        if self._is_digit(ch):
            return self._read_number

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if ch == '/':
            return self._read_slash

        return self._read_operator

    def _read_colon(self) -> None:
        """
        Read a label (``:label``) or a double-colon comment (``:: ...``).

        At line start, ``::`` is a comment and ``:word`` is a label.
        Otherwise, ``:`` is an operator (used in variable slicing, etc.).
        """
        is_line_start = all(self._is_whitespace(c) for c in self._input[:self._position])

        if not is_line_start:
            self._read_operator()
            return

        if self._position + 1 < self._input_len and self._input[self._position + 1] == ':':
            start = self._position
            self._position = self._input_len
            self._tokens.append(Token(
                type=TokenType.COMMENT,
                value=self._input[start:self._position],
                start=start
            ))
            return

        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] in '-_.')):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.DIRECTIVE,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_percent(self) -> None:
        """
        Read a percent-sign variable: ``%VAR%``, ``%%a``, ``%0``-``%9``.
        """
        start = self._position

        if self._position + 1 >= self._input_len:
            self._read_operator()
            return

        next_ch = self._input[self._position + 1]

        # %% - escaped percent (used in FOR loops)
        if next_ch == '%':
            if self._position + 2 < self._input_len:
                after_percent = self._input[self._position + 2]
                if (self._is_letter(after_percent) or after_percent == '_' or
                        self._is_digit(after_percent)):
                    self._position += 3
                    while (self._position < self._input_len and
                           (self._is_letter_or_digit_or_underscore(
                               self._input[self._position]))):
                        self._position += 1

                    self._tokens.append(Token(
                        type=TokenType.IDENTIFIER,
                        value=self._input[start:self._position],
                        start=start
                    ))
                    return

            self._position += 2
            self._tokens.append(Token(
                type=TokenType.OPERATOR,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # %0-%9 - positional parameters
        if self._is_digit(next_ch):
            self._position += 2
            self._tokens.append(Token(
                type=TokenType.IDENTIFIER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # %VAR% - environment variable
        if self._is_letter(next_ch) or next_ch == '_':
            self._position += 2
            while (self._position < self._input_len and
                   self._input[self._position] != '%'):
                self._position += 1

            if self._position < self._input_len:
                self._position += 1  # closing %

            self._tokens.append(Token(
                type=TokenType.IDENTIFIER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Bare % (e.g. %~dp0, %cd%, etc. — let bare % be an operator)
        self._read_operator()

    def _read_bang(self) -> None:
        """
        Read delayed expansion variable ``!VAR!`` or operator ``!``.
        """
        if self._position + 1 < self._input_len:
            next_ch = self._input[self._position + 1]
            if self._is_letter(next_ch) or next_ch == '_':
                start = self._position
                self._position += 2

                while (self._position < self._input_len and
                       self._input[self._position] != '!'):
                    self._position += 1

                if self._position < self._input_len:
                    self._position += 1  # closing !

                self._tokens.append(Token(
                    type=TokenType.IDENTIFIER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        self._read_operator()

    def _read_at(self) -> None:
        """
        Read ``@`` prefix (e.g. ``@echo off``) as a single operator token.
        """
        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_slash(self) -> None:
        """
        Read a command-line switch (``/S``, ``/Q``, ``/F:tokens=1``) or
        a ``/`` operator.
        """
        if self._position + 1 < self._input_len:
            next_ch = self._input[self._position + 1]
            if self._is_letter(next_ch):
                start = self._position
                self._position += 2

                while (self._position < self._input_len and
                       (self._is_letter_or_digit(self._input[self._position]) or
                        self._input[self._position] in ':=')):
                    self._position += 1

                self._tokens.append(Token(
                    type=TokenType.OPTION,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        self._read_operator()

    def _read_string(self) -> None:
        """
        Read a double-quoted string.
        """
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '"':
                self._position += 1

                # Batch allows "" as escaped quote inside strings
                if (self._position < self._input_len and
                        self._input[self._position] == '"'):
                    self._position += 1
                    continue

                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.
        """
        start = self._position
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
        Read an identifier, keyword, boolean, or command.

        Matching is case-insensitive: the extracted word is lowercased for
        keyword/command lookup, but the original casing is preserved in the
        token value.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] in '-.')):
            self._position += 1

        value = self._input[start:self._position]
        lower = value.lower()

        if lower in self._KEYWORDS:
            self._tokens.append(Token(type=TokenType.KEYWORD, value=value, start=start))
            return

        if lower in self._BOOLEANS:
            self._tokens.append(Token(type=TokenType.BOOLEAN, value=value, start=start))
            return

        if lower in self._COMMANDS:
            self._tokens.append(Token(type=TokenType.COMMAND, value=value, start=start))
            return

        self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))
