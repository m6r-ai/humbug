from collections.abc import Callable
from dataclasses import dataclass

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class BashLexerState(LexerState):
    """
    State information for the Bash lexer.

    Attributes:
        in_heredoc: Whether we are consuming heredoc body lines.
        heredoc_delimiter: The delimiter that terminates the active heredoc.
        heredoc_quoted: Whether the heredoc delimiter was quoted (no expansion).
    """
    in_heredoc: bool = False
    heredoc_delimiter: str = ""
    heredoc_quoted: bool = False


class BashLexer(Lexer):
    """
    Lexer for shell scripts (Bash, sh, Zsh, fish).

    Handles comments, strings (single/double/dollar-quoted), variables,
    heredocs, keywords, built-in commands, operators, and numbers.
    """

    _OPERATORS = [
        '&&', '||', ';;', '<<-', '<<', '>>', '>&', '<&', '2>&1', '1>&2',
        '<>', '>', '<', '|', '&', ';', '(', ')', '{', '}', '=', '$',
        '!', '*', '?', '+', '-', '/', '%', ':', ',', '.', '~', '^', '@'
    ]

    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    _KEYWORDS = {
        'if', 'then', 'elif', 'else', 'fi',
        'for', 'while', 'until', 'do', 'done',
        'case', 'esac', 'in',
        'function', 'select', 'time',
        'return', 'break', 'continue', 'exit',
    }

    _COMMANDS = {
        'alias', 'bg', 'bind', 'builtin', 'caller', 'cd', 'command',
        'compgen', 'complete', 'compopt', 'declare', 'dirs', 'disown',
        'echo', 'enable', 'eval', 'exec', 'export', 'false', 'fc',
        'fg', 'getopts', 'hash', 'help', 'history', 'jobs', 'kill',
        'let', 'local', 'logout', 'mapfile', 'popd', 'printf',
        'pushd', 'pwd', 'read', 'readarray', 'readonly', 'set',
        'shift', 'shopt', 'source', 'suspend', 'test', 'times',
        'trap', 'true', 'type', 'typeset', 'ulimit', 'umask',
        'unalias', 'unset', 'wait',
        # Common external commands
        'awk', 'cat', 'chmod', 'chown', 'cp', 'curl', 'cut',
        'diff', 'du', 'df', 'find', 'grep', 'gzip', 'head', 'install',
        'less', 'ln', 'ls', 'make', 'man', 'mkdir', 'mktemp', 'mv',
        'nl', 'paste', 'pip', 'python', 'rm', 'rmdir', 'rsync',
        'sed', 'seq', 'sleep', 'sort', 'ssh', 'tail', 'tar', 'tee',
        'telnet', 'touch', 'tr', 'uniq', 'wc', 'wget', 'xargs',
    }

    _BOOLEANS = {'true', 'false'}

    def __init__(self) -> None:
        super().__init__()
        self._in_heredoc = False
        self._heredoc_delimiter = ""
        self._heredoc_quoted = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> BashLexerState:
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

        if prev_lexer_state is not None:
            assert isinstance(prev_lexer_state, BashLexerState), \
                f"Expected BashLexerState, got {type(prev_lexer_state).__name__}"

            self._in_heredoc = prev_lexer_state.in_heredoc
            self._heredoc_delimiter = prev_lexer_state.heredoc_delimiter
            self._heredoc_quoted = prev_lexer_state.heredoc_quoted

        if self._in_heredoc:
            self._continue_heredoc()

        if not self._in_heredoc:
            self._inner_lex()

        lexer_state = BashLexerState()
        lexer_state.in_heredoc = self._in_heredoc
        lexer_state.heredoc_delimiter = self._heredoc_delimiter
        lexer_state.heredoc_quoted = self._heredoc_quoted
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

        if ch == '#':
            return self._read_comment

        if ch == '\\' and self._position + 1 < self._input_len:
            return self._read_escape

        if ch in ('"', "'"):
            return self._read_string

        if ch == '`':
            return self._read_backtick

        if ch == '$':
            return self._read_dollar

        if self._is_digit(ch):
            return self._read_number

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        return self._read_operator

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

    def _read_escape(self) -> None:
        """
        Read a backslash escape sequence as a single OPERATOR token.

        In shell, ``\\<newline>`` is a line continuation, and ``\\c`` escapes
        the next character. We consume both characters as one token.
        """
        start = self._position
        self._position += 2
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_string(self) -> None:
        """
        Read a single- or double-quoted string.

        Handles escape sequences inside double-quoted strings. Single-quoted
        strings are literal (no escapes).
        """
        quote = self._input[self._position]
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\' and quote == '"' and self._position + 1 < self._input_len:
                self._position += 2
                continue

            if ch == quote:
                self._position += 1
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_backtick(self) -> None:
        """
        Read a backtick command substitution string.

        Handles escaped backticks inside the string.
        """
        start = self._position
        self._position += 1

        while self._position < self._input_len:
            ch = self._input[self._position]

            if ch == '\\' and self._position + 1 < self._input_len:
                self._position += 2
                continue

            if ch == '`':
                self._position += 1
                break

            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_dollar(self) -> None:
        """
        Read a dollar-sign construct: variable, command substitution,
        arithmetic, or brace expansion.
        """
        start = self._position

        if self._position + 1 >= self._input_len:
            self._read_operator()
            return

        next_ch = self._input[self._position + 1]

        # ${...} - brace-enclosed variable
        if next_ch == '{':
            self._position += 2
            depth = 1
            while self._position < self._input_len and depth > 0:
                ch = self._input[self._position]
                if ch == '{':
                    depth += 1

                elif ch == '}':
                    depth -= 1
                    if depth == 0:
                        self._position += 1
                        break

                self._position += 1

            self._tokens.append(Token(
                type=TokenType.IDENTIFIER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # $(...) - command substitution
        if next_ch == '(':
            if (self._position + 2 < self._input_len and
                    self._input[self._position + 2] == '('):
                # $((...)) - arithmetic expansion
                self._position += 3
                depth = 2

            else:
                self._position += 2
                depth = 1

            while self._position < self._input_len and depth > 0:
                ch = self._input[self._position]
                if ch == '(':
                    depth += 1

                elif ch == ')':
                    depth -= 1
                    if depth == 0:
                        self._position += 1
                        break

                self._position += 1

            self._tokens.append(Token(
                type=TokenType.STRING,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # $"..." - locale-translated string
        if next_ch in ('"', "'"):
            self._position += 2
            quote = next_ch
            while self._position < self._input_len:
                ch = self._input[self._position]
                if ch == '\\' and quote == '"' and self._position + 1 < self._input_len:
                    self._position += 2
                    continue

                if ch == quote:
                    self._position += 1
                    break

                self._position += 1

            self._tokens.append(Token(
                type=TokenType.STRING,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Special parameters: $?, $@, $#, $$, $!, $-, $*, $0-$9
        if next_ch in '?@#!$-*':
            self._position += 2
            self._tokens.append(Token(
                type=TokenType.IDENTIFIER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # $VARNAME - simple variable reference
        if self._is_letter(next_ch) or next_ch == '_':
            self._position += 2
            while (self._position < self._input_len and
                   (self._is_letter_or_digit_or_underscore(self._input[self._position]))):
                self._position += 1

            self._tokens.append(Token(
                type=TokenType.IDENTIFIER,
                value=self._input[start:self._position],
                start=start
            ))
            return

        # Just a bare $
        self._read_operator()

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
        Read an identifier, keyword, boolean, or built-in command.

        If a heredoc redirect (``<<`` or ``<<-``) was just emitted, the
        next word is treated as the heredoc delimiter and activates heredoc
        mode for subsequent lines.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] in '-.')):
            self._position += 1

        value = self._input[start:self._position]

        if value in self._KEYWORDS:
            self._tokens.append(Token(type=TokenType.KEYWORD, value=value, start=start))
            return

        if value in self._BOOLEANS:
            self._tokens.append(Token(type=TokenType.BOOLEAN, value=value, start=start))
            return

        if value in self._COMMANDS:
            self._tokens.append(Token(type=TokenType.COMMAND, value=value, start=start))
            return

        self._tokens.append(Token(type=TokenType.IDENTIFIER, value=value, start=start))

    def _read_operator(self) -> None:
        """
        Read an operator, possibly triggering heredoc mode.

        When ``<<`` or ``<<-`` is detected, scan forward past whitespace to
        capture the heredoc delimiter (which may be quoted, indicating a
        non-expanding heredoc).
        """
        first_char = self._input[self._position]
        potential_operators = self._OPERATORS_MAP.get(first_char, [])

        for op in potential_operators:
            if self._input[self._position:].startswith(op):
                start = self._position
                self._position += len(op)
                token_type = TokenType.LPAREN if op == '(' else (
                    TokenType.RPAREN if op == ')' else TokenType.OPERATOR
                )
                token = Token(type=token_type, value=op, start=start)
                self._tokens.append(token)

                if op in ('<<', '<<-'):
                    self._initiate_heredoc()

                return

        start = self._position
        ch = self._input[self._position]
        self._position += 1
        self._tokens.append(Token(type=TokenType.OPERATOR, value=ch, start=start))

    def _initiate_heredoc(self) -> None:
        """
        After seeing ``<<`` or ``<<-``, parse the heredoc delimiter.

        Skips whitespace, then reads the delimiter word. If the delimiter
        is quoted, the heredoc body will not undergo expansion. Sets the
        heredoc state fields so subsequent lines are consumed as body text.
        """
        save_pos = self._position

        while (self._position < self._input_len and
               self._is_whitespace(self._input[self._position])):
            self._position += 1

        if self._position >= self._input_len:
            self._position = save_pos
            return

        delimiter = ""
        quoted = False

        if self._input[self._position] in ('"', "'"):
            quote_char = self._input[self._position]
            quoted = True
            self._position += 1

            while (self._position < self._input_len and
                   self._input[self._position] != quote_char):
                delimiter += self._input[self._position]
                self._position += 1

            if self._position < self._input_len:
                self._position += 1

        else:
            while (self._position < self._input_len and
                   (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                    self._input[self._position] in '-.')):
                delimiter += self._input[self._position]
                self._position += 1

        if not delimiter:
            self._position = save_pos
            return

        self._in_heredoc = True
        self._heredoc_delimiter = delimiter
        self._heredoc_quoted = quoted

        self._tokens.append(Token(
            type=TokenType.IDENTIFIER,
            value=self._input[save_pos:self._position],
            start=save_pos
        ))

        # Consume any trailing content on the same line (the body starts next line)
        self._position = self._input_len

    def _continue_heredoc(self) -> None:
        """
        Consume a heredoc body line.

        If the line (after optional leading whitespace for ``<<-`` heredocs)
        matches the delimiter, heredoc mode ends and remaining content on the
        line is lexed normally. Otherwise the entire line is emitted as a
        STRING token.
        """
        stripped = self._input.lstrip('\t ')

        if stripped == self._heredoc_delimiter or stripped.rstrip() == self._heredoc_delimiter:
            leading_ws = len(self._input) - len(stripped)
            delimiter_len = len(self._heredoc_delimiter)

            self._tokens.append(Token(
                type=TokenType.STRING,
                value=self._input[:leading_ws + delimiter_len],
                start=0
            ))

            self._position = leading_ws + delimiter_len
            self._in_heredoc = False
            self._heredoc_delimiter = ""
            self._heredoc_quoted = False

            if self._position < self._input_len:
                self._inner_lex()

            return

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input,
            start=0
        ))
        self._position = self._input_len
