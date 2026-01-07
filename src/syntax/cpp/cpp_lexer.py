"""
C++ Lexer

This module implements a lexer for C++ code, extending the functionality of the C lexer.
"""
from typing import Callable

from syntax.c.c_lexer import CLexer
from syntax.lexer import Lexer, Token, TokenType



class CppLexer(CLexer):
    """
    Lexer for C++ code.

    This lexer extends the C lexer to handle C++-specific syntax including
    additional keywords and operators.
    """

    # Operators list
    _OPERATORS = [
        '>>=', '<<=', '&&=', '||=', '!=', '==', '+=', '-=', '*=',
        '/=', '%=', '&=', '|=', '^=', '<=', '>=', '&&', '||', '<<',
        '>>', '++', '--', '->', '::', '+', '-', '*', '/', '%', '&',
        '~', '!', '|', '^', '=', '<', '>', '(', ')', '{', '}', '[',
        ']', ';', ':', '?', '.', ','
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Overrides the C lexer to handle C++-specific features like raw strings.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if self._is_whitespace(ch):
            return self._read_whitespace

        if ch == 'L':
            return self._read_L

        if ch == 'u':
            return self._read_u

        if ch == 'U':
            return self._read_U

        if ch == 'R':
            return self._read_R

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch in ('"', "'"):
            return self._read_string

        if ch == '.':
            return self._read_dot

        if ch == '/':
            return self._read_forward_slash

        if ch == '#':
            return self._read_preprocessor_directive

        return self._read_operator

    def _read_L(self) -> None:  # pylint: disable=invalid-name
        """
        Read an L character, which could be the start of a wide string/character literal
        (L"..." or L'...'), a wide raw string (LR"(...)"), or an identifier.
        """
        if self._position + 1 < self._input_len:
            next_char = self._input[self._position + 1]

            # Check for LR"(...)" - wide raw string
            if next_char == 'R' and self._position + 2 < self._input_len:
                third_char = self._input[self._position + 2]
                if third_char == '"':
                    self._read_raw_string()
                    return

            # Check for L"..." or L'...'
            if next_char in ('"', "'"):
                self._read_string()
                return

        self._read_identifier_or_keyword()

    def _read_u(self) -> None:
        """
        Read a u character, which could be the start of a UTF-16 string/character literal
        (u"..." or u'...'), a UTF-8 string literal (u8"..." or u8'...'),
        a UTF-16 raw string (uR"(...)"), a UTF-8 raw string (u8R"(...)"), or an identifier.
        """
        if self._position + 1 < self._input_len:
            next_char = self._input[self._position + 1]

            # Check for u8"...", u8'...', or u8R"(...)"
            if next_char == '8' and self._position + 2 < self._input_len:
                third_char = self._input[self._position + 2]

                # Check for u8R"(...)" - UTF-8 raw string
                if third_char == 'R' and self._position + 3 < self._input_len:
                    fourth_char = self._input[self._position + 3]
                    if fourth_char == '"':
                        self._read_raw_string()
                        return

                # Check for u8"..." or u8'...'
                if third_char in ('"', "'"):
                    self._read_string()
                    return

            # Check for uR"(...)" - UTF-16 raw string
            if next_char == 'R' and self._position + 2 < self._input_len:
                third_char = self._input[self._position + 2]
                if third_char == '"':
                    self._read_raw_string()
                    return

            # Check for u"..." or u'...'
            if next_char in ('"', "'"):
                self._read_string()
                return

        self._read_identifier_or_keyword()

    def _read_U(self) -> None:  # pylint: disable=invalid-name
        """
        Read a U character, which could be the start of a UTF-32 string/character literal
        (U"..." or U'...'), a UTF-32 raw string (UR"(...)"), or an identifier.
        """
        if self._position + 1 < self._input_len:
            next_char = self._input[self._position + 1]

            # Check for UR"(...)" - UTF-32 raw string
            if next_char == 'R' and self._position + 2 < self._input_len:
                third_char = self._input[self._position + 2]
                if third_char == '"':
                    self._read_raw_string()
                    return

            # Check for U"..." or U'...'
            if next_char in ('"', "'"):
                self._read_string()
                return

        self._read_identifier_or_keyword()

    def _read_R(self) -> None:  # pylint: disable=invalid-name
        """
        Read an R character, which could be the start of a raw string literal
        (R"(...)") or an identifier.
        """
        if self._position + 1 < self._input_len and self._input[self._position + 1] == '"':
            self._read_raw_string()
            return

        self._read_identifier_or_keyword()

    def _read_raw_string(self) -> None:
        """
        Read a raw string literal token.

        Raw strings have the format: [prefix]R"delimiter(content)delimiter"
        Where:
        - prefix is optional: L, u8, u, or U
        - delimiter is optional and can be up to 16 characters
        - content can contain any characters including quotes and newlines
        - No escape sequences are processed
        """
        start = self._position

        # Skip over any prefix (L, u, U, u8)
        ch = self._input[self._position]
        if ch == 'L':
            self._position += 1
        elif ch == 'U':
            self._position += 1
        elif ch == 'u':
            self._position += 1
            # Check for u8 prefix
            if self._position < self._input_len and self._input[self._position] == '8':
                self._position += 1

        # Skip 'R'
        assert self._position < self._input_len and self._input[self._position] == 'R', "Expected 'R' for raw string"
        self._position += 1

        # Skip opening quote
        assert self._position < self._input_len and self._input[self._position] == '"', "Expected '\"' after R"
        self._position += 1

        # Read delimiter (between opening quote and opening paren)
        delimiter_start = self._position
        while self._position < self._input_len and self._input[self._position] != '(':
            self._position += 1

        delimiter = self._input[delimiter_start:self._position]

        # Skip opening paren
        if self._position < self._input_len and self._input[self._position] == '(':
            self._position += 1

        # Read content until we find: )delimiter"
        closing_sequence = ')' + delimiter + '"'
        while self._position < self._input_len:
            # Check if we've found the closing sequence
            if self._input[self._position:self._position + len(closing_sequence)] == closing_sequence:
                self._position += len(closing_sequence)
                break
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a C++ keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a C++ keyword, False otherwise
        """
        keywords = {
            'alignas', 'alignof', 'and', 'and_eq', 'asm',
            'atomic_cancel', 'atomic_commit', 'atomic_noexcept',
            'auto', 'bitand', 'bitor', 'bool', 'break', 'case',
            'catch', 'char', 'char16_t', 'char32_t', 'class',
            'compl', 'concept', 'const', 'const_cast', 'consteval',
            'constexpr', 'constinit', 'continue', 'co_await',
            'co_return', 'co_yield', 'decltype', 'default',
            'delete', 'do', 'double', 'dynamic_cast', 'else',
            'enum', 'explicit', 'export', 'extern', 'false',
            'final', 'float', 'for', 'friend', 'goto', 'if', 'inline',
            'int', 'long', 'mutable', 'namespace', 'new',
            'noexcept', 'not', 'not_eq', 'nullptr', 'operator', 'override',
            'or', 'or_eq', 'private', 'protected', 'public',
            'register', 'reinterpret_cast', 'requires', 'return',
            'short', 'signed', 'sizeof', 'static', 'static_assert',
            'static_cast', 'struct', 'switch', 'template', 'this',
            'thread_local', 'throw', 'true', 'try', 'typedef',
            'typeid', 'typename', 'union', 'unsigned', 'using',
            'virtual', 'void', 'volatile', 'wchar_t', 'while',
            'xor', 'xor_eq'
        }
        return value in keywords
