from typing import Callable

from humbug.syntax.lexer import Lexer, Token


class MarkdownLexer(Lexer):
    def __init__(self, input_text: str):
        super().__init__(input_text)

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        :param ch: The start character
        :return: The lexing function
        """
        if ch == '\n':
            return self._read_newline

        if self._is_whitespace(ch):
            return self._read_whitespace

        if ch == '`':
            return self._read_backtick

        return self._read_text

    def _read_text(self) -> None:
        start: int = self._position
        self._position += 1
        while self._position < len(self._input) and self._input[self._position] != '\n':
            self._position += 1

        text_value = self._input[start:self._position]
        self._tokens.append(Token(type='TEXT', value=text_value))

    def _read_backtick(self) -> None:
        self._position += 1
        self._tokens.append(Token(type='BACKTICK', value='`'))
