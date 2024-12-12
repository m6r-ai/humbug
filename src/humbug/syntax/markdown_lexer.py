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
        while self._position < len(self._input) and self._input[self._position] != '\n' and self._input[self._position] != '`':
            self._position += 1

        text_value = self._input[start:self._position]
        self._tokens.append(Token(type='TEXT', value=text_value, start=start))

    def _read_backtick(self) -> None:
        start: int = self._position

        # Do we have 3 backticks?  If yes, we have code fence and the next word after that
        # is the (optional) name of the language
        if self._input[self._position:].startswith('```'):
            self._position += 3

            # Capture optional language identifier
            lang_identifier = ''
            while self._position < len(self._input) and self._input[self._position].isspace():
                # Skip any whitespace after backticks
                self._position += 1

            # Start capturing the language identifier
            start_lang = self._position
            while (self._position < len(self._input) and
                   (self._input[self._position].isalnum() or self._input[self._position] in ('-', '_'))):
                self._position += 1

            lang_identifier = self._input[start_lang:self._position]
            self._tokens.append(Token(type='FENCE', value=lang_identifier, start=start))
            return

        self._position += 1
        self._tokens.append(Token(type='BACKTICK', value='`', start=start))
