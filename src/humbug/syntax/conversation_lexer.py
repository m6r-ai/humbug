from typing import Callable, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


class ConversationLexer(Lexer):
    def lex(self, prev_lexer_state: Optional[LexerState], input_str: str) -> LexerState:
        """
        Lex all the tokens in the input.
        """
        self._input = input_str
        self._inner_lex()
        return None

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        :param ch: The start character
        :return: The lexing function
        """
        if self._is_letter_or_digit(ch):
            return self._read_text

        if self._is_whitespace(ch):
            return self._read_whitespace

        if ch == '`':
            return self._read_backtick

        if ch == '\n':
            return self._read_newline

        return self._read_symbol

    def _read_symbol(self) -> None:
        start: int = self._position
        self._position += 1
        text_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.SYMBOL, value=text_value, start=start))

    def _read_text(self) -> None:
        start: int = self._position
        self._position += 1
        while self._position < len(self._input):
            ch = self._input[self._position]
            if (not self._is_letter_or_digit_or_underscore(ch)) and (ch != '-') and (ch != '+'):
                break

            self._position += 1

        text_value = self._input[start:self._position]
        self._tokens.append(Token(type=TokenType.TEXT, value=text_value, start=start))

    def _read_backtick(self) -> None:
        start: int = self._position

        # Do we have 3 backticks?  If yes, we have code fence and the next word after that
        # is the (optional) name of the language
        if self._input[self._position:].startswith('```'):
            self._position += 3
            self._tokens.append(Token(type=TokenType.FENCE, value='```', start=start))
            return

        self._position += 1
        self._tokens.append(Token(type=TokenType.BACKTICK, value='`', start=start))
