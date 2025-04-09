from typing import Callable

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


class MetaphorLexer(Lexer):
    """
    Lexer for Metaphor language.

    This lexer handles Metaphor-specific syntax including keywords, headings,
    and text content.
    """

    def __init__(self) -> None:
        """
        Initialize the Metaphor lexer.
        """
        super().__init__()
        self._seen_keyword: bool = False

    def lex(self, _prev_lexer_state: LexerState | None, input_str: str) -> None:
        """
        Lex all the tokens in the input.
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
            The appropriate lexing function for the character
        """
        if self._is_whitespace(ch):
            return self._read_whitespace

        if ch == '`':
            return self._read_backtick

        if ch == '#':
            return self._read_comment

        return self._read_text_or_keyword

    def _read_text_or_keyword(self) -> None:
        """
        Read either a text token or a keyword token.

        Keywords are special strings that end with ':'. If a keyword is found,
        the remainder of the line is treated as a heading.
        """
        start = self._position

        while self._position < self._input_len:
            ch = self._input[self._position]
            self._position += 1

            # If we've already seen a keyword on this line then we don't need to
            # look for another one.
            if self._seen_keyword:
                continue

            # Is this a keyword?
            if ch == ':':
                potential_keyword = self._input[start:self._position]
                if self._is_keyword(potential_keyword):
                    self._seen_keyword = True
                    self._tokens.append(Token(
                        type=TokenType.KEYWORD,
                        value=potential_keyword,
                        start=start
                    ))
                    return

        token_type = TokenType.HEADING if self._seen_keyword else TokenType.TEXT
        self._tokens.append(Token(
            type=token_type,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_backtick(self) -> None:
        start: int = self._position

        # Do we have 3 backticks?  If yes, we have code fence and the next word after that
        # is the (optional) name of the language
        if self._input[self._position:].startswith('```'):
            self._position += 3
            self._tokens.append(Token(type=TokenType.FENCE, value='```', start=start))
            return

        self._read_text_or_keyword()

    def _read_comment(self) -> None:
        """
        Read a single-line comment token.
        """
        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[self._position:],
            start=self._position
        ))
        self._position = self._input_len

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a Metaphor keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Metaphor keyword, False otherwise
        """
        keywords = {
            'Action:',
            'Context:',
            'Embed:',
            'Include:',
            'Role:'
        }
        return value in keywords
