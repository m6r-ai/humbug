from typing import Callable, Dict, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token


# Define styles as a module-level dictionary similar to the TypeScript implementation
styles: Dict[str, str] = {'HEADING': 'heading'}


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

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if ch == '\n':
            return self._read_newline

        if self._is_whitespace(ch):
            return self._read_whitespace

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

        while self._position < len(self._input):
            ch = self._input[self._position]

            if ch == '\n':
                break

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
                        type='KEYWORD',
                        value=potential_keyword,
                        start=start
                    ))
                    return

        token_type = 'HEADING' if self._seen_keyword else 'TEXT'
        self._tokens.append(Token(
            type=token_type,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_newline(self) -> None:
        """
        Read a newline token and reset the keyword flag.
        """
        start = self._position
        self._position += 1
        self._seen_keyword = False
        self._tokens.append(Token(
            type='NEWLINE',
            value='\n',
            start=start
        ))

    def _read_comment(self) -> None:
        """
        Read a single-line comment token.
        """
        start = self._position
        self._position += 1
        while (self._position < len(self._input) and
               self._input[self._position] != '\n'):
            self._position += 1

        self._tokens.append(Token(
            type='COMMENT',
            value=self._input[start:self._position],
            start=start
        ))

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
            'Import:',
            'Role:'
        }
        return value in keywords
