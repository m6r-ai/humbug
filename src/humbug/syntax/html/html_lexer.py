from dataclasses import dataclass
from typing import Callable, Optional

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class HTMLLexerState(LexerState):
    """
    State information for the HTML lexer.

    Attributes:
        in_tag: Indicates if we're currently parsing a tag
        tag_name: The name of any tag we're currently parsing
        seen_equals: Indicates if we're currently parsing an attribute
        in_comment: Indicates if we're currently parsing a block comment
        in_script: Indicates if we're currently parsing a script block
        in_style: Indicates if we're currently parsing a style block
    """
    in_tag: bool = False
    tag_name: str = ""
    seen_equals: bool = False
    in_comment: bool = False
    in_script: bool = False
    in_style: bool = False


class HTMLLexer(Lexer):
    """
    Lexer for HTML code.

    This lexer handles HTML-specific syntax including tags, attributes, DOCTYPE
    declarations, comments, and embedded script and style content.
    """

    def __init__(self) -> None:
        """
        Initialize the HTML lexer.

        Args:
            input_str: The input string to lex
        """
        super().__init__()
        self._in_tag = False
        self._tag_name = ''
        self._seen_equals = False
        self._in_comment = False
        self._in_script = False
        self._in_style = False

    def lex(self, prev_lexer_state: Optional[HTMLLexerState], input_str: str) -> HTMLLexerState:
        """
        Lex all the tokens in the input.
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state:
            self._in_tag = prev_lexer_state.in_tag
            self._tag_name = prev_lexer_state.tag_name
            self._seen_equals = prev_lexer_state.seen_equals
            self._in_comment = prev_lexer_state.in_comment
            self._in_script = prev_lexer_state.in_script
            self._in_style = prev_lexer_state.in_style

        if self._in_comment:
            self._read_html_comment(0)
        elif self._in_script:
            self._read_script_block()
        elif self._in_style:
            self._read_style_block()

        if not self._in_comment and not self._in_script and not self._in_style:
            self._inner_lex()

        lexer_state = HTMLLexerState()
        lexer_state.in_tag = self._in_tag
        lexer_state.tag_name = self._tag_name
        lexer_state.seen_equals = self._seen_equals
        lexer_state.in_comment = self._in_comment
        lexer_state.in_script = self._in_script
        lexer_state.in_style = self._in_style
        return lexer_state

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

        if ch == '<':
            return self._read_open

        if ch == '>':
            return self._read_close

        return self._read_default

    def _read_open(self) -> None:
        """
        Read an opening angle bracket and handle DOCTYPE declarations and comments.
        """
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] == '!'):
            if self._input[self._position + 2:].startswith('DOCTYPE'):
                self._read_doctype()
                return

            self._in_comment = True
            self._read_html_comment(4)
            return

        self._position += 1
        self._in_tag = True
        self._tag_name = ''
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value='<',
            start=self._position - 1
        ))

    def _read_close(self) -> None:
        """
        Read a closing angle bracket.
        """
        self._position += 1
        self._in_tag = False
        self._tokens.append(Token(
            type=TokenType.OPERATOR,
            value='>',
            start=self._position - 1
        ))

        if self._tag_name.lower() == 'script':
            self._in_script = True
            self._read_script_block()
            return

        if self._tag_name.lower() == 'style':
            self._in_style = True
            self._read_style_block()

    def _read_default(self) -> None:
        """
        Read tag content or text between tags.
        """
        if self._in_tag:
            self._read_tag()
            return

        self._read_text()

    def _read_doctype(self) -> None:
        """
        Read a DOCTYPE declaration.
        """
        start = self._position
        self._position += 9  # Skip past '<!DOCTYPE'
        while (self._position < self._input_len and
               self._input[self._position] != '>'):
            self._position += 1

        if self._position < self._input_len:
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.DOCTYPE,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_html_comment(self, skip_chars: int) -> None:
        """
        Read an HTML comment.
        """
        start = self._position
        self._position += skip_chars  # Skip past '<!--'
        while (self._position + 2) < self._input_len:
            if self._input[self._position:self._position + 3] == '-->':
                self._in_comment = False
                self._position += 3

            self._position += 1

        # If we're still in a block comment we've got two characters left on this line and
        # we need to include them in the comment too.
        if self._in_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_script_block(self) -> None:
        """
        Read a script block.
        """
        start = self._position
        script_close = self._input.lower().find('</script', self._position)
        if script_close != -1:
            self._in_script = False
        else:
            script_close = self._input_len

        self._position = script_close
        self._tokens.append(Token(
            type=TokenType.SCRIPT,
            value=self._input[start:script_close],
            start=start
        ))

    def _read_style_block(self) -> None:
        """
        Read a style block.
        """
        start = self._position
        style_close = self._input.lower().find('</style', self._position)
        if style_close != -1:
            self._in_style = False
        else:
            style_close = self._input_len

        self._position = style_close
        self._tokens.append(Token(
            type=TokenType.STYLE,
            value=self._input[start:style_close],
            start=start
        ))

    def _read_tag_or_attribute(self, token_type: str) -> Token:
        """
        Read a tag name or attribute name.

        Args:
            token_type: The type of token to create

        Returns:
            The created token
        """
        start = self._position
        self._position += 1
        while (self._position < self._input_len and
               (self._is_letter_or_digit_or_underscore(self._input[self._position]) or
                self._input[self._position] in ('-', '/'))):
            self._position += 1

        return Token(
            type=token_type,
            value=self._input[start:self._position],
            start=start
        )

    def _read_tag(self) -> None:
        """
        Read tag content including tag names and attributes.
        """
        if not self._tag_name:
            token = self._read_tag_or_attribute(TokenType.HTML_TAG)
            self._tag_name = token.value
            self._tokens.append(token)
            return

        ch = self._input[self._position]
        if ch == '=':
            self._position += 1
            self._seen_equals = True
            self._tokens.append(Token(
                type=TokenType.OPERATOR,
                value='=',
                start=self._position - 1
            ))
            return

        seen_equals = self._seen_equals
        self._seen_equals = False

        if ch in ('"', "'"):
            self._read_string()
            return

        self._tokens.append(
            self._read_tag_or_attribute(TokenType.STRING if seen_equals else TokenType.HTML_ATTRIBUTE)
        )

    def _read_text(self) -> None:
        """
        Read text content between HTML tags.
        """
        start = self._position
        while (self._position < self._input_len and
               self._input[self._position] != '<'):
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.TEXT,
            value=self._input[start:self._position],
            start=start
        ))
