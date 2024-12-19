from typing import Callable

from humbug.syntax.lexer import Lexer, Token


class HTMLLexer(Lexer):
    """
    Lexer for HTML code.

    This lexer handles HTML-specific syntax including tags, attributes, DOCTYPE
    declarations, comments, and embedded script and style content.

    Attributes:
        _in_tag: Indicates if we're currently within an HTML tag
        _tag_name: The name of the current tag being processed
        _seen_equals: Indicates if we've seen an equals sign in the current attribute
    """

    def __init__(self, input_str: str) -> None:
        """
        Initialize the HTML lexer.

        Args:
            input_str: The input string to lex
        """
        super().__init__(input_str)
        self._in_tag = False
        self._tag_name = ''
        self._seen_equals = False

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
        if (self._position + 1 < len(self._input) and
                self._input[self._position + 1] == '!'):
            if self._input[self._position + 2:].startswith('HTML_DOCTYPE'):
                self._read_doctype()
                return

            self._read_html_comment()
            return

        self._position += 1
        self._in_tag = True
        self._tag_name = ''
        self._tokens.append(Token(
            type='OPERATOR',
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
            type='OPERATOR',
            value='>',
            start=self._position - 1
        ))

    def _read_default(self) -> None:
        """
        Read tag content or text between tags.
        """
        if self._in_tag:
            self._read_tag()
            return

        # Handle script elements
        if self._tag_name.lower() == 'script':
            script_open = self._position
            script_close = self._input.lower().find('</script', self._position)
            if script_close == -1:
                script_close = len(self._input)

            self._position = script_close
            self._tokens.append(Token(
                type='SCRIPT',
                value=self._input[script_open:script_close],
                start=script_open
            ))
            return

        # Handle style elements
        if self._tag_name.lower() == 'style':
            style_open = self._position
            style_close = self._input.lower().find('</style', self._position)
            if style_close == -1:
                style_close = len(self._input)

            self._position = style_close
            self._tokens.append(Token(
                type='STYLE',
                value=self._input[style_open:style_close],
                start=style_open
            ))
            return

        self._read_text()

    def _read_doctype(self) -> None:
        """
        Read a DOCTYPE declaration.
        """
        start = self._position
        self._position += 9  # Skip past '<!DOCTYPE'
        while (self._position < len(self._input) and
               self._input[self._position] != '>'):
            self._position += 1

        if self._position < len(self._input):
            self._position += 1

        self._tokens.append(Token(
            type='DOCTYPE',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_html_comment(self) -> None:
        """
        Read an HTML comment.
        """
        start = self._position
        self._position += 4  # Skip past '<!--'
        while (self._position < len(self._input) and
                (self._input[self._position - 2:self._position + 1] != '-->')):
            self._position += 1

        if self._position < len(self._input):
            self._position += 1

        self._tokens.append(Token(
            type='COMMENT',
            value=self._input[start:self._position],
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
        while (self._position < len(self._input) and
               (self._is_letter_or_digit(self._input[self._position]) or
                self._input[self._position] in ('_', '-', '/'))):
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
            token = self._read_tag_or_attribute('HTML_TAG')
            self._tag_name = token.value
            self._tokens.append(token)
            return

        ch = self._input[self._position]
        if ch == '=':
            self._position += 1
            self._seen_equals = True
            self._tokens.append(Token(
                type='OPERATOR',
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
            self._read_tag_or_attribute('STRING' if seen_equals else 'HTML_ATTRIBUTE')
        )

    def _read_text(self) -> None:
        """
        Read text content between HTML tags.
        """
        start = self._position
        while (self._position < len(self._input) and
               self._input[self._position] != '<'):
            self._position += 1

        self._tokens.append(Token(
            type='TEXT',
            value=self._input[start:self._position],
            start=start
        ))
