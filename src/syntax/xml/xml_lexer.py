from dataclasses import dataclass
from typing import Callable

from syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class XMLLexerState(LexerState):
    """
    State information for the XML lexer.

    Attributes:
        in_tag: Indicates if we're currently parsing a tag
        tag_name: The name of any tag we're currently parsing
        seen_equals: Indicates if we're currently parsing an attribute
        in_comment: Indicates if we're currently parsing a block comment
        in_cdata: Indicates if we're currently parsing a CDATA section
        in_processing_instruction: Indicates if we're currently parsing a processing instruction
    """
    in_tag: bool = False
    tag_name: str = ""
    seen_equals: bool = False
    in_comment: bool = False
    in_cdata: bool = False
    in_processing_instruction: bool = False


class XMLLexer(Lexer):
    """
    Lexer for XML code.

    This lexer handles XML-specific syntax including tags, attributes, XML declarations,
    comments, CDATA sections, and processing instructions.
    """

    def __init__(self) -> None:
        """
        Initialize the XML lexer.
        """
        super().__init__()
        self._in_tag = False
        self._tag_name = ''
        self._seen_equals = False
        self._in_comment = False
        self._in_cdata = False
        self._in_processing_instruction = False

    def lex(self, prev_lexer_state: LexerState | None, input_str: str) -> XMLLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state is not None:
            assert isinstance(prev_lexer_state, XMLLexerState), \
                f"Expected XMLLexerState, got {type(prev_lexer_state).__name__}"
            self._in_tag = prev_lexer_state.in_tag
            self._tag_name = prev_lexer_state.tag_name
            self._seen_equals = prev_lexer_state.seen_equals
            self._in_comment = prev_lexer_state.in_comment
            self._in_cdata = prev_lexer_state.in_cdata
            self._in_processing_instruction = prev_lexer_state.in_processing_instruction

        if self._in_comment:
            self._read_xml_comment(0)

        elif self._in_cdata:
            self._read_cdata_section(0)

        elif self._in_processing_instruction:
            self._read_processing_instruction(0)

        if not self._in_comment and not self._in_cdata and not self._in_processing_instruction:
            self._inner_lex()

        lexer_state = XMLLexerState()
        lexer_state.in_tag = self._in_tag
        lexer_state.tag_name = self._tag_name
        lexer_state.seen_equals = self._seen_equals
        lexer_state.in_comment = self._in_comment
        lexer_state.in_cdata = self._in_cdata
        lexer_state.in_processing_instruction = self._in_processing_instruction
        return lexer_state

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if ch == '<':
            return self._read_open

        if ch == '>':
            return self._read_close

        return self._read_default

    def _read_open(self) -> None:
        """
        Read an opening angle bracket and handle special XML constructs.
        """
        # Check for XML comments (<!--)
        if (self._position + 3 < self._input_len and
                self._input[self._position:self._position + 4] == '<!--'):
            self._in_comment = True
            self._read_xml_comment(4)
            return

        # Check for CDATA sections (<![CDATA[)
        if (self._position + 8 < self._input_len and
                self._input[self._position:self._position + 9] == '<![CDATA['):
            self._in_cdata = True
            self._read_cdata_section(9)
            return

        # Check for processing instructions (<?xml or other <?)
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] == '?'):
            self._in_processing_instruction = True
            self._read_processing_instruction(2)
            return

        # Check for DOCTYPE declaration
        if (self._position + 1 < self._input_len and
                self._input[self._position + 1] == '!'):
            if self._input[self._position + 2:].startswith('DOCTYPE'):
                self._read_doctype()
                return

        # Regular tag opening
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

        # Handle nested brackets in DOCTYPE (for internal DTD subset)
        bracket_depth = 1
        while self._position < self._input_len and bracket_depth > 0:
            ch = self._input[self._position]
            if ch == '[':
                bracket_depth += 1
            elif ch == ']':
                bracket_depth -= 1
            elif ch == '>' and bracket_depth == 1:
                break
            self._position += 1

        if self._position < self._input_len:
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.DOCTYPE,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_xml_comment(self, skip_chars: int) -> None:
        """
        Read an XML comment.

        Args:
            skip_chars: Number of characters to skip at the start (0 if continuing from previous line)
        """
        start = self._position
        self._position += skip_chars  # Skip past '<!--'

        while (self._position + 2) < self._input_len:
            if self._input[self._position:self._position + 3] == '-->':
                self._in_comment = False
                self._position += 3
                break
            self._position += 1

        # If we're still in a comment, we've reached the end of the line
        if self._in_comment:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_cdata_section(self, skip_chars: int) -> None:
        """
        Read a CDATA section.

        Args:
            skip_chars: Number of characters to skip at the start (0 if continuing from previous line)
        """
        start = self._position
        self._position += skip_chars  # Skip past '<![CDATA['

        while (self._position + 2) < self._input_len:
            if self._input[self._position:self._position + 3] == ']]>':
                self._in_cdata = False
                self._position += 3
                break
            self._position += 1

        # If we're still in CDATA, we've reached the end of the line
        if self._in_cdata:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_processing_instruction(self, skip_chars: int) -> None:
        """
        Read a processing instruction (e.g., <?xml version="1.0"?>).

        Args:
            skip_chars: Number of characters to skip at the start (0 if continuing from previous line)
        """
        start = self._position
        self._position += skip_chars  # Skip past '<?'

        while (self._position + 1) < self._input_len:
            if self._input[self._position:self._position + 2] == '?>':
                self._in_processing_instruction = False
                self._position += 2
                break
            self._position += 1

        # If we're still in a processing instruction, we've reached the end of the line
        if self._in_processing_instruction:
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.PREPROCESSOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_tag_or_attribute(self, token_type: TokenType, allow_slash: bool = False) -> Token:
        """
        Read a tag name or attribute name.

        Args:
            token_type: The type of token to create

        Returns:
            The created token
        """
        start = self._position
        self._position += 1
        allowed_special = ('-', ':')
        if allow_slash:
            allowed_special = ('-', ':', '/')

        while (self._position < self._input_len and
               (self._input[self._position].isalnum() or
                self._input[self._position] == '_' or
                self._input[self._position] in allowed_special)):
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
        # Skip whitespace inside tags
        if self._is_whitespace(self._input[self._position]):
            self._read_whitespace()
            return

        if not self._tag_name:
            token = self._read_tag_or_attribute(TokenType.HTML_TAG, allow_slash=True)
            self._tag_name = token.value
            self._tokens.append(token)
            return

        ch = self._input[self._position]

        # Skip standalone '/' in self-closing tags
        if ch == '/' and self._position + 1 < self._input_len and self._input[self._position + 1] == '>':
            self._position += 1
            return

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
        Read text content between XML tags.
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
