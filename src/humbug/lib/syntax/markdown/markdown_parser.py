from dataclasses import dataclass, field
import re
from typing import List, cast

from humbug.lib.syntax.lexer import TokenType, Token
from humbug.lib.syntax.markdown.markdown_lexer import MarkdownLexer
from humbug.lib.syntax.parser import Parser, ParserState
from humbug.lib.syntax.parser_registry import ParserRegistry
from humbug.lib.syntax.programming_language import ProgrammingLanguage
from humbug.lib.syntax.programming_language_utils import ProgrammingLanguageUtils


@dataclass
class MarkdownParserState(ParserState):
    """
    State information for the Markdown parser.

    Attributes:
        in_fence_block: Indicates if we're currently in a code fence block
        fence_depth: Indentation of the current fence block (if we are in one)
        language: The current programming language being parsed
        embedded_parser_state: State of the embedded language parser
        in_list_item: Indicates if we're currently in a list item
        list_indent_stack: Stack of indentation levels for nested lists
        block_type: The type of block element we're in (heading, blockquote, list)
        inline_formatting_stack: Stack tracking nested inline formatting
        in_link: Indicates if we're currently in a link
        in_image: Indicates if we're currently in an image
    """
    in_fence_block: bool = False
    fence_depth: int = 0
    language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN
    embedded_parser_state: ParserState | None = None
    in_list_item: bool = False
    list_indent_stack: List[int] | None = None
    block_type: TokenType | None = None
    inline_formatting_stack: List[str] = field(default_factory=list)
    in_link: bool = False
    in_image: bool = False

    def __post_init__(self) -> None:
        """Initialize mutable default attributes."""
        if self.list_indent_stack is None:
            self.list_indent_stack = []


@ParserRegistry.register_parser(ProgrammingLanguage.MARKDOWN)
class MarkdownParser(Parser):
    """
    Parser for conversation content with embedded code blocks.

    This parser processes conversation content and delegates embedded code blocks to
    appropriate language-specific parsers.
    """

    def _embedded_parse(
        self,
        language: ProgrammingLanguage,
        prev_embedded_parser_state: ParserState | None,
        input_str: str
    ) -> ParserState | None:
        """
        Parse embedded code content using an appropriate language parser.

        Args:
            language: The programming language to use for parsing
            prev_embedded_parser_state: Previous parser state if any
            input_str: The input string to parse

        Returns:
            Updated parser state after parsing

        Note:
            Uses ParserFactory to instantiate appropriate parser for the language.
            Returns None if no parser is available for the language.
        """
        embedded_parser = ParserRegistry.create_parser(language)
        if not embedded_parser:
            return None

        # We apply a per-parser offset to any continuation value in case we switched language!
        continuation_offset = int(language) * 0x1000
        embedded_parser_state = embedded_parser.parse(prev_embedded_parser_state, input_str)
        if embedded_parser_state is not None:
            embedded_parser_state.continuation_state += continuation_offset

        while True:
            token = embedded_parser.get_next_token()
            if token is None:
                break

            self._tokens.append(token)

        return embedded_parser_state

    def _apply_block_style(self, tokens: List[Token], block_type: TokenType) -> None:
        """
        Apply a block style to all tokens that aren't already block elements.

        Args:
            tokens: List of tokens to modify
            block_type: The token type to apply
        """
        for i, token in enumerate(tokens):
            if token.type not in (TokenType.FENCE, TokenType.FENCE_START, TokenType.FENCE_END,
                                  TokenType.LANGUAGE, TokenType.HEADING,
                                  TokenType.BLOCKQUOTE, TokenType.LIST_MARKER):
                tokens[i] = Token(
                    type=block_type,
                    value=token.value,
                    start=token.start
                )

    def _find_closing_paren(self, text: str, start_pos: int) -> int:
        """
        Find matching closing parenthesis, handling nested parentheses.

        Args:
            text: The text to search in
            start_pos: The position after the opening parenthesis

        Returns:
            The position of the closing parenthesis, or -1 if not found
        """
        paren_count = 1
        pos = start_pos

        while pos < len(text):
            if text[pos] == '(':
                paren_count += 1

            elif text[pos] == ')':
                paren_count -= 1
                if paren_count == 0:
                    return pos
            pos += 1

        return -1

    def _parse_inline_formatting_in_text(self, text_token: Token, block_type: TokenType | None) -> List[Token]:
        """
        Parse a text token for inline formatting and return a list of tokens.

        Args:
            text_token: The original text token to parse
            block_type: The block-level type to apply to non-formatted text

        Returns:
            List of tokens with inline formatting broken out
        """
        text = text_token.value
        tokens: List[Token] = []
        i = 0
        current_text_start = 0
        base_position = text_token.start

        def add_text_token(start_idx: int, end_idx: int) -> None:
            """Add a text token with the appropriate block type."""
            if start_idx < end_idx:
                content = text[start_idx:end_idx]
                token_type = block_type if block_type else TokenType.TEXT
                tokens.append(Token(
                    type=token_type,
                    value=content,
                    start=base_position + start_idx
                ))

        while i < len(text):
            # Check for image (highest precedence due to '!' prefix)
            if text[i] == '!' and i + 1 < len(text) and text[i + 1] == '[':
                # Add any accumulated text
                add_text_token(current_text_start, i)

                # Parse image: ![alt](url)
                alt_end = text.find('](', i + 2)
                if alt_end != -1:
                    url_end = self._find_closing_paren(text, alt_end + 2)
                    if url_end != -1:
                        # Image start
                        tokens.append(Token(TokenType.IMAGE_START, '![', base_position + i))

                        # Alt text
                        alt_text = text[i + 2:alt_end]
                        if alt_text:
                            alt_tokens = self._parse_inline_formatting_in_text(
                                Token(TokenType.IMAGE_ALT_TEXT, alt_text, base_position + i + 2),
                                TokenType.IMAGE_ALT_TEXT
                            )
                            tokens.extend(alt_tokens)

                        # Alt end / URL start
                        tokens.append(Token(TokenType.IMAGE_ALT_END, '](', base_position + alt_end))

                        # URL
                        url_text = text[alt_end + 2:url_end]
                        tokens.append(Token(TokenType.IMAGE_URL, url_text, base_position + alt_end + 2))

                        # Image end
                        tokens.append(Token(TokenType.IMAGE_END, ')', base_position + url_end))

                        i = url_end + 1
                        current_text_start = i
                        continue

            # Check for link
            elif text[i] == '[':
                # Add any accumulated text
                add_text_token(current_text_start, i)

                # Parse link: [text](url)
                text_end = text.find('](', i + 1)
                if text_end != -1:
                    url_end = self._find_closing_paren(text, text_end + 2)
                    if url_end != -1:
                        # Link start
                        tokens.append(Token(TokenType.LINK_START, '[', base_position + i))

                        # Link text (may contain nested formatting)
                        link_text = text[i + 1:text_end]
                        if link_text:
                            link_tokens = self._parse_inline_formatting_in_text(
                                Token(TokenType.LINK_TEXT, link_text, base_position + i + 1),
                                TokenType.LINK_TEXT
                            )
                            tokens.extend(link_tokens)

                        # Link text end / URL start
                        tokens.append(Token(TokenType.LINK_TEXT_END, '](', base_position + text_end))

                        # URL
                        url_text = text[text_end + 2:url_end]
                        tokens.append(Token(TokenType.LINK_URL, url_text, base_position + text_end + 2))

                        # Link end
                        tokens.append(Token(TokenType.LINK_END, ')', base_position + url_end))

                        i = url_end + 1
                        current_text_start = i
                        continue

            # Check for inline code
            elif text[i] == '`':
                # Add any accumulated text
                add_text_token(current_text_start, i)

                # Find closing backtick
                code_end = text.find('`', i + 1)
                if code_end != -1:
                    # Code start
                    tokens.append(Token(TokenType.INLINE_CODE_START, '`', base_position + i))

                    # Code content
                    code_content = text[i + 1:code_end]
                    if code_content:
                        tokens.append(Token(TokenType.INLINE_CODE, code_content, base_position + i + 1))

                    # Code end
                    tokens.append(Token(TokenType.INLINE_CODE_END, '`', base_position + code_end))

                    i = code_end + 1
                    current_text_start = i
                    continue

            # Check for bold (**text** or __text__)
            elif (i + 2 < len(text) and
                    ((text[i:i+2] == '**') or (text[i:i+2] == '__' and (i == 0 or text[i - 1].isspace()))) and
                    not text[i+2].isspace()):  # pylint: disable=too-many-boolean-expressions
                # Add any accumulated text
                add_text_token(current_text_start, i)

                marker = text[i:i+2]
                bold_end = text.find(marker, i + 2)
                if bold_end != -1:
                    # Bold start
                    tokens.append(Token(TokenType.BOLD_START, marker, base_position + i))

                    # Bold content (may contain nested formatting)
                    bold_content = text[i + 2:bold_end]
                    if bold_content:
                        bold_tokens = self._parse_inline_formatting_in_text(
                            Token(TokenType.BOLD, bold_content, base_position + i + 2),
                            TokenType.BOLD
                        )
                        tokens.extend(bold_tokens)

                    # Bold end
                    tokens.append(Token(TokenType.BOLD_END, marker, base_position + bold_end))

                    i = bold_end + 2
                    current_text_start = i
                    continue

            # Check for italic (*text* or _text_)
            elif (i + 1 < len(text) and
                    ((text[i] == '*') or (text[i] == '_' and (i == 0 or text[i - 1].isspace()))) and
                    not text[i + 1].isspace() and
                    (i == 0 or text[i-1] != text[i])):  # pylint: disable=too-many-boolean-expressions
                # Add any accumulated text
                add_text_token(current_text_start, i)

                marker = text[i]
                italic_end = text.find(marker, i + 1)
                if (italic_end != -1 and
                    (italic_end + 1 >= len(text) or text[italic_end + 1] != marker)):  # Avoid **

                    # Italic start
                    tokens.append(Token(TokenType.ITALIC_START, marker, base_position + i))

                    # Italic content (may contain nested formatting)
                    italic_content = text[i + 1:italic_end]
                    if italic_content:
                        italic_tokens = self._parse_inline_formatting_in_text(
                            Token(TokenType.ITALIC, italic_content, base_position + i + 1),
                            TokenType.ITALIC
                        )
                        tokens.extend(italic_tokens)

                    # Italic end
                    tokens.append(Token(TokenType.ITALIC_END, marker, base_position + italic_end))

                    i = italic_end + 1
                    current_text_start = i
                    continue

            # No formatting found, move to next character
            i += 1

        # Add any remaining text
        add_text_token(current_text_start, len(text))

        return tokens

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> MarkdownParserState:
        """
        Parse conversation content including embedded code blocks.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            Handles transitions between regular conversation content and code fence blocks,
            delegating code blocks to appropriate language parsers.
            Supports nested list items by maintaining a stack of indentation levels.
        """
        in_fence_block = False
        fence_depth = 0
        language = ProgrammingLanguage.UNKNOWN
        embedded_parser_state = None
        parsing_continuation = False
        in_list_item = False
        list_indent_stack: List[int] = []
        block_type = None

        if prev_parser_state is not None:
            assert isinstance(prev_parser_state, MarkdownParserState), \
                f"Expected MarkdownParserState, got {type(prev_parser_state).__name__}"
            in_fence_block = prev_parser_state.in_fence_block
            fence_depth = prev_parser_state.fence_depth
            language = prev_parser_state.language
            embedded_parser_state = prev_parser_state.embedded_parser_state
            parsing_continuation = prev_parser_state.parsing_continuation
            in_list_item = prev_parser_state.in_list_item
            list_indent_stack = cast(List[int], prev_parser_state.list_indent_stack).copy()  # Copy to avoid reference issues
            block_type = prev_parser_state.block_type

        parse_embedded = language != ProgrammingLanguage.UNKNOWN

        if not parsing_continuation:
            lexer = MarkdownLexer()
            lexer.lex(None, input_str)

            # Collect all tokens from the lexer
            while True:
                token = lexer.get_next_token()
                if not token:
                    break

                if token.type == TokenType.FENCE:
                    if in_fence_block:
                        # Only close the fence if indentation is less than or equal to opening fence
                        if token.start > fence_depth:
                            # This fence is indented more than the opening fence
                            # Treat it as regular text within the current fence block
                            token.type = TokenType.TEXT
                            self._tokens.append(token)
                            continue

                        token.type = TokenType.FENCE_END
                        self._tokens.append(token)
                        in_fence_block = False
                        fence_depth = 0
                        language = ProgrammingLanguage.UNKNOWN
                        embedded_parser_state = None
                        parse_embedded = False
                        break

                    in_fence_block = True
                    fence_depth = token.start
                    embedded_parser_state = None
                    token.type = TokenType.FENCE_START
                    self._tokens.append(token)

                    next_token = lexer.peek_next_token()
                    if next_token and (next_token.type == TokenType.TEXT):
                        lexer.get_next_token()  # Consume the text token
                        next_token.type = TokenType.LANGUAGE
                        self._tokens.append(next_token)
                        language = ProgrammingLanguageUtils.from_name(next_token.value)
                        break

                    language = ProgrammingLanguage.TEXT
                    break

                if parse_embedded:
                    break

                if token.type in (TokenType.HEADING, TokenType.BLOCKQUOTE):
                    in_list_item = False
                    block_type = token.type
                    list_indent_stack.clear()
                    self._tokens.append(token)
                    continue

                if token.type == TokenType.LIST_MARKER:
                    # We've found a list marker - determine its nesting level
                    matches = list(re.finditer(r'\S+', token.value))
                    assert len(matches) >= 2, "List marker must have at least two elements (marker and text)"
                    second_word_match = matches[1]
                    current_indent = second_word_match.start() + token.start

                    # Handle nesting level logic
                    if not in_list_item:
                        # Starting a new list
                        in_list_item = True
                        list_indent_stack.append(current_indent)

                    else:
                        # We're already in a list, check if this is a nested list or continuation
                        if list_indent_stack and current_indent > list_indent_stack[-1]:
                            # This is a nested list with deeper indentation
                            list_indent_stack.append(current_indent)

                        elif list_indent_stack:
                            # Check if we need to pop out of nested lists
                            while list_indent_stack and current_indent < list_indent_stack[-1]:
                                list_indent_stack.pop()

                            # If we popped everything, we're at a new list
                            if not list_indent_stack:
                                list_indent_stack.append(current_indent)

                            # If at same level as an existing indent, we're continuing that list
                            elif current_indent != list_indent_stack[-1]:
                                list_indent_stack.append(current_indent)

                    block_type = token.type
                    self._tokens.append(token)
                    continue

                if in_list_item and list_indent_stack:
                    # Check if this is a continuation of the current list item
                    if token.start >= list_indent_stack[-1]:
                        token.type = TokenType.LIST_MARKER
                        self._tokens.append(token)
                        continue

                    # Not enough indentation, pop out of nested lists as needed
                    while list_indent_stack and token.start < list_indent_stack[-1]:
                        list_indent_stack.pop()

                    if not list_indent_stack:
                        # We've exited all lists
                        in_list_item = False
                        block_type = None

                    else:
                        # We're still in a list, but at a higher level
                        token.type = TokenType.LIST_MARKER
                        self._tokens.append(token)
                        continue

                # Not in a list anymore or never were
                if not list_indent_stack:
                    block_type = None
                    in_list_item = False

                self._tokens.append(token)

            # After collecting all tokens from lexer, process inline formatting
            if not parse_embedded:
                processed_tokens = []
                for token in self._tokens:
                    if token.type  in (TokenType.TEXT, TokenType.HEADING, TokenType.BLOCKQUOTE, TokenType.LIST_MARKER):
                        # Parse inline formatting in this token
                        inline_tokens = self._parse_inline_formatting_in_text(token, token.type)
                        processed_tokens.extend(inline_tokens)

                    else:
                        processed_tokens.append(token)

                self._tokens = processed_tokens

        # Create and return parser state
        parser_state = MarkdownParserState()
        parser_state.in_fence_block = in_fence_block
        parser_state.fence_depth = fence_depth
        parser_state.language = language
        parser_state.in_list_item = in_list_item
        parser_state.list_indent_stack = list_indent_stack
        parser_state.block_type = block_type

        if parse_embedded:
            new_embedded_parser_state = self._embedded_parse(parser_state.language, embedded_parser_state, input_str)
            parser_state.embedded_parser_state = new_embedded_parser_state
            if new_embedded_parser_state is not None:
                parser_state.continuation_state = new_embedded_parser_state.continuation_state
                parser_state.parsing_continuation = new_embedded_parser_state.parsing_continuation

        return parser_state
