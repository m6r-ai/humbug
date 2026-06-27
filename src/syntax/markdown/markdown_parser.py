from dataclasses import dataclass, field

from syntax.lexer import TokenType, Token
from syntax.markdown.markdown_lexer import MarkdownLexer
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.programming_language_utils import ProgrammingLanguageUtils


@dataclass
class BlockContext:
    """
    A single entry in the block-level context stack.

    Attributes:
        type: The block type — either BLOCKQUOTE or LIST_MARKER
        indent: For LIST_MARKER, the column at which list item content begins.
                For BLOCKQUOTE, unused (depth is implicit in stack position).
    """
    type: TokenType
    indent: int = 0


@dataclass
class MarkdownParserState(ParserState):
    """
    State information for the Markdown parser.

    Attributes:
        in_fence_block: Indicates if we're currently in a code fence block
        fence_depth: Indentation of the current fence block (if we are in one)
        nested_fence_depth: Counter for nested fence blocks within the current fence
        fence_blockquote_depth: Number of BLOCKQUOTE entries in the block stack when
            the current fence was opened; used to strip the same number of '>' prefixes
            from each subsequent content line before delegating to the embedded parser
        language: The current programming language being parsed
        embedded_parser_state: State of the embedded language parser
        block_stack: Stack of active block-level contexts (BLOCKQUOTE and LIST_MARKER
            entries). The innermost entry determines how content tokens are coloured.
        inline_formatting_stack: Stack tracking nested inline formatting
        in_link: Indicates if we're currently in a link
        in_image: Indicates if we're currently in an image
    """
    in_fence_block: bool = False
    fence_depth: int = 0
    nested_fence_depth: int = 0
    fence_blockquote_depth: int = 0
    language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN
    embedded_parser_state: ParserState | None = None
    block_stack: list[BlockContext] = field(default_factory=list)
    inline_formatting_stack: list[str] = field(default_factory=list)
    in_link: bool = False
    in_image: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.MARKDOWN)
class MarkdownParser(Parser):
    """
    Parser for Markdown content with embedded code blocks.

    Each call to parse() handles one line.  The broad structure is:

    1. Strip leading blockquote prefixes ('> ') from the line, emitting a
       BLOCKQUOTE token for each one and recording the total character offset.
    2. If we are inside a fence block, delegate the stripped content to the
       embedded language parser (adjusting token offsets back to the original
       line's coordinate space).
    3. Otherwise, lex the stripped content and classify it: fence open/close,
       list marker, heading, or plain text.
    4. Run inline-formatting detection over any TEXT / HEADING / BLOCKQUOTE /
       LIST_MARKER tokens to emit bold, italic, link, etc. sub-tokens.
    """

    def _strip_blockquote_prefixes(self, input_str: str) -> tuple[list[Token], str, int]:
        """
        Strip all leading blockquote markers from a line.

        Mirrors dmarkdown's _identify_line_type blockquote-stripping loop so that
        the rest of the parser always works on the innermost content regardless of
        how many levels of '>' nesting are present.

        Args:
            input_str: The raw input line.

        Returns:
            A tuple of:
            - list of BLOCKQUOTE tokens, one per '>' marker found
            - the remaining string after all markers have been removed
            - the total number of characters consumed (== offset of remaining in input_str)
        """
        bq_tokens: list[Token] = []
        remaining = input_str
        offset = 0

        while True:
            stripped = remaining.lstrip()
            spaces_before = len(remaining) - len(stripped)

            if not stripped.startswith('>'):
                break

            # Emit a BLOCKQUOTE token for this '>' (plus the leading spaces and
            # the optional single space that follows the '>').
            after_marker = stripped[1:]
            if after_marker.startswith(' '):
                consumed = spaces_before + 2   # "> "
                token_value = remaining[:consumed]

            else:
                consumed = spaces_before + 1   # ">"
                token_value = remaining[:consumed]

            bq_tokens.append(Token(type=TokenType.BLOCKQUOTE, value=token_value, start=offset))
            offset += consumed
            remaining = remaining[consumed:]

        return bq_tokens, remaining, offset

    def _embedded_parse(
        self,
        language: ProgrammingLanguage,
        prev_embedded_parser_state: ParserState | None,
        input_str: str,
        token_offset: int = 0
    ) -> ParserState | None:
        """
        Parse embedded code content using an appropriate language parser.

        Args:
            language: The programming language to use for parsing
            prev_embedded_parser_state: Previous parser state if any
            input_str: The input string to parse (already stripped of blockquote prefixes)
            token_offset: Characters removed from the start of the original line before
                input_str was produced.  Every token's start position is shifted by this
                amount so that offsets are relative to the original line.

        Returns:
            Updated parser state after parsing, or None if no parser is available.
        """
        embedded_parser = ParserRegistry.create_parser(language)
        if not embedded_parser:
            return None

        # Apply a per-language offset to continuation values to avoid collisions
        # when the embedded language changes between lines.
        continuation_offset = int(language) * 0x1000
        embedded_parser_state = embedded_parser.parse(prev_embedded_parser_state, input_str)
        if embedded_parser_state is not None:
            embedded_parser_state.continuation_state += continuation_offset

        while True:
            token = embedded_parser.get_next_token()
            if token is None:
                break

            self._tokens.append(Token(type=token.type, value=token.value, start=token.start + token_offset))

        return embedded_parser_state

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

    def _is_bold_marker(self, text: str, i: int) -> bool:
        """
        Check if position i starts a valid bold formatting marker.

        Args:
            text: The text being parsed
            i: The current position in the text

        Returns:
            True if position i starts a bold marker (**text** or __text__)
        """
        if i + 2 >= len(text) or text[i+2].isspace():
            return False

        is_double_asterisk = text[i:i+2] == '**'
        is_double_underscore = (text[i:i+2] == '__' and (i == 0 or text[i - 1].isspace()))

        return is_double_asterisk or is_double_underscore

    def _is_italic_marker(self, text: str, i: int) -> bool:
        """
        Check if position i starts a valid italic formatting marker.

        Args:
            text: The text being parsed
            i: The current position in the text

        Returns:
            True if position i starts an italic marker (*text* or _text_)
        """
        if i + 1 >= len(text) or text[i + 1].isspace():
            return False

        at_start = i == 0
        preceded_by_space = not at_start and text[i - 1].isspace()
        not_part_of_bold = text[i + 1] != text[i]

        is_asterisk = text[i] == '*'
        is_underscore = text[i] == '_' and (at_start or preceded_by_space)

        return (is_asterisk or is_underscore) and not_part_of_bold

    def _is_strikethrough_marker(self, text: str, i: int) -> bool:
        """
        Check if position i starts a valid strikethrough formatting marker.

        Args:
            text: The text being parsed
            i: The current position in the text

        Returns:
            True if position i starts a strikethrough marker (~~text~~)
        """
        if i + 2 >= len(text) or text[i + 2].isspace():
            return False

        return text[i:i+2] == '~~'

    def _parse_inline_formatting_in_text(self, text_token: Token, block_type: TokenType | None) -> list[Token]:
        """
        Parse a text token for inline formatting and return a list of tokens.

        Args:
            text_token: The original text token to parse
            block_type: The block-level type to apply to non-formatted text

        Returns:
            List of tokens with inline formatting broken out
        """
        text = text_token.value
        tokens: list[Token] = []
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
                add_text_token(current_text_start, i)

                alt_end = text.find('](', i + 2)
                if alt_end != -1:
                    url_end = self._find_closing_paren(text, alt_end + 2)
                    if url_end != -1:
                        tokens.append(Token(TokenType.IMAGE_START, '![', base_position + i))

                        alt_text = text[i + 2:alt_end]
                        if alt_text:
                            alt_tokens = self._parse_inline_formatting_in_text(
                                Token(TokenType.IMAGE_ALT_TEXT, alt_text, base_position + i + 2),
                                TokenType.IMAGE_ALT_TEXT
                            )
                            tokens.extend(alt_tokens)

                        tokens.append(Token(TokenType.IMAGE_ALT_END, '](', base_position + alt_end))

                        url_text = text[alt_end + 2:url_end]
                        tokens.append(Token(TokenType.IMAGE_URL, url_text, base_position + alt_end + 2))

                        tokens.append(Token(TokenType.IMAGE_END, ')', base_position + url_end))

                        i = url_end + 1
                        current_text_start = i
                        continue

                # Incomplete image syntax; treat the '![' as plain text
                current_text_start = i
                i += 2
                continue

            # Check for link
            if text[i] == '[':
                add_text_token(current_text_start, i)

                text_end = text.find('](', i + 1)
                if text_end != -1:
                    url_end = self._find_closing_paren(text, text_end + 2)
                    if url_end != -1:
                        tokens.append(Token(TokenType.LINK_START, '[', base_position + i))

                        link_text = text[i + 1:text_end]
                        if link_text:
                            link_tokens = self._parse_inline_formatting_in_text(
                                Token(TokenType.LINK_TEXT, link_text, base_position + i + 1),
                                TokenType.LINK_TEXT
                            )
                            tokens.extend(link_tokens)

                        tokens.append(Token(TokenType.LINK_TEXT_END, '](', base_position + text_end))

                        url_text = text[text_end + 2:url_end]
                        tokens.append(Token(TokenType.LINK_URL, url_text, base_position + text_end + 2))

                        tokens.append(Token(TokenType.LINK_END, ')', base_position + url_end))

                        i = url_end + 1
                        current_text_start = i
                        continue

                # Incomplete link syntax; treat the '[' as plain text
                current_text_start = i

            # Check for inline code
            elif text[i] == '`':
                add_text_token(current_text_start, i)

                code_end = text.find('`', i + 1)
                if code_end != -1:
                    tokens.append(Token(TokenType.INLINE_CODE_START, '`', base_position + i))

                    code_content = text[i + 1:code_end]
                    if code_content:
                        tokens.append(Token(TokenType.INLINE_CODE, code_content, base_position + i + 1))

                    tokens.append(Token(TokenType.INLINE_CODE_END, '`', base_position + code_end))

                    i = code_end + 1
                    current_text_start = i
                    continue

                # No closing backtick found; treat the ` as plain text
                current_text_start = i

            # Check for bold (**text** or __text__)
            elif self._is_bold_marker(text, i):
                add_text_token(current_text_start, i)

                marker = text[i:i+2]
                bold_end = text.find(marker, i + 2)
                if bold_end != -1:
                    tokens.append(Token(TokenType.BOLD_START, marker, base_position + i))

                    bold_content = text[i + 2:bold_end]
                    if bold_content:
                        bold_tokens = self._parse_inline_formatting_in_text(
                            Token(TokenType.BOLD, bold_content, base_position + i + 2),
                            TokenType.BOLD
                        )
                        tokens.extend(bold_tokens)

                    tokens.append(Token(TokenType.BOLD_END, marker, base_position + bold_end))

                    i = bold_end + 2
                    current_text_start = i
                    continue

                # No closing bold marker found; treat the marker as plain text
                current_text_start = i

            # Check for italic (*text* or _text_)
            elif self._is_italic_marker(text, i):
                add_text_token(current_text_start, i)

                marker = text[i]
                search_from = i + 1
                italic_end = -1
                while True:
                    candidate = text.find(marker, search_from)
                    if candidate == -1:
                        break

                    if candidate + 1 >= len(text) or text[candidate + 1] != marker:
                        italic_end = candidate
                        break

                    # This candidate is part of **, skip past it
                    search_from = candidate + 2

                if italic_end != -1:
                    tokens.append(Token(TokenType.ITALIC_START, marker, base_position + i))

                    italic_content = text[i + 1:italic_end]
                    if italic_content:
                        italic_tokens = self._parse_inline_formatting_in_text(
                            Token(TokenType.ITALIC, italic_content, base_position + i + 1),
                            TokenType.ITALIC
                        )
                        tokens.extend(italic_tokens)

                    tokens.append(Token(TokenType.ITALIC_END, marker, base_position + italic_end))

                    i = italic_end + 1
                    current_text_start = i
                    continue

                # Italic span rejected or unclosed; treat the marker as plain text
                current_text_start = i

            # Check for strikethrough (~~text~~)
            elif self._is_strikethrough_marker(text, i):
                add_text_token(current_text_start, i)

                strike_end = text.find('~~', i + 2)
                if strike_end != -1:
                    tokens.append(Token(TokenType.STRIKETHROUGH_START, '~~', base_position + i))

                    strike_content = text[i + 2:strike_end]
                    if strike_content:
                        strike_tokens = self._parse_inline_formatting_in_text(
                            Token(TokenType.STRIKETHROUGH, strike_content, base_position + i + 2),
                            TokenType.STRIKETHROUGH
                        )
                        tokens.extend(strike_tokens)

                    tokens.append(Token(TokenType.STRIKETHROUGH_END, '~~', base_position + strike_end))

                    i = strike_end + 2
                    current_text_start = i
                    continue

                # No closing strikethrough marker; treat the ~~ as plain text
                current_text_start = i

            # No formatting found, move to next character
            i += 1

        # Add any remaining text
        add_text_token(current_text_start, len(text))

        return tokens

    def _is_list_marker(self, text: str) -> bool:
        """
        Return True if text begins with a valid unordered or ordered list marker.

        Mirrors the detection logic in MarkdownLexer so that content lines that
        contain a list item are recognised consistently regardless of surrounding
        blockquote context.

        Args:
            text: The content string to test (leading whitespace already stripped)

        Returns:
            True if the text starts with a list marker followed by whitespace
        """
        if not text:
            return False

        if text[0] in ('-', '*', '+') and len(text) > 1 and text[1] == ' ':
            return True

        # Ordered list: one or more digits followed by '.' or ')' then a space
        i = 0
        while i < len(text) and text[i].isdigit():
            i += 1

        if 0 < i < len(text) and text[i] in ('.', ')') and i + 1 < len(text) and text[i + 1] == ' ':
            return True

        return False

    def _create_parser_state(
        self,
        in_fence_block: bool,
        fence_depth: int,
        nested_fence_depth: int,
        fence_blockquote_depth: int,
        language: ProgrammingLanguage,
        block_stack: list[BlockContext],
        embedded_parser_state: ParserState | None,
    ) -> MarkdownParserState:
        """Helper to create a MarkdownParserState from individual fields."""
        parser_state = MarkdownParserState()
        parser_state.in_fence_block = in_fence_block
        parser_state.fence_depth = fence_depth
        parser_state.nested_fence_depth = nested_fence_depth
        parser_state.fence_blockquote_depth = fence_blockquote_depth
        parser_state.language = language
        parser_state.block_stack = block_stack
        parser_state.embedded_parser_state = embedded_parser_state
        return parser_state

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> MarkdownParserState:
        """
        Parse one line of Markdown content.

        Args:
            prev_parser_state: Parser state carried over from the previous line,
                or None for the first line.
            input_str: The line to parse (no trailing newline expected).

        Returns:
            The updated parser state after parsing this line.
        """
        self._tokens = []
        self._next_token = 0

        # Unpack previous state
        in_fence_block = False
        fence_depth = 0
        nested_fence_depth = 0
        fence_blockquote_depth = 0
        language = ProgrammingLanguage.UNKNOWN
        embedded_parser_state = None
        parsing_continuation = False
        block_stack: list[BlockContext] = []

        if prev_parser_state is not None:
            assert isinstance(prev_parser_state, MarkdownParserState), \
                f"Expected MarkdownParserState, got {type(prev_parser_state).__name__}"

            in_fence_block = prev_parser_state.in_fence_block
            fence_depth = prev_parser_state.fence_depth
            nested_fence_depth = prev_parser_state.nested_fence_depth
            fence_blockquote_depth = prev_parser_state.fence_blockquote_depth
            language = prev_parser_state.language
            embedded_parser_state = prev_parser_state.embedded_parser_state
            parsing_continuation = prev_parser_state.parsing_continuation
            block_stack = prev_parser_state.block_stack.copy()

        # Step 1: strip leading blockquote prefixes and emit BLOCKQUOTE tokens.
        #
        # We always do this, even when inside a fence block, so that the '> '
        # markers that were present when the fence opened are removed before the
        # content is handed to the embedded parser.  We only strip as many levels
        # as were present when the fence was opened (fence_blockquote_depth) so
        # that any '>' characters that are genuine code content are preserved.
        if in_fence_block and fence_blockquote_depth > 0:
            # Strip exactly the number of blockquote levels that were active when
            # the fence opened, preserving any deeper '>' as code content.
            remaining = input_str
            bq_offset = 0
            bq_tokens_count = 0
            for _ in range(fence_blockquote_depth):
                stripped = remaining.lstrip()
                spaces_before = len(remaining) - len(stripped)
                if not stripped.startswith('>'):
                    break

                after_marker = stripped[1:]
                if after_marker.startswith(' '):
                    consumed = spaces_before + 2

                else:
                    consumed = spaces_before + 1

                token_value = remaining[:consumed]
                self._tokens.append(Token(type=TokenType.BLOCKQUOTE, value=token_value, start=bq_offset))
                bq_offset += consumed
                remaining = remaining[consumed:]
                bq_tokens_count += 1

            content_offset = bq_offset

        else:
            bq_tokens, remaining, content_offset = self._strip_blockquote_prefixes(input_str)
            self._tokens.extend(bq_tokens)
            bq_tokens_count = len(bq_tokens)

        # Step 2: if we are inside a fence block, delegate to the embedded parser.
        if in_fence_block and not parsing_continuation:
            # Check whether this line closes the fence.  A closing fence is '```'
            # (optionally indented) with nothing else on the line, at an indentation
            # no greater than the opening fence.
            content_stripped = remaining.lstrip()
            content_indent = content_offset + (len(remaining) - len(content_stripped))
            relative_indent = len(remaining) - len(content_stripped)

            if content_stripped.startswith('```'):
                rest = content_stripped[3:].strip()
                if not rest:
                    # Candidate closing fence — check against nested depth and indent.
                    embedded_in_fence = False
                    if embedded_parser_state and isinstance(embedded_parser_state, MarkdownParserState):
                        embedded_in_fence = embedded_parser_state.in_fence_block

                    if not embedded_in_fence:
                        if relative_indent <= fence_depth:
                            # This closes our fence.
                            self._tokens.append(Token(type=TokenType.FENCE_END, value='```', start=content_indent))
                            return self._create_parser_state(
                                False, 0, 0, 0,
                                ProgrammingLanguage.UNKNOWN,
                                block_stack, None
                            )

                        # More indented than the opening fence — treat as content.

                    elif nested_fence_depth > 0:
                        nested_fence_depth -= 1
                        # Fall through to delegate as content.

                elif nested_fence_depth > 0 or rest:
                    # Has a language tag or we're tracking nested fences — content.
                    if rest:
                        nested_fence_depth += 1

            # Delegate remaining content to the embedded parser.
            new_embedded_state = self._embedded_parse(language, embedded_parser_state, remaining, content_offset)
            parser_state = self._create_parser_state(
                in_fence_block, fence_depth, nested_fence_depth, fence_blockquote_depth,
                language, block_stack, embedded_parser_state
            )
            parser_state.embedded_parser_state = new_embedded_state
            if new_embedded_state is not None:
                parser_state.continuation_state = new_embedded_state.continuation_state
                parser_state.parsing_continuation = new_embedded_state.parsing_continuation

            return parser_state

        # Step 3: handle multi-line continuation inside the embedded parser
        # (e.g. a string literal that spans lines).
        if parsing_continuation:
            new_embedded_state = self._embedded_parse(language, embedded_parser_state, remaining, content_offset)
            parser_state = self._create_parser_state(
                in_fence_block, fence_depth, nested_fence_depth, fence_blockquote_depth,
                language, block_stack, embedded_parser_state
            )
            parser_state.embedded_parser_state = new_embedded_state
            if new_embedded_state is not None:
                parser_state.continuation_state = new_embedded_state.continuation_state
                parser_state.parsing_continuation = new_embedded_state.parsing_continuation

            return parser_state

        # Step 4: not in a fence block — lex and classify the stripped content.
        #
        # Reconcile the block stack with the observed blockquote depth for this line.
        # The number of leading '>' markers stripped in Step 1 tells us the current
        # blockquote nesting level.  We pop stack entries that belong to a deeper
        # blockquote context (both the excess BLOCKQUOTE entries and any LIST_MARKER
        # entries nested inside them), then push new BLOCKQUOTE entries if the depth
        # has increased.
        current_bq_depth = sum(1 for ctx in block_stack if ctx.type == TokenType.BLOCKQUOTE)

        if bq_tokens_count < current_bq_depth:
            # Blockquote depth decreased — trim the stack down to the new depth,
            # discarding both the excess BLOCKQUOTE entries and any LIST_MARKER
            # entries that were nested inside them.
            new_stack: list[BlockContext] = []
            remaining_bq = bq_tokens_count
            for ctx in block_stack:
                if ctx.type == TokenType.BLOCKQUOTE:
                    if remaining_bq > 0:
                        new_stack.append(ctx)
                        remaining_bq -= 1

                    else:
                        break

                else:
                    new_stack.append(ctx)

            block_stack = new_stack

        elif bq_tokens_count > current_bq_depth:
            # Blockquote depth increased — push new BLOCKQUOTE entries.
            # If we are entering a blockquote from depth 0, any list context
            # that existed outside any blockquote is a separate block and must
            # be cleared before we push the new blockquote level.
            if current_bq_depth == 0:
                block_stack = [ctx for ctx in block_stack if ctx.type != TokenType.LIST_MARKER]

            for _ in range(bq_tokens_count - current_bq_depth):
                block_stack.append(BlockContext(type=TokenType.BLOCKQUOTE))

        lexer = MarkdownLexer()
        lexer.lex(None, remaining)

        parse_embedded = False
        fence_just_opened = False

        while True:
            token = lexer.get_next_token()
            if token is None:
                break

            # Adjust token positions to be relative to the original line.
            token = Token(type=token.type, value=token.value, start=token.start + content_offset)

            if token.type == TokenType.FENCE:
                if in_fence_block:
                    # Check if this opens a nested fence (has a language tag following).
                    next_token = lexer.peek_next_token()
                    if next_token and next_token.type == TokenType.TEXT:
                        nested_fence_depth += 1
                        break  # delegate to embedded parser

                    if nested_fence_depth > 0:
                        nested_fence_depth -= 1
                        break  # delegate to embedded parser

                    if token.start - content_offset > fence_depth:
                        # More indented than opening fence — treat as content.
                        token = Token(type=TokenType.TEXT, value=token.value, start=token.start)
                        self._tokens.append(token)
                        continue

                    # Close the fence.
                    token = Token(type=TokenType.FENCE_END, value=token.value, start=token.start)
                    self._tokens.append(token)
                    in_fence_block = False
                    fence_blockquote_depth = 0
                    fence_depth = 0
                    nested_fence_depth = 0
                    language = ProgrammingLanguage.UNKNOWN
                    embedded_parser_state = None
                    parse_embedded = False
                    break

                # Open a new fence.  A fence at or outside the innermost list indent
                # pops list entries from the stack (the fence is not a list
                # continuation), but blockquote entries are preserved.
                in_fence_block = True
                fence_depth = token.start - content_offset
                fence_just_opened = True
                fence_blockquote_depth = bq_tokens_count
                embedded_parser_state = None

                list_indent = next(
                    (ctx.indent for ctx in reversed(block_stack) if ctx.type == TokenType.LIST_MARKER),
                    None
                )
                if list_indent is None or fence_depth < list_indent:
                    block_stack = [ctx for ctx in block_stack if ctx.type != TokenType.LIST_MARKER]

                token = Token(type=TokenType.FENCE_START, value=token.value, start=token.start)
                self._tokens.append(token)

                next_token = lexer.peek_next_token()
                if next_token and next_token.type == TokenType.TEXT:
                    lexer.get_next_token()  # consume
                    lang_token = Token(
                        type=TokenType.LANGUAGE,
                        value=next_token.value,
                        start=next_token.start + content_offset
                    )
                    self._tokens.append(lang_token)
                    language = ProgrammingLanguageUtils.from_name(next_token.value)

                else:
                    language = ProgrammingLanguage.TEXT

                parse_embedded = True
                break

            if token.type == TokenType.HEADING:
                # A heading resets any list context but preserves blockquote context.
                block_stack = [ctx for ctx in block_stack if ctx.type != TokenType.LIST_MARKER]
                self._tokens.append(token)
                continue

            if token.type == TokenType.LIST_MARKER:
                # The LIST_MARKER token now covers only the marker prefix (e.g.
                # "* " or "1. "), so the content indent is simply the position
                # immediately after the token.
                current_indent = token.start + len(token.value)

                # Find the innermost LIST_MARKER entry in the stack (if any).
                list_ctx_index = next(
                    (i for i in range(len(block_stack) - 1, -1, -1)
                     if block_stack[i].type == TokenType.LIST_MARKER),
                    None
                )

                if list_ctx_index is None:
                    # No list active yet — start one.
                    block_stack.append(BlockContext(type=TokenType.LIST_MARKER, indent=current_indent))

                else:
                    top_indent = block_stack[list_ctx_index].indent
                    if current_indent > top_indent:
                        # Deeper indent — push a nested list level.
                        block_stack.append(BlockContext(type=TokenType.LIST_MARKER, indent=current_indent))

                    elif current_indent < top_indent:
                        # Shallower indent — pop list levels until we match or exhaust.
                        while (block_stack and
                               block_stack[-1].type == TokenType.LIST_MARKER and
                               block_stack[-1].indent > current_indent):
                            block_stack.pop()

                        if not block_stack or block_stack[-1].type != TokenType.LIST_MARKER:
                            block_stack.append(BlockContext(type=TokenType.LIST_MARKER, indent=current_indent))

                    # else: same indent — stay on the current level (no push/pop needed)

                self._tokens.append(token)
                continue

            # Content token following a list marker or on a continuation line.
            # Only plain TEXT tokens are retyped to LIST_MARKER — structural tokens
            # (TABLE, HEADING, HORIZONTAL_RULE, FENCE, BLOCKQUOTE) keep their own
            # type so the highlighter can colour them appropriately.
            list_ctx = next(
                (ctx for ctx in reversed(block_stack) if ctx.type == TokenType.LIST_MARKER),
                None
            )
            if list_ctx is not None:
                if token.start >= list_ctx.indent:
                    # Content is indented enough to be list content.  Retype only
                    # plain TEXT tokens; structural tokens keep their own type.
                    if token.type == TokenType.TEXT:
                        token = Token(type=TokenType.LIST_MARKER, value=token.value, start=token.start)

                    self._tokens.append(token)
                    continue

                # Content is less indented than the list — pop list levels.
                while (block_stack and
                       block_stack[-1].type == TokenType.LIST_MARKER and
                       block_stack[-1].indent > token.start):
                    block_stack.pop()

            self._tokens.append(token)

        # Step 5: inline formatting pass over non-fence tokens.
        # BLOCKQUOTE, LIST_MARKER, and HEADING tokens are passed with their own
        # type so the highlighter colours their content consistently.
        # TEXT tokens use the innermost block context type from the stack so that
        # plain text content inside a blockquote or list continuation is coloured
        # to match its container rather than appearing as unstyled TEXT.
        if not parse_embedded:
            innermost_type: TokenType | None = next(
                (ctx.type for ctx in reversed(block_stack)),
                None
            )
            processed_tokens: list[Token] = []
            for token in self._tokens:
                if token.type in (TokenType.TEXT, TokenType.HEADING, TokenType.BLOCKQUOTE, TokenType.LIST_MARKER):
                    if token.type == TokenType.TEXT and innermost_type is not None:
                        block_type = innermost_type

                    else:
                        block_type = token.type

                    inline_tokens = self._parse_inline_formatting_in_text(token, block_type)
                    processed_tokens.extend(inline_tokens)

                else:
                    processed_tokens.append(token)

            self._tokens = processed_tokens

        # Step 6: build and return the new parser state.
        parser_state = self._create_parser_state(
            in_fence_block, fence_depth, nested_fence_depth, fence_blockquote_depth,
            language, block_stack, embedded_parser_state
        )

        if parse_embedded and not fence_just_opened:
            new_embedded_state = self._embedded_parse(language, embedded_parser_state, remaining, content_offset)
            parser_state.embedded_parser_state = new_embedded_state
            if new_embedded_state is not None:
                parser_state.continuation_state = new_embedded_state.continuation_state
                parser_state.parsing_continuation = new_embedded_state.parsing_continuation

        return parser_state
