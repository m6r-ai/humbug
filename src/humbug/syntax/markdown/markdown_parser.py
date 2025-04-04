from dataclasses import dataclass
from typing import List

from humbug.syntax.lexer import TokenType, Token
from humbug.syntax.markdown.markdown_lexer import MarkdownLexer
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.programming_language_utils import ProgrammingLanguageUtils


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
        list_indent: The indentation level of the current list item
        block_type: The type of block element we're in (heading, blockquote, list)
    """
    in_fence_block: bool = False
    fence_depth: int = 0
    language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN
    embedded_parser_state: ParserState | None = None
    in_list_item: bool = False
    list_indent: int = 0
    block_type: TokenType | None = None


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

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> MarkdownParserState:
        """
        Parse conversation content including embedded code blocks.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Raises:
            TypeError: If prev_parser_state is not None and not a MarkdownParserState instance

        Note:
            Handles transitions between regular conversation content and code fence blocks,
            delegating code blocks to appropriate language parsers.
        """
        in_fence_block = False
        fence_depth = 0
        language = ProgrammingLanguage.UNKNOWN
        embedded_parser_state = None
        parsing_continuation = False
        in_list_item = False
        list_indent = 0
        block_type = None

        if prev_parser_state is not None:
            if not isinstance(prev_parser_state, MarkdownParserState):
                raise TypeError(f"Expected MarkdownParserState, got {type(prev_parser_state).__name__}")

            in_fence_block = prev_parser_state.in_fence_block
            fence_depth = prev_parser_state.fence_depth
            language = prev_parser_state.language
            embedded_parser_state = prev_parser_state.embedded_parser_state
            parsing_continuation = prev_parser_state.parsing_continuation
            in_list_item = prev_parser_state.in_list_item
            list_indent = prev_parser_state.list_indent
            block_type = prev_parser_state.block_type

        parse_embedded = language != ProgrammingLanguage.UNKNOWN

        if not parsing_continuation:
            lexer = MarkdownLexer()
            lexer.lex(None, input_str)

            seen_text = False

            # Collect all tokens from the lexer
            while True:
                token = lexer.get_next_token()
                if not token:
                    break

                # If we've already proessed something interesting on this line, run to the end of it
                if seen_text:
                    if block_type is not None:
                        token.type = block_type

                    self._tokens.append(token)
                    continue

                seen_text = True

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
                        next_token = lexer.get_next_token()
                        next_token.type = TokenType.LANGUAGE
                        self._tokens.append(next_token)
                        language = ProgrammingLanguageUtils.from_name(next_token.value)
                        break

                    language = ProgrammingLanguage.TEXT
                    break

                if parse_embedded:
                    break

                if token.type in (TokenType.HEADING, TokenType.BLOCKQUOTE):
                    # If we encounter a block element, we need to check if we're in a list item
                    in_list_item = False
                    block_type = token.type
                    list_indent = 0
                    self._tokens.append(token)
                    continue

                if token.type == TokenType.LIST_MARKER:
                    # If we encounter a list marker, we need to check if we're in a list item
                    in_list_item = True
                    block_type = token.type

                    next_token = lexer.peek_next_token()
                    list_indent = 0 if not next_token else next_token.start
                    self._tokens.append(token)
                    continue

                if in_list_item:
                    # Check if this is a continuation of a list item
                    if token.start >= list_indent:
                        token.type = TokenType.LIST_MARKER
                        self._tokens.append(token)
                        continue

                block_type = None
                in_list_item = False
                list_indent = 0
                self._tokens.append(token)

        parser_state = MarkdownParserState()
        parser_state.in_fence_block = in_fence_block
        parser_state.fence_depth = fence_depth
        parser_state.language = language
        parser_state.in_list_item = in_list_item
        parser_state.list_indent = list_indent
        parser_state.block_type = block_type

        if parse_embedded:
            new_embedded_parser_state = self._embedded_parse(parser_state.language, embedded_parser_state, input_str)
            parser_state.embedded_parser_state = new_embedded_parser_state
            if new_embedded_parser_state:
                parser_state.continuation_state = new_embedded_parser_state.continuation_state
                parser_state.parsing_continuation = new_embedded_parser_state.parsing_continuation

        return parser_state
