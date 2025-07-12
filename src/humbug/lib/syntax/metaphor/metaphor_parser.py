from dataclasses import dataclass

from humbug.lib.syntax.lexer import TokenType
from humbug.lib.syntax.metaphor.metaphor_lexer import MetaphorLexer
from humbug.lib.syntax.parser import Parser, ParserState
from humbug.lib.syntax.parser_registry import ParserRegistry
from humbug.lib.syntax.programming_language import ProgrammingLanguage
from humbug.lib.syntax.programming_language_utils import ProgrammingLanguageUtils


@dataclass
class MetaphorParserState(ParserState):
    """
    State information for the Conversation parser.

    Attributes:
        in_fence_block: Indicates if we're currently in a code fence block
        language: The current programming language being parsed
        embedded_parser_state: State of the embedded language parser
    """
    in_fence_block: bool = False
    language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN
    embedded_parser_state: ParserState | None = None


@ParserRegistry.register_parser(ProgrammingLanguage.METAPHOR)
class MetaphorParser(Parser):
    """
    Parser for Metaphor language.

    This parser processes tokens from the Metaphor lexer and handles the
    document tree structure of Role, Context, and Action blocks.
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

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> MetaphorParserState:
        """
        Parse the input string.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            Handles transitions between regular conversation content and code fence blocks,
            delegating code blocks to appropriate language parsers.
        """
        in_fence_block = False
        language = ProgrammingLanguage.UNKNOWN
        embedded_parser_state = None
        parsing_continuation = False
        if prev_parser_state:
            assert isinstance(prev_parser_state, MetaphorParserState), \
                f"Expected MetaphorParserState, got {type(prev_parser_state).__name__}"
            in_fence_block = prev_parser_state.in_fence_block
            language = prev_parser_state.language
            embedded_parser_state = prev_parser_state.embedded_parser_state
            parsing_continuation = prev_parser_state.parsing_continuation

        parse_embedded = language != ProgrammingLanguage.UNKNOWN
        continuation_state = 0

        if not parsing_continuation:
            lexer = MetaphorLexer()
            lexer.lex(None, input_str)

            while True:
                lex_token = lexer.get_next_token()
                if not lex_token:
                    break

                if lex_token.type == TokenType.FENCE:
                    if in_fence_block:
                        lex_token.type = TokenType.FENCE_END
                        self._tokens.append(lex_token)
                        in_fence_block = False
                        language = ProgrammingLanguage.UNKNOWN
                        embedded_parser_state = None
                        parse_embedded = False
                        continue

                    in_fence_block = True
                    embedded_parser_state = None
                    lex_token.type = TokenType.FENCE_START
                    self._tokens.append(lex_token)

                    next_token = lexer.peek_next_token()
                    if next_token and (next_token.type == TokenType.TEXT):
                        lexer.get_next_token()  # Consume the text token
                        next_token.type = TokenType.LANGUAGE
                        self._tokens.append(next_token)

                        language = ProgrammingLanguageUtils.from_name(next_token.value)
                        continuation_state = int(language)
                        continue

                    language = ProgrammingLanguage.TEXT
                    continuation_state = int(language)
                    continue

                if language != ProgrammingLanguage.UNKNOWN:
                    break

                self._tokens.append(lex_token)

        parser_state = MetaphorParserState()
        parser_state.continuation_state = continuation_state
        parser_state.in_fence_block = in_fence_block
        parser_state.language = language
        if parse_embedded:
            new_embedded_parser_state = self._embedded_parse(parser_state.language, embedded_parser_state, input_str)
            parser_state.embedded_parser_state = new_embedded_parser_state
            if new_embedded_parser_state is not None:
                parser_state.continuation_state = new_embedded_parser_state.continuation_state
                parser_state.parsing_continuation = new_embedded_parser_state.parsing_continuation

        return parser_state
