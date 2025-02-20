from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.metaphor_lexer import MetaphorLexer
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.parser_registry import ParserRegistry


# Mapping from lowercase language names to enum members
LANGUAGE_MAPPING = {
    "c": ProgrammingLanguage.C,
    "c++": ProgrammingLanguage.CPP,
    "cpp": ProgrammingLanguage.CPP,
    "css": ProgrammingLanguage.CSS,
    "go": ProgrammingLanguage.GO,
    "html": ProgrammingLanguage.HTML,
    "java": ProgrammingLanguage.JAVA,
    "javascript": ProgrammingLanguage.JAVASCRIPT,
    "json": ProgrammingLanguage.JSON,
    "kotlin": ProgrammingLanguage.KOTLIN,
    "metaphor": ProgrammingLanguage.METAPHOR,
    "move": ProgrammingLanguage.MOVE,
    "python": ProgrammingLanguage.PYTHON,
    "rust": ProgrammingLanguage.RUST,
    "scheme": ProgrammingLanguage.SCHEME,
    "swift": ProgrammingLanguage.SWIFT,
    "typescript": ProgrammingLanguage.TYPESCRIPT
}


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
    embedded_parser_state: ParserState = None


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
            prev_embedded_parser_state: ParserState,
            input_str: str
    ) -> ParserState:
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

            self._tokens.append(Token(type=token.type, value=token.value, start=token.start))

        return embedded_parser_state

    def parse(self, prev_parser_state: Optional[MetaphorParserState], input_str: str) -> MetaphorParserState:
        """
        Parse the input string.

        Args:
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

                if lex_token.type == 'WHITESPACE':
                    self._tokens.append(Token(type=lex_token.type, value=lex_token.value, start=lex_token.start))
                    continue

                if lex_token.type == 'FENCE':
                    if in_fence_block:
                        self._tokens.append(Token(type='LANGUAGE', value='```', start=lex_token.start))
                        in_fence_block = False
                        language = ProgrammingLanguage.UNKNOWN
                        embedded_parser_state = None
                        parse_embedded = False
                        continue

                    in_fence_block = True
                    embedded_parser_state = None
                    self._tokens.append(Token(type='LANGUAGE', value='```', start=lex_token.start))

                    next_token = lexer.peek_next_token('WHITESPACE')
                    if next_token and (next_token.type == 'TEXT'):
                        next_token = lexer.get_next_token('WHITESPACE')
                        self._tokens.append(Token(type='LANGUAGE', value=next_token.value, start=next_token.start))

                        input_normalized = next_token.value.strip().lower()
                        language = LANGUAGE_MAPPING.get(input_normalized, ProgrammingLanguage.TEXT)
                        continuation_state = int(language)
                        continue

                    language = LANGUAGE_MAPPING.get('', ProgrammingLanguage.TEXT)
                    continuation_state = int(language)
                    continue

                if language != ProgrammingLanguage.UNKNOWN:
                    break

                self._tokens.append(Token(type=lex_token.type, value=lex_token.value, start=lex_token.start))

        parser_state = MetaphorParserState()
        parser_state.continuation_state = continuation_state
        parser_state.in_fence_block = in_fence_block
        parser_state.language = language
        if parse_embedded:
            new_embedded_parser_state = self._embedded_parse(parser_state.language, embedded_parser_state, input_str)
            parser_state.embedded_parser_state = new_embedded_parser_state
            if new_embedded_parser_state:
                parser_state.continuation_state = new_embedded_parser_state.continuation_state
                parser_state.parsing_continuation = new_embedded_parser_state.parsing_continuation

        return parser_state
