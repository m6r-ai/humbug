from dataclasses import dataclass

from humbug.lib.syntax.html.html_lexer import HTMLLexer
from humbug.lib.syntax.lexer import TokenType
from humbug.lib.syntax.parser import Parser, ParserState
from humbug.lib.syntax.parser_registry import ParserRegistry
from humbug.lib.syntax.programming_language import ProgrammingLanguage


@dataclass
class HTMLParserState(ParserState):
    """
    State information for the HTML parser.

    Attributes:
        js_parser: Optional JavaScript parser for script content
        css_parser: Optional CSS parser for style content
    """
    embedded_parser_state: ParserState | None = None


@ParserRegistry.register_parser(ProgrammingLanguage.HTML)
class HTMLParser(Parser):
    """
    Parser for HTML code.

    This parser processes tokens from the HTML lexer and handles special cases
    like embedded JavaScript and CSS content.
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

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> HTMLParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser handles embedded JavaScript and CSS content by delegating to
            specialized parsers for those languages.
        """
        prev_lexer_state = None
        embedded_parser_state = None
        if prev_parser_state:
            assert isinstance(prev_parser_state, HTMLParserState), \
                f"Expected HTMLParserState, got {type(prev_parser_state).__name__}"
            prev_lexer_state = prev_parser_state.lexer_state
            embedded_parser_state = prev_parser_state.embedded_parser_state

        lexer = HTMLLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        continuation_state = 0
        if lexer_state.in_comment:
            continuation_state = 1

        elif lexer_state.in_script:
            continuation_state = 2

        elif lexer_state.in_style:
            continuation_state = 3

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.SCRIPT:
                embedded_parser_state = self._embedded_parse(ProgrammingLanguage.JAVASCRIPT, embedded_parser_state, token.value)
                if embedded_parser_state is not None:
                    continuation_state = embedded_parser_state.continuation_state

                continue

            if token.type == TokenType.STYLE:
                embedded_parser_state = self._embedded_parse(ProgrammingLanguage.CSS, embedded_parser_state, token.value)
                if embedded_parser_state is not None:
                    continuation_state = embedded_parser_state.continuation_state

                continue

            embedded_parser_state = None
            self._tokens.append(token)

        parser_state = HTMLParserState()
        parser_state.continuation_state = continuation_state
        parser_state.lexer_state = lexer_state
        parser_state.embedded_parser_state = embedded_parser_state
        return parser_state
