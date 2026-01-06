from dataclasses import dataclass

from syntax.xml.xml_lexer import XMLLexer
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage


@dataclass
class XMLParserState(ParserState):
    """
    State information for the XML parser.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.XML)
class XMLParser(Parser):
    """
    Parser for XML code.

    This parser processes tokens from the XML lexer and handles special cases
    like comments, CDATA sections, and processing instructions.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> XMLParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        prev_lexer_state = None
        if prev_parser_state:
            assert isinstance(prev_parser_state, XMLParserState), \
                f"Expected XMLParserState, got {type(prev_parser_state).__name__}"
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = XMLLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        continuation_state = 0
        if lexer_state.in_comment:
            continuation_state = 1
        elif lexer_state.in_cdata:
            continuation_state = 2
        elif lexer_state.in_processing_instruction:
            continuation_state = 3

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            self._tokens.append(token)

        parser_state = XMLParserState()
        parser_state.continuation_state = continuation_state
        parser_state.lexer_state = lexer_state
        return parser_state
