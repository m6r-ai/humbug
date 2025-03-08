from dataclasses import dataclass
from typing import Optional

from humbug.syntax.json.json_lexer import JSONLexer
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class JSONParserState(ParserState):
    """
    State information for the JSON parser.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.JSON)
class JSONParser(Parser):
    """
    Parser for JSON.

    This parser processes tokens from the JSON lexer and validates
    JSON syntax.
    """

    def __init__(self):
        super().__init__()
        self._lexer = JSONLexer()

    def parse(self, prev_parser_state: Optional[JSONParserState], input_str: str) -> JSONParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        self._lexer.lex(None, input_str)

        while True:
            token = self._lexer.get_next_token()
            if not token:
                break

            self._tokens.append(token)

        return JSONParserState()
