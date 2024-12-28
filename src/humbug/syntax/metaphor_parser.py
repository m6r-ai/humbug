from dataclasses import dataclass
from typing import Optional

from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.metaphor_lexer import MetaphorLexer
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.parser_registry import ParserRegistry


@dataclass
class MetaphorParserState(ParserState):
    """
    State information for the Metaphor parser.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.METAPHOR)
class MetaphorParser(Parser):
    """
    Parser for Metaphor language.

    This parser processes tokens from the Metaphor lexer and handles the
    document tree structure of Role, Context, and Action blocks.
    """

    def parse(self, prev_parser_state: Optional[MetaphorParserState], input_str: str) -> MetaphorParserState:
        """
        Parse the input string.

        Args:
            input_str: The input string to parse

        Raises:
            ValueError: If the lexer has not been initialized
        """
        lexer = MetaphorLexer()
        lexer.lex(None, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            self._tokens.append(token)

        parser_state = MetaphorParserState()
        return parser_state
