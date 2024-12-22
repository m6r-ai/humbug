from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState


@dataclass
class TextParserState(ParserState):
    """
    State information for the Text parser.
    """


class TextParser(Parser):
    """
    Parser for text.
    """

    def parse(self, prev_parser_state: Optional[TextParserState], input_str: str) -> TextParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        parser_state = TextParserState()

        self._tokens.append(Token(
            type='TEXT',
            value=input_str,
            start=0
        ))

        return parser_state