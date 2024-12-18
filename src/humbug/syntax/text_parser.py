from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser


@dataclass
class TextParserState:
    """
    State information for the Text parser.
    """


class TextParser(Parser):
    """
    Parser for text.
    """

    def __init__(self) -> None:
        """Initialize the text parser."""
        super().__init__()
        self._parser_state = TextParserState()

    def parse(self, parser_state: Optional[TextParserState], input_str: str) -> TextParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        self._tokens.append(Token(
            type='TEXT',
            value=input_str,
            start=0
        ))

        new_parser_state = TextParserState()
        return new_parser_state
