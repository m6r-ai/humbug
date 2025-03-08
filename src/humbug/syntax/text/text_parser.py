from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class TextParserState(ParserState):
    """
    State information for the Text parser.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.TEXT)
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
        self._tokens = []
        self._next_token = 0

        self._tokens.append(Token(
            type=TokenType.CODE,
            value=input_str,
            start=0
        ))

        parser_state = TextParserState()
        return parser_state
