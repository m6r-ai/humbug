from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List

from syntax.lexer import Lexer, LexerState, Token


@dataclass
class ParserState:
    """
    State information for the Parser.
    """
    lexer_state: LexerState | None = None
    parsing_continuation: bool = False
    continuation_state: int = 0


class Parser(ABC):
    """
    Abstract base class for all parsers.

    This class provides the basic functionality for parsing input strings
    and managing the state of the parser.
    """
    def __init__(self) -> None:
        self._lexer: Lexer | None = None
        self._tokens: List[Token] = []
        self._next_token: int = 0

    @abstractmethod
    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> ParserState | None:
        """
        Parse the input string
        """

    def get_next_token(self) -> Token | None:
        """
        Gets the next token from the input.

        Returns:
            The next Token available or None if there are no tokens left.
        """
        if self._next_token >= len(self._tokens):
            return None

        token = self._tokens[self._next_token]
        self._next_token += 1
        return token
