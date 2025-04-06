from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional, List

from humbug.syntax.lexer import Lexer, LexerState, Token


@dataclass
class ParserState:
    """
    State information for the Parser.
    """
    lexer_state: Optional[LexerState] = None
    parsing_continuation: bool = False
    continuation_state: int = 0


class Parser(ABC):
    def __init__(self) -> None:
        self._lexer: Optional[Lexer] = None
        self._tokens: List[Token] = []
        self._next_token: int = 0

    @abstractmethod
    def parse(self, prev_parser_state: Optional[ParserState], input_str: str) -> ParserState:
        """
        Parse the input string
        """

    def get_next_token(self) -> Optional[Token]:
        """
        Gets the next token from the input.

        :return: The next Token available or None if there are no tokens left.
        """
        if self._next_token >= len(self._tokens):
            return None

        token = self._tokens[self._next_token]
        self._next_token += 1
        return token
