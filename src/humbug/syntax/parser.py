from abc import ABC, abstractmethod
from typing import Optional


from humbug.syntax.lexer import Lexer, Token


class Parser(ABC):
    def __init__(self):
        self._lexer: Optional[Lexer] = None

    @abstractmethod
    def get_next_token(self) -> Optional[Token]:
        """
        Gets the next token from the input.

        :return: The next Token available or None if there are no tokens left.
        """
