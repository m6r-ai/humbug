from abc import ABC
from typing import Optional, List


from humbug.syntax.lexer import Lexer, Token


class Parser(ABC):
    def __init__(self):
        self._lexer: Optional[Lexer] = None
        self._tokens: List[Token] = []
        self._next_token: int = 0

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
