from typing import Optional

from humbug.syntax.markdown_lexer import MarkdownLexer
from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser


class MarkdownParser(Parser):
    def __init__(self, input_str: str):
        super().__init__()
        self._lexer = MarkdownLexer(input_str)

    def get_next_token(self) -> Optional[Token]:
        if self._lexer is None:
            raise ValueError("Lexer has not been initialized.")

        return self._lexer.get_next_token()
