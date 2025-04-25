from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@ParserRegistry.register_parser(ProgrammingLanguage.TEXT)
class TextParser(Parser):
    """
    Parser for text.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> None:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse
        """
        self._tokens.append(Token(
            type=TokenType.CODE,
            value=input_str,
            start=0
        ))
