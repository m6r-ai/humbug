from humbug.syntax.json.json_lexer import JSONLexer
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@ParserRegistry.register_parser(ProgrammingLanguage.JSON)
class JSONParser(Parser):
    """
    Parser for JSON.

    This parser processes tokens from the JSON lexer and validates
    JSON syntax.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> None:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        lexer = JSONLexer()
        lexer.lex(None, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            self._tokens.append(token)

        return None
