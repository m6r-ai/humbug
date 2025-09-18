from dataclasses import dataclass

from syntax.aifpl.aifpl_lexer import AIFPLLexer
from syntax.lexer import TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage


@dataclass
class AIFPLParserState(ParserState):
    """
    State information for the AIFPL parser.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.AIFPL)
class AIFPLParser(Parser):
    """
    Parser for AIFPL code.

    This parser processes tokens from the AIFPL lexer and handles special cases
    like nested expressions and vectors.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> AIFPLParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        lexer = AIFPLLexer()
        lexer_state = lexer.lex(None, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            # Handle opening parentheses
            if token.type == TokenType.LPAREN:
                self._tokens.append(token)

                next_token = lexer.peek_next_token()
                if next_token and next_token.type == TokenType.IDENTIFIER:
                    lexer.get_next_token()  # Consume the identifier
                    next_token.type = TokenType.FUNCTION_OR_METHOD
                    self._tokens.append(next_token)
                    continue

                continue

            self._tokens.append(token)

        parser_state = AIFPLParserState()
        parser_state.lexer_state = lexer_state
        return parser_state
