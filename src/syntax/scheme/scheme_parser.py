from dataclasses import dataclass

from syntax.lexer import TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.scheme.scheme_lexer import SchemeLexer


@dataclass
class SchemeParserState(ParserState):
    """
    State information for the Scheme parser.

    Attributes:
        in_vector: Whether we're currently parsing a vector
    """
    in_vector: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.SCHEME)
class SchemeParser(Parser):
    """
    Parser for R5RS Scheme code.

    This parser processes tokens from the Scheme lexer and handles special cases
    like nested expressions and vectors.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> SchemeParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        in_vector = False
        prev_lexer_state = None
        continuation_state = 0

        if prev_parser_state:
            assert isinstance(prev_parser_state, SchemeParserState), \
                f"Expected SchemeParserState, got {type(prev_parser_state).__name__}"
            in_vector = prev_parser_state.in_vector
            prev_lexer_state = prev_parser_state.lexer_state
            continuation_state = prev_parser_state.continuation_state

        lexer = SchemeLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            # Handle vector start
            if token.type == TokenType.VECTOR_START:
                in_vector = True
                continuation_state += 1
                self._tokens.append(token)
                continue

            # Handle closing parentheses
            if token.type == 'RPAREN':
                if continuation_state > 0:
                    continuation_state -= 1
                    if continuation_state == 0:
                        in_vector = False

                self._tokens.append(token)
                continue

            if token.type == 'DOT':
                token.type = TokenType.OPERATOR
                self._tokens.append(token)
                continue

            self._tokens.append(token)

        parser_state = SchemeParserState()
        parser_state.continuation_state = 1 if lexer_state.in_comment else 0
        parser_state.parsing_continuation = lexer_state.in_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_vector = in_vector
        return parser_state
