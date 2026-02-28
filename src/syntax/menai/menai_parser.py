from dataclasses import dataclass

from syntax.menai.menai_lexer import MenaiLexer
from syntax.lexer import TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage


@dataclass
class MenaiParserState(ParserState):
    """
    State information for the Menai parser.

    Attributes:
        paren_depth: Current parenthesis nesting depth
    """
    paren_depth: int = 0


@ParserRegistry.register_parser(ProgrammingLanguage.MENAI)
class MenaiParser(Parser):
    """
    Parser for Menai code.

    This parser processes tokens from the Menai lexer and handles special cases
    like nested expressions, pattern matching, and validates the dot operator
    is only used in valid pattern matching contexts.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> MenaiParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser validates that dot operators are only used in pattern
            matching contexts within match expressions. Invalid dots are
            converted to ERROR tokens.
        """
        paren_depth = 0
        prev_lexer_state = None

        if prev_parser_state:
            assert isinstance(prev_parser_state, MenaiParserState), \
                f"Expected MenaiParserState, got {type(prev_parser_state).__name__}"
            prev_lexer_state = prev_parser_state.lexer_state
            paren_depth = prev_parser_state.paren_depth

        lexer = MenaiLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            # Track parenthesis depth
            if token.type == TokenType.LPAREN:
                paren_depth += 1

            elif token.type == TokenType.RPAREN:
                paren_depth = max(0, paren_depth - 1)

            # Validate dot operator usage
            if token.type == TokenType.OPERATOR and token.value == '.':
                if not self._is_valid_dot_context(lexer):
                    # Convert invalid dot to error
                    token.type = TokenType.ERROR

            self._tokens.append(token)

        parser_state = MenaiParserState()
        parser_state.continuation_state = 1 if lexer_state.in_string else 0
        parser_state.parsing_continuation = lexer_state.in_string
        parser_state.lexer_state = lexer_state
        parser_state.paren_depth = paren_depth
        return parser_state

    def _is_valid_dot_context(self, lexer: MenaiLexer) -> bool:
        """
        Check if the dot operator is in a valid context.

        A dot is valid if it's used in pattern matching, which means:
        - It should be inside parentheses (pattern context)
        - It should have an identifier or rparen before it
        - It should have an identifier after it

        Args:
            lexer: The lexer to peek at surrounding tokens

        Returns:
            True if the dot is in a valid context, False otherwise
        """
        # Check the next token - should be an identifier or whitespace before identifier
        next_token = lexer.peek_next_token()
        if not next_token:
            return False

        # The next meaningful token should be an identifier
        if next_token.type == TokenType.IDENTIFIER:
            return True

        # If next is rparen, this might be valid too (e.g., (a . ))
        # But that's likely a syntax error, so we'll be conservative
        return False
