from dataclasses import dataclass

from humbug.syntax.c.c_lexer import CLexer
from humbug.syntax.lexer import TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class CParserState(ParserState):
    """
    State information for the C parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element
    """
    in_element: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.C)
class CParser(Parser):
    """
    Parser for C code.

    This parser processes tokens from the C lexer and handles special cases
    like function calls and element access.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> CParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state of type CParserState.
                If provided with a different ParserState subclass, TypeError will be raised.
            input_str: The input string to parse

        Returns:
            The updated CParserState after parsing

        Raises:
            TypeError: If prev_parser_state is not None and not a CParserState instance

        Note:
            The parser converts identifier tokens to FUNCTION_OR_METHOD tokens
            when they're followed by parentheses, and to ELEMENT tokens when
            they're part of a dotted or arrow access chain.
        """
        in_element = False
        prev_lexer_state = None

        if prev_parser_state is not None:
            if not isinstance(prev_parser_state, CParserState):
                raise TypeError(f"Expected CParserState, got {type(prev_parser_state).__name__}")

            in_element = prev_parser_state.in_element
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = CLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type != TokenType.IDENTIFIER:
                self._tokens.append(token)
                continue

            # Look at the next token. If it's a '(' operator then we're making a
            # function or method call!
            cur_in_element = in_element
            next_token = lexer.peek_next_token()
            in_element = cur_in_element

            next_in_element = False
            if next_token and next_token.type == TokenType.OPERATOR:
                if next_token.value == '(':
                    in_element = False
                    token.type = TokenType.FUNCTION_OR_METHOD
                    self._tokens.append(token)
                    continue

                # Is the next token going to be an element?
                if next_token.value in ('.', '->'):
                    next_in_element = True

            in_element = next_in_element

            if cur_in_element:
                token.type = TokenType.ELEMENT
                self._tokens.append(token)
                continue

            self._tokens.append(token)

        parser_state = CParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.parsing_continuation = lexer_state.in_block_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        return parser_state
