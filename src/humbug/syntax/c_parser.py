from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.c_lexer import CLexer


@dataclass
class CParserState(ParserState):
    """
    State information for the C parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element
    """
    in_element: bool = False


class CParser(Parser):
    """
    Parser for C code.

    This parser processes tokens from the C lexer and handles special cases
    like function calls and element access.
    """

    def parse(self, prev_parser_state: Optional[CParserState], input_str: str) -> CParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser converts identifier tokens to FUNCTION_OR_METHOD tokens
            when they're followed by parentheses, and to ELEMENT tokens when
            they're part of a dotted or arrow access chain.
        """
        parser_state = CParserState()
        prev_lexer_state = None

        if prev_parser_state:
            parser_state.in_element = prev_parser_state.in_element
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = CLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.lexer_state = lexer_state

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type != 'IDENTIFIER':
                self._tokens.append(token)
                continue

            # Look at the next token. If it's a '(' operator then we're making a
            # function or method call!
            cur_in_element = parser_state.in_element
            next_token = lexer.peek_next_token(['WHITESPACE'])
            parser_state.in_element = cur_in_element

            next_in_element = False
            if next_token and next_token.type == 'OPERATOR':
                if next_token.value == '(':
                    parser_state.in_element = False
                    self._tokens.append(Token(
                        type='FUNCTION_OR_METHOD',
                        value=token.value,
                        start=token.start
                    ))
                    continue

                # Is the next token going to be an element?
                if next_token.value in ('.', '->'):
                    next_in_element = True

            parser_state.in_element = next_in_element

            if cur_in_element:
                self._tokens.append(Token(
                    type='ELEMENT',
                    value=token.value,
                    start=token.start
                ))
                continue

            self._tokens.append(token)

        return parser_state
