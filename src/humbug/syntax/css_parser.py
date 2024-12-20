from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.css_lexer import CSSLexer


@dataclass
class CSSParserState(ParserState):
    """
    State information for the CSS parser.

    This maintains parsing context information that may be needed across
    multiple parse operations.
    """


class CSSParser(Parser):
    """
    Parser for CSS code.

    This parser processes tokens from the CSS lexer and handles special cases
    like function calls and property values.
    """

    def parse(self, prev_parser_state: Optional[CSSParserState], input_str: str) -> CSSParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser converts identifier tokens to FUNCTION_OR_METHOD tokens
            when they're followed by parentheses.
        """
        parser_state = CSSParserState()

        prev_lexer_state = None

        if prev_parser_state:
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = CSSLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)
        parser_state.continuation_state = 1 if lexer_state.in_comment else 0
        parser_state.lexer_state = lexer_state

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == 'HEX' or token.type == 'DIMENSION':
                self._tokens.append(Token(
                    type='NUMBER',
                    value=token.value,
                    start=token.start
                ))
                continue

            if token.type == 'HASH':
                self._tokens.append(Token(
                    type='IDENTIFIER',
                    value=token.value,
                    start=token.start
                ))
                continue

            if token.type != 'IDENTIFIER':
                self._tokens.append(token)
                continue

            # Look at the next token. If it's a '(' operator then we're making a
            # function call!
            next_token = lexer.peek_next_token(['WHITESPACE'])
            if next_token and next_token.type == 'OPERATOR':
                if next_token.value == '(':
                    self._tokens.append(Token(
                        type='FUNCTION_OR_METHOD',
                        value=token.value,
                        start=token.start
                    ))
                    continue

            self._tokens.append(token)

        return parser_state
