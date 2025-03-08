from dataclasses import dataclass
from typing import Optional

from humbug.syntax.css.css_lexer import CSSLexer
from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class CSSParserState(ParserState):
    """
    State information for the CSS parser.

    This maintains parsing context information that may be needed across
    multiple parse operations.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.CSS)
class CSSParser(Parser):
    """
    Parser for CSS code.

    This parser processes tokens from the CSS lexer and handles special cases
    like function calls and property values.
    """

    def __init__(self):
        super().__init__()
        self._lexer = CSSLexer()

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
        self._tokens = []
        self._next_token = 0

        prev_lexer_state = None
        if prev_parser_state:
            prev_lexer_state = prev_parser_state.lexer_state

        lexer_state = self._lexer.lex(prev_lexer_state, input_str)

        while True:
            token = self._lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.HEX or token.type == TokenType.DIMENSION:
                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=token.value,
                    start=token.start
                ))
                continue

            if token.type == TokenType.HASH:
                self._tokens.append(Token(
                    type=TokenType.IDENTIFIER,
                    value=token.value,
                    start=token.start
                ))
                continue

            if token.type != TokenType.IDENTIFIER:
                self._tokens.append(token)
                continue

            # Look at the next token. If it's a '(' operator then we're making a
            # function call!
            next_token = self._lexer.peek_next_token([TokenType.WHITESPACE])
            if next_token and next_token.type == TokenType.OPERATOR:
                if next_token.value == '(':
                    self._tokens.append(Token(
                        type=TokenType.FUNCTION_OR_METHOD,
                        value=token.value,
                        start=token.start
                    ))
                    continue

            self._tokens.append(token)

        parser_state = CSSParserState()
        parser_state.continuation_state = 1 if lexer_state.in_comment else 0
        parser_state.parsing_continuation = lexer_state.in_comment
        parser_state.lexer_state = lexer_state
        return parser_state
