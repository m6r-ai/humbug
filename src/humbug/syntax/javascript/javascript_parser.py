from dataclasses import dataclass
from typing import Optional

from humbug.syntax.javascript.javascript_lexer import JavaScriptLexer
from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class JavaScriptParserState(ParserState):
    """
    State information for the JavaScript parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element
    """
    in_element: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.JAVASCRIPT)
class JavaScriptParser(Parser):
    """
    Parser for JavaScript code.

    This parser processes tokens from the JavaScript lexer and handles special cases
    like function calls and element access.
    """

    def __init__(self):
        super().__init__()
        self._lexer = JavaScriptLexer()

    def parse(self, prev_parser_state: Optional[JavaScriptParserState], input_str: str) -> JavaScriptParserState:
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
            they're part of a dotted access chain.
        """
        in_element = False
        prev_lexer_state = None
        if prev_parser_state:
            in_element = prev_parser_state.in_element
            prev_lexer_state = prev_parser_state.lexer_state

        lexer_state = self._lexer.lex(prev_lexer_state, input_str)

        while True:
            token = self._lexer.get_next_token()
            if not token:
                break

            if token.type != TokenType.IDENTIFIER:
                if (token.type == TokenType.OPERATOR and
                        token.value not in ('.', '?.')):
                    in_element = False
                    self._tokens.append(token)
                    continue

                if token.type != TokenType.KEYWORD:
                    self._tokens.append(token)
                    continue

                if token.value != 'this' and not in_element:
                    self._tokens.append(token)
                    continue

            # Look at the next token. If it's a '(' operator then we're making a
            # function or method call!
            cur_in_element = in_element
            next_token = self._lexer.peek_next_token([TokenType.WHITESPACE])
            in_element = cur_in_element

            next_in_element = False
            if next_token and next_token.type == TokenType.OPERATOR:
                if next_token.value == '(':
                    in_element = False
                    self._tokens.append(Token(
                        type=TokenType.FUNCTION_OR_METHOD,
                        value=token.value,
                        start=token.start
                    ))
                    continue

                # Is the next token going to be an element?
                if next_token.value in ('.', '?.'):
                    next_in_element = True

            in_element = next_in_element

            if cur_in_element:
                self._tokens.append(Token(
                    type=TokenType.ELEMENT,
                    value=token.value,
                    start=token.start
                ))
                continue

            self._tokens.append(token)

        parser_state = JavaScriptParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.parsing_continuation = lexer_state.in_block_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        return parser_state
