from dataclasses import dataclass
from typing import Optional

from humbug.syntax.go.go_lexer import GoLexer
from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class GoParserState(ParserState):
    """
    State information for the Go parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element access
        in_struct_literal: Indicates if we're parsing a struct literal
    """
    in_element: bool = False
    in_struct_literal: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.GO)
class GoParser(Parser):
    """
    Parser for Go code.

    This parser processes tokens from the Go lexer and handles special cases like:
    - Function calls
    - Method calls
    - Element access
    - Package imports
    - Struct literals
    """

    def __init__(self):
        super().__init__()
        self._lexer = GoLexer()

    def parse(self, prev_parser_state: Optional[GoParserState], input_str: str) -> GoParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser handles:
            - Converting identifiers to FUNCTION_OR_METHOD tokens when followed by parentheses
            - Converting identifiers to ELEMENT tokens in dotted access chains
            - Processing struct literal field names
            - Tracking import statements
        """
        in_element = False
        in_struct_literal = False
        prev_lexer_state = None

        if prev_parser_state:
            in_element = prev_parser_state.in_element
            in_struct_literal = prev_parser_state.in_struct_literal
            prev_lexer_state = prev_parser_state.lexer_state

        lexer_state = self._lexer.lex(prev_lexer_state, input_str)

        while True:
            token = self._lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.KEYWORD:
                if token.value == 'struct':
                    next_token = self._lexer.peek_next_token([TokenType.WHITESPACE])
                    if next_token and next_token.type == TokenType.OPERATOR and next_token.value == '{':
                        in_struct_literal = True
                self._tokens.append(token)
                continue

            if token.type == TokenType.OPERATOR:
                if token.value == '}':
                    in_struct_literal = False
                self._tokens.append(token)
                continue

            if token.type != TokenType.IDENTIFIER:
                self._tokens.append(token)
                continue

            # Handle struct literal field names
            if in_struct_literal:
                next_token = self._lexer.peek_next_token([TokenType.WHITESPACE])
                if next_token and next_token.type == TokenType.OPERATOR and next_token.value == ':':
                    self._tokens.append(Token(
                        type=TokenType.ELEMENT,
                        value=token.value,
                        start=token.start
                    ))
                    continue

            # Look at the next token to determine context
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

                # Check for method calls or element access
                if next_token.value == '.':
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

        parser_state = GoParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.parsing_continuation = lexer_state.in_block_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_struct_literal = in_struct_literal
        return parser_state
