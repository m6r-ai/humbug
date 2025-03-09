from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.move.move_lexer import MoveLexer
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class MoveParserState(ParserState):
    """
    State information for the Move parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element access
        in_module_access: Indicates if we're currently parsing a module access path
    """
    in_element: bool = False
    in_module_access: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.MOVE)
class MoveParser(Parser):
    """
    Parser for Move code.

    This parser processes tokens from the Move lexer and handles special cases
    like function calls, module access paths, and element access. It handles
    Move-specific syntax such as:
    - Module access paths (e.g., 0x1::string::utf8)
    - Function calls with type parameters
    - Struct instantiation
    - Vector operations
    """

    def parse(self, prev_parser_state: Optional[MoveParserState], input_str: str) -> MoveParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser converts identifier tokens to:
            - FUNCTION_OR_METHOD tokens when followed by parentheses
            - ELEMENT tokens when part of a dotted or :: access chain
            - TYPE tokens when used in type contexts (after ':' or '<')
        """
        in_element = False
        in_module_access = False
        prev_lexer_state = None
        if prev_parser_state:
            in_element = prev_parser_state.in_element
            in_module_access = prev_parser_state.in_module_access
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = MoveLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.ADDRESS:
                next_token = lexer.peek_next_token()
                if next_token and next_token.type == TokenType.OPERATOR and next_token.value == '::':
                    in_module_access = True
                self._tokens.append(token)
                continue

            if token.type != TokenType.IDENTIFIER:
                if token.type == TokenType.OPERATOR:
                    if token.value == '::':
                        in_module_access = True
                        self._tokens.append(token)
                        continue

                    if token.value == ':':
                        # Next identifier might be a type
                        next_token = lexer.peek_next_token()
                        if next_token and next_token.type == TokenType.IDENTIFIER:
                            self._tokens.append(token)
                            token = lexer.get_next_token()
                            self._tokens.append(Token(
                                type=TokenType.TYPE,
                                value=token.value,
                                start=token.start
                            ))
                            continue

                    if token.value == '<':
                        # Might be start of type parameters or vector literal
                        next_token = lexer.peek_next_token()
                        if next_token and next_token.type == TokenType.IDENTIFIER:
                            self._tokens.append(token)
                            token = lexer.get_next_token()
                            self._tokens.append(Token(
                                type=TokenType.TYPE,
                                value=token.value,
                                start=token.start
                            ))
                            continue

                    if token.value not in ('.', '::'):
                        in_element = False
                        in_module_access = False

                self._tokens.append(token)
                continue

            # Look at the next token. If it's a '(' operator then we're making a
            # function or method call!
            cur_in_element = in_element
            cur_in_module_access = in_module_access
            next_token = lexer.peek_next_token()
            in_element = cur_in_element
            in_module_access = cur_in_module_access

            next_in_element = False
            next_in_module_access = False

            if next_token and next_token.type == TokenType.OPERATOR:
                if next_token.value == '(':
                    in_element = False
                    in_module_access = False
                    self._tokens.append(Token(
                        type=TokenType.FUNCTION_OR_METHOD,
                        value=token.value,
                        start=token.start
                    ))
                    continue

                if next_token.value == '<':
                    # Check if this is a type instantiation
                    next_next_token = lexer.peek_next_token([TokenType.IDENTIFIER, TokenType.OPERATOR])
                    if next_next_token and next_next_token.value == '>':
                        self._tokens.append(Token(
                            type=TokenType.TYPE,
                            value=token.value,
                            start=token.start
                        ))
                        continue

                # Is the next token going to be an element?
                if next_token.value == '::':
                    next_in_module_access = True
                    next_in_element = True

            in_element = next_in_element
            in_module_access = next_in_module_access

            if cur_in_element or cur_in_module_access:
                self._tokens.append(Token(
                    type=TokenType.ELEMENT,
                    value=token.value,
                    start=token.start
                ))
                continue

            self._tokens.append(token)

        parser_state = MoveParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.parsing_continuation = lexer_state.in_block_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_module_access = in_module_access
        return parser_state
