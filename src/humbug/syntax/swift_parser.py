from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.swift_lexer import SwiftLexer


@dataclass
class SwiftParserState(ParserState):
    """
    State information for the Swift parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element access
        in_closure: Indicates if we're currently parsing a closure
        closure_brace_count: Number of nested braces in current closure
        in_generic: Indicates if we're currently parsing generic parameters
        generic_angle_count: Number of nested angle brackets in current generic
    """
    in_element: bool = False
    in_closure: bool = False
    closure_brace_count: int = 0
    in_generic: bool = False
    generic_angle_count: int = 0


@ParserRegistry.register_parser(ProgrammingLanguage.SWIFT)
class SwiftParser(Parser):
    """
    Parser for Swift code.

    This parser processes tokens from the Swift lexer and handles special cases
    like function calls, property access, closure expressions, and generic types.
    """

    def parse(self, prev_parser_state: Optional[SwiftParserState], input_str: str) -> SwiftParserState:
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
            - Converting identifiers to ELEMENT tokens in property access chains
            - Tracking closure expressions and their brace depth
            - Tracking generic parameter lists and their angle bracket depth
            - String interpolation expressions
        """
        in_element = False
        in_closure = False
        closure_brace_count = 0
        in_generic = False
        generic_angle_count = 0
        prev_lexer_state = None

        if prev_parser_state:
            in_element = prev_parser_state.in_element
            in_closure = prev_parser_state.in_closure
            closure_brace_count = prev_parser_state.closure_brace_count
            in_generic = prev_parser_state.in_generic
            generic_angle_count = prev_parser_state.generic_angle_count
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = SwiftLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == 'IDENTIFIER':
                # Look ahead for function calls, property access, or generic parameters
                next_token = lexer.peek_next_token(['WHITESPACE'])
                if next_token and next_token.type == 'OPERATOR':
                    if next_token.value == '(':
                        # Function or method call
                        self._tokens.append(Token(
                            type='FUNCTION_OR_METHOD',
                            value=token.value,
                            start=token.start
                        ))
                        continue
                    elif next_token.value == '.':
                        # Property access
                        self._tokens.append(Token(
                            type='ELEMENT',
                            value=token.value,
                            start=token.start
                        ))
                        continue
                    elif next_token.value == '<' and not in_generic:
                        # Start of generic parameters
                        in_generic = True
                        generic_angle_count = 1
                        self._tokens.append(token)
                        continue

            elif token.type == 'OPERATOR':
                if token.value == '{':
                    # Check for closure context
                    next_token = lexer.peek_next_token(['WHITESPACE'])
                    if next_token and (
                        next_token.type == 'OPERATOR' and next_token.value == '(' or
                        next_token.type == 'IDENTIFIER'
                    ):
                        in_closure = True
                        closure_brace_count += 1
                elif token.value == '}' and in_closure:
                    closure_brace_count -= 1
                    if closure_brace_count == 0:
                        in_closure = False
                elif token.value == '<' and not in_closure:
                    # Could be start of generic parameters or less-than operator
                    next_token = lexer.peek_next_token(['WHITESPACE'])
                    if next_token and (
                        next_token.type == 'IDENTIFIER' or
                        (next_token.type == 'OPERATOR' and next_token.value in ('@', '_'))
                    ):
                        in_generic = True
                        generic_angle_count += 1
                elif token.value == '>' and in_generic:
                    generic_angle_count -= 1
                    if generic_angle_count == 0:
                        in_generic = False

            # Handle string interpolation
            elif token.type == 'INTERPOLATION_START':
                self._handle_interpolation(token, lexer)
                continue

            self._tokens.append(token)

        parser_state = SwiftParserState()
        parser_state.continuation_state = (
            1 if lexer_state.in_block_comment or lexer_state.in_string_interpolation else 0
        )
        parser_state.parsing_continuation = (
            lexer_state.in_block_comment or lexer_state.in_string_interpolation
        )
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_closure = in_closure
        parser_state.closure_brace_count = closure_brace_count
        parser_state.in_generic = in_generic
        parser_state.generic_angle_count = generic_angle_count
        return parser_state

    def _handle_interpolation(self, token: Token, lexer: SwiftLexer) -> None:
        """
        Handle string interpolation expressions.

        Args:
            token: The interpolation start token
            lexer: The lexer instance for getting more tokens
        """
        self._tokens.append(token)
        brace_count = 1  # We're already inside one expression

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == 'OPERATOR':
                if token.value == '{':
                    brace_count += 1
                elif token.value == '}':
                    brace_count -= 1
                    if brace_count == 0:
                        break

            self._tokens.append(token)
