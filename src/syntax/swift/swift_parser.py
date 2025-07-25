from dataclasses import dataclass

from syntax.lexer import TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.swift.swift_lexer import SwiftLexer


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

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> SwiftParserState:
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
        """
        in_element = False
        in_closure = False
        closure_brace_count = 0
        in_generic = False
        generic_angle_count = 0
        prev_lexer_state = None

        if prev_parser_state:
            assert isinstance(prev_parser_state, SwiftParserState), \
                f"Expected SwiftParserState, got {type(prev_parser_state).__name__}"
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

            if token.type == TokenType.IDENTIFIER:
                # Look ahead for function calls, property access, or generic parameters
                next_token = lexer.peek_next_token()
                if next_token and next_token.type == TokenType.OPERATOR:
                    if next_token.value == '(':
                        # Function or method call
                        token.type = TokenType.FUNCTION_OR_METHOD
                        self._tokens.append(token)
                        continue

                    if next_token.value == '.':
                        # Property access
                        token.type = TokenType.ELEMENT
                        self._tokens.append(token)
                        continue

                    if next_token.value == '<' and not in_generic:
                        # Start of generic parameters
                        in_generic = True
                        generic_angle_count = 1
                        self._tokens.append(token)
                        continue

            elif token.type == TokenType.OPERATOR:
                if token.value == '{':
                    # Check for closure context
                    next_token = lexer.peek_next_token()
                    if next_token and (
                        next_token.type == TokenType.OPERATOR and next_token.value == '(' or
                        next_token.type == TokenType.IDENTIFIER
                    ):
                        in_closure = True
                        closure_brace_count += 1
                elif token.value == '}' and in_closure:
                    closure_brace_count -= 1
                    if closure_brace_count == 0:
                        in_closure = False
                elif token.value == '<' and not in_closure:
                    # Could be start of generic parameters or less-than operator
                    next_token = lexer.peek_next_token()
                    if next_token and (
                        next_token.type == TokenType.IDENTIFIER or
                        (next_token.type == TokenType.OPERATOR and next_token.value in ('@', '_'))
                    ):
                        in_generic = True
                        generic_angle_count += 1
                elif token.value == '>' and in_generic:
                    generic_angle_count -= 1
                    if generic_angle_count == 0:
                        in_generic = False

            self._tokens.append(token)

        parser_state = SwiftParserState()
        parser_state.continuation_state = (
            1 if lexer_state.in_block_comment or lexer_state.in_multiline_string else 0
        )
        parser_state.parsing_continuation = (
            lexer_state.in_block_comment or lexer_state.in_multiline_string
        )
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_closure = in_closure
        parser_state.closure_brace_count = closure_brace_count
        parser_state.in_generic = in_generic
        parser_state.generic_angle_count = generic_angle_count
        return parser_state
