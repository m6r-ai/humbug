from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.rust.rust_lexer import RustLexer


@dataclass
class RustParserState(ParserState):
    """
    State information for the Rust parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element
        in_generic_params: Indicates if we're inside generic type parameters
        generic_depth: Tracks nested generic parameter depth
    """
    in_element: bool = False
    in_generic_params: bool = False
    generic_depth: int = 0


@ParserRegistry.register_parser(ProgrammingLanguage.RUST)
class RustParser(Parser):
    """
    Parser for Rust code.

    This parser processes tokens from the Rust lexer and handles special cases like:
    - Function calls
    - Generic type parameters
    - Element access
    - Raw identifiers
    """

    def __init__(self):
        super().__init__()
        self._lexer = RustLexer()

    def parse(self, prev_parser_state: Optional[RustParserState], input_str: str) -> RustParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        self._tokens = []
        self._next_token = 0

        in_element = False
        in_generic_params = False
        generic_depth = 0
        prev_lexer_state = None

        if prev_parser_state:
            in_element = prev_parser_state.in_element
            in_generic_params = prev_parser_state.in_generic_params
            generic_depth = prev_parser_state.generic_depth
            prev_lexer_state = prev_parser_state.lexer_state

        lexer_state = self._lexer.lex(prev_lexer_state, input_str)

        while True:
            token = self._lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.OPERATOR:
                if token.value == '<':
                    # Check if this is the start of generic parameters
                    next_token = self._lexer.peek_next_token([TokenType.WHITESPACE])
                    if (next_token and
                            (next_token.type in (TokenType.IDENTIFIER, TokenType.KEYWORD) or
                             next_token.value in ('\'', '>'))):
                        in_generic_params = True
                        generic_depth += 1
                        self._tokens.append(token)
                        continue
                elif token.value == '>':
                    if in_generic_params:
                        generic_depth -= 1
                        if generic_depth == 0:
                            in_generic_params = False
                        self._tokens.append(token)
                        continue
                elif token.value == '::':
                    # Module path separator
                    in_element = True
                    self._tokens.append(token)
                    continue
                elif token.value == '.':
                    # Method or field access
                    in_element = True
                    self._tokens.append(token)
                    continue

            if token.type == TokenType.IDENTIFIER:
                self._handle_identifier(token, self._lexer, in_element)
                continue

            # Reset element context for other token types
            in_element = False
            self._tokens.append(token)

        parser_state = RustParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.parsing_continuation = lexer_state.in_block_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_generic_params = in_generic_params
        parser_state.generic_depth = generic_depth
        return parser_state

    def _is_type_parameter_start(self, token: Token, next_token: Optional[Token]) -> bool:
        """
        Determine if tokens indicate the start of a type parameter list.

        Args:
            token: Current token
            next_token: Next token in the stream

        Returns:
            True if this is the start of type parameters, False otherwise
        """
        if not next_token:
            return False

        # Must start with an identifier followed by <
        if token.type != TokenType.IDENTIFIER:
            return False
        if next_token.type != TokenType.OPERATOR or next_token.value != '<':
            return False

        return True

    def _handle_identifier(self, token: Token, lexer: RustLexer, in_element: bool) -> None:
        """
        Process identifier tokens based on context.

        This determines whether an identifier should be treated as:
        - A type name with generic parameters
        - A function/method call
        - A path/module element
        - A regular identifier

        Args:
            token: The identifier token to process
            lexer: The lexer instance for lookahead
            in_element: Whether we're in a path/element chain
        """
        # Look ahead for type parameters or function calls
        next_token = lexer.peek_next_token([TokenType.WHITESPACE])
        if next_token and next_token.type == TokenType.OPERATOR:
            if next_token.value == '<':
                # Possible generic type
                peek_ahead = lexer.peek_next_token([TokenType.WHITESPACE], offset=1)
                if peek_ahead and (peek_ahead.type in (TokenType.IDENTIFIER, TokenType.LIFETIME, TokenType.KEYWORD)):
                    self._tokens.append(Token(
                        type=TokenType.TYPE,
                        value=token.value,
                        start=token.start
                    ))
                    return

            if next_token.value == '(':
                # Function or method call
                self._tokens.append(Token(
                    type=TokenType.FUNCTION_OR_METHOD,
                    value=token.value,
                    start=token.start
                ))
                return

            if next_token.value in ('::', '.'):
                # Path or field access
                self._tokens.append(Token(
                    type=TokenType.ELEMENT,
                    value=token.value,
                    start=token.start
                ))
                return

        # Regular identifier
        self._tokens.append(token)
