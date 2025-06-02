"""
Solidity parser

This module implements a parser for Solidity smart contract code, extending the functionality of the base parser.
"""

from dataclasses import dataclass

from humbug.syntax.lexer import TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.solidity.solidity_lexer import SolidityLexer


@dataclass
class SolidityParserState(ParserState):
    """
    State information for the Solidity parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element
        in_import: Indicates if we're currently parsing an import statement
    """
    in_element: bool = False
    in_import: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.SOLIDITY)
class SolidityParser(Parser):
    """
    Parser for Solidity smart contract code.

    This parser processes tokens from the Solidity lexer and handles special cases
    like function calls, element access, and import statements.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> SolidityParserState:
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
        in_import = False
        prev_lexer_state = None

        if prev_parser_state:
            assert isinstance(prev_parser_state, SolidityParserState), \
                f"Expected SolidityParserState, got {type(prev_parser_state).__name__}"
            in_element = prev_parser_state.in_element
            in_import = prev_parser_state.in_import
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = SolidityLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.KEYWORD and token.value == 'import':
                in_import = True

            if token.type != TokenType.IDENTIFIER:
                if (token.type == TokenType.OPERATOR and
                        token.value != '.'):
                    in_element = False

                self._tokens.append(token)
                continue

            # Look at the next token to determine if this is a function call or element access
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
                if next_token.value == '.':
                    next_in_element = True

            in_element = next_in_element

            if cur_in_element:
                token.type = TokenType.ELEMENT
                self._tokens.append(token)
                continue

            # If we're in an import statement, treat certain identifiers specially
            if in_import and not in_element:
                # For import statements, you might want to handle differently
                self._tokens.append(token)
                continue

            self._tokens.append(token)

        # Build and return the parser state
        parser_state = SolidityParserState()
        parser_state.continuation_state = 1 if (lexer_state.in_block_comment or
                                                lexer_state.in_natspec_comment) else 0
        parser_state.parsing_continuation = (lexer_state.in_block_comment or lexer_state.in_natspec_comment)
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_import = in_import
        return parser_state
