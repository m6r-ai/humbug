"""
C++ parser

This module implements a parser for C++ code, extending the functionality of the C parser.
"""
from dataclasses import dataclass

from syntax.c.c_parser import CParser, CParserState
from syntax.cpp.cpp_lexer import CppLexer
from syntax.lexer import TokenType
from syntax.parser import ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage


@dataclass
class CppParserState(CParserState):
    """
    State information for the Cpp parser.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.CPP)
class CppParser(CParser):
    """
    Parser for C++ code.

    This parser extends the C parser to handle C++-specific syntax while
    maintaining the same token processing logic for function calls and
    element access.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> CppParserState:
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
            they're part of a dotted or arrow access chain. It also handles
            C++-specific cases like the 'this' keyword.
        """
        in_element = False
        prev_lexer_state = None
        if prev_parser_state:
            assert isinstance(prev_parser_state, CppParserState), \
                f"Expected CppParserState, got {type(prev_parser_state).__name__}"
            in_element = prev_parser_state.in_element
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = CppLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type != TokenType.IDENTIFIER:
                if token.type == TokenType.OPERATOR and token.value not in ('.', '->'):
                    in_element = False
                    self._tokens.append(token)
                    continue

                if token.type != TokenType.KEYWORD or token.value != 'this':
                    self._tokens.append(token)
                    continue

            # Look at the next token. If it's a '(' operator then we're making a
            # function or method call!
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
                if next_token.value in ('.', '->'):
                    next_in_element = True

            in_element = next_in_element

            if cur_in_element:
                token.type = TokenType.ELEMENT
                self._tokens.append(token)
                continue

            self._tokens.append(token)

        parser_state = CppParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.parsing_continuation = lexer_state.in_block_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        return parser_state
