from dataclasses import dataclass

from syntax.lexer import TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.python.python_lexer import PythonLexer


@dataclass
class PythonParserState(ParserState):
    """
    State information for the Python parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element
    """
    in_element: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.PYTHON)
class PythonParser(Parser):
    """
    Parser for Python code.

    This parser processes tokens from the Python lexer and handles special cases
    like function calls and element access.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> PythonParserState:
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
            assert isinstance(prev_parser_state, PythonParserState), \
                f"Expected PythonParserState, got {type(prev_parser_state).__name__}"
            in_element = prev_parser_state.in_element
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = PythonLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.KEYWORD and token.value in ('from', 'import'):
                in_import = True

            if token.type != TokenType.IDENTIFIER and not in_import:
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
                if next_token.value == '.':
                    next_in_element = True

            in_element = next_in_element

            if cur_in_element:
                token.type = TokenType.ELEMENT
                self._tokens.append(token)
                continue

            self._tokens.append(token)

        parser_state = PythonParserState()
        parser_state.continuation_state = 1 if lexer_state.in_docstring else 0
        parser_state.parsing_continuation = lexer_state.in_docstring
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        return parser_state
