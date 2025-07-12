from dataclasses import dataclass

from humbug.lib.syntax.javascript.javascript_parser import JavaScriptParser, JavaScriptParserState
from humbug.lib.syntax.lexer import TokenType
from humbug.lib.syntax.parser import ParserState
from humbug.lib.syntax.parser_registry import ParserRegistry
from humbug.lib.syntax.programming_language import ProgrammingLanguage
from humbug.lib.syntax.typescript.typescript_lexer import TypeScriptLexer


@dataclass
class TypeScriptParserState(JavaScriptParserState):
    """
    State information for the Cpp parser.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.TYPESCRIPT)
class TypeScriptParser(JavaScriptParser):
    """
    Parser for TypeScript code.

    This parser extends the JavaScript parser to handle TypeScript-specific syntax
    and constructs.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> TypeScriptParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            Uses the TypeScript lexer for token generation while maintaining the
            JavaScript parsing logic.
        """
        in_element = False
        prev_lexer_state = None
        if prev_parser_state:
            assert isinstance(prev_parser_state, TypeScriptParserState), \
                f"Expected TypeScriptParserState, got {type(prev_parser_state).__name__}"
            in_element = prev_parser_state.in_element
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = TypeScriptLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
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
                if next_token.value in ('.', '?.'):
                    next_in_element = True

            in_element = next_in_element

            if cur_in_element:
                token.type = TokenType.ELEMENT
                self._tokens.append(token)
                continue

            self._tokens.append(token)

        parser_state = TypeScriptParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.parsing_continuation = lexer_state.in_block_comment
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        return parser_state
