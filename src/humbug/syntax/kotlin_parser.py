from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.kotlin_lexer import KotlinLexer
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class KotlinParserState(ParserState):
    """
    State information for the Kotlin parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element access
        in_lambda: Indicates if we're currently parsing a lambda expression
        lambda_brace_count: Number of nested braces in current lambda
        template_expression_count: Count of open template expressions
    """
    in_element: bool = False
    in_lambda: bool = False
    lambda_brace_count: int = 0
    template_expression_count: int = 0


@ParserRegistry.register_parser(ProgrammingLanguage.KOTLIN)
class KotlinParser(Parser):
    """
    Parser for Kotlin code.

    This parser processes tokens from the Kotlin lexer and handles special cases
    like property/function access, string templates, and lambda expressions.
    """

    def parse(self, prev_parser_state: Optional[KotlinParserState], input_str: str) -> KotlinParserState:
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
            - Tracking lambda expressions and their brace depth
            - String template expressions
        """
        in_element = False
        in_lambda = False
        lambda_brace_count = 0
        template_expression_count = 0
        prev_lexer_state = None

        if prev_parser_state:
            in_element = prev_parser_state.in_element
            in_lambda = prev_parser_state.in_lambda
            lambda_brace_count = prev_parser_state.lambda_brace_count
            template_expression_count = prev_parser_state.template_expression_count
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = KotlinLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            # Handle special token sequences
            if token.type == 'IDENTIFIER':
                self._handle_identifier(token, lexer, in_element)
                continue

            if token.type == 'OPERATOR':
                token_value = token.value

                # Track lambda braces
                if token_value == '{':
                    if in_lambda:
                        lambda_brace_count += 1
                    # Check if this starts a lambda by looking ahead
                    else:
                        next_token = lexer.peek_next_token(['WHITESPACE'])
                        if next_token and (
                            next_token.type == 'IDENTIFIER' or
                            (next_token.type == 'OPERATOR' and next_token.value == '->')
                        ):
                            in_lambda = True
                            lambda_brace_count = 1
                elif token_value == '}':
                    if in_lambda:
                        lambda_brace_count -= 1
                        if lambda_brace_count == 0:
                            in_lambda = False
                    if template_expression_count > 0:
                        template_expression_count -= 1

                # Track property access and safe calls
                elif token_value in {'.', '?.'}:
                    in_element = True
                    self._tokens.append(token)
                    continue

                # Handle string template expressions
                elif token_value == '${':
                    template_expression_count += 1

                in_element = False
                self._tokens.append(token)
                continue

            # Other token types pass through unchanged
            in_element = False
            self._tokens.append(token)

        parser_state = KotlinParserState()
        parser_state.continuation_state = 1 if lexer_state.in_block_comment else 0
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_lambda = in_lambda
        parser_state.lambda_brace_count = lambda_brace_count
        parser_state.template_expression_count = template_expression_count
        return parser_state

    def _handle_identifier(self, token: Token, lexer: KotlinLexer, in_element: bool) -> None:
        """
        Handle identifier tokens based on context.

        This method determines whether an identifier should be treated as:
        - A function/method call
        - A property/element access
        - A regular identifier

        Args:
            token: The identifier token to process
            lexer: The lexer instance for lookahead
            in_element: Whether we're in a property access chain
        """
        # Look at the next token to determine context
        next_token = lexer.peek_next_token(['WHITESPACE'])

        if next_token and next_token.type == 'OPERATOR':
            if next_token.value == '(':
                # Function or method call
                self._tokens.append(Token(
                    type='FUNCTION_OR_METHOD',
                    value=token.value,
                    start=token.start
                ))
                return

            if next_token.value == '<':
                # Check for generic type parameters
                # Keep track of tokens we consume while looking ahead
                consumed_tokens = []
                depth = 1
                found_generic = False

                while True:
                    next_token = lexer.get_next_token(['WHITESPACE'])
                    if not next_token:
                        break

                    consumed_tokens.append(next_token)
                    if next_token.type == 'OPERATOR':
                        if next_token.value == '<':
                            depth += 1
                        elif next_token.value == '>':
                            depth -= 1
                            if depth == 0:
                                found_generic = True
                                break

                # Push tokens back in reverse order
                for token in reversed(consumed_tokens):
                    self._tokens.insert(len(self._tokens), token)
                if found_generic:
                    self._tokens.append(Token(
                        type='TYPE',
                        value=token.value,
                        start=token.start
                    ))
                    return

        if in_element:
            # Property or element access
            self._tokens.append(Token(
                type='ELEMENT',
                value=token.value,
                start=token.start
            ))
            return

        # Regular identifier
        self._tokens.append(token)
