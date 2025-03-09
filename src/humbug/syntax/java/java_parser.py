from dataclasses import dataclass
from typing import Optional

from humbug.syntax.java.java_lexer import JavaLexer
from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class JavaParserState(ParserState):
    """
    State information for the Java parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element access
        in_generic: Indicates if we're currently parsing generic type parameters
        generic_depth: Tracks nested depth of generic type parameters
    """
    in_element: bool = False
    in_generic: bool = False
    generic_depth: int = 0


@ParserRegistry.register_parser(ProgrammingLanguage.JAVA)
class JavaParser(Parser):
    """
    Parser for Java code.

    This parser processes tokens from the Java lexer and generates specialized token types:
    - GENERIC_TYPE: Identifiers used as generic type parameters
    - METHOD_REFERENCE: Method names in method references
    - TYPE_PARAMETER: Type parameters in generic declarations
    """

    def parse(self, prev_parser_state: Optional[JavaParserState], input_str: str) -> JavaParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        The parser uses state tracking to emit specialized tokens:
        - generic_depth > 0: Identifiers become GENERIC_TYPE or TYPE_PARAMETER tokens
        - After '::': Identifiers become METHOD_REFERENCE tokens
        """
        in_element = False
        in_generic = False
        generic_depth = 0
        in_import = False
        prev_lexer_state = None

        if prev_parser_state:
            in_element = prev_parser_state.in_element
            in_generic = prev_parser_state.in_generic
            generic_depth = prev_parser_state.generic_depth
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = JavaLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            if token.type == TokenType.KEYWORD and token.value in ('import', 'package'):
                in_import = True

            if token.type == TokenType.OPERATOR:
                token_value = token.value

                # Handle potential generic type parameters or less-than operator
                if token_value == '<':
                    # Look back at previous token and forward to help determine context
                    prev_token = self._get_last_token()
                    next_token = lexer.peek_next_token()

                    is_generic = False
                    if prev_token and next_token:
                        # Generics can follow:
                        # 1. Class/interface names (identifiers)
                        # 2. Method names in generic method declarations
                        # 3. After a comma in a generic parameter list
                        # 4. After another generic parameter list (nested generics)
                        if prev_token.type in (TokenType.IDENTIFIER, TokenType.FUNCTION_OR_METHOD) and (
                            next_token.type in (TokenType.IDENTIFIER, TokenType.KEYWORD) or
                            (next_token.type == TokenType.OPERATOR and next_token.value == '?')
                        ):
                            is_generic = True
                        elif (prev_token.type == TokenType.OPERATOR and
                              prev_token.value == ',' and
                              next_token.type in (TokenType.IDENTIFIER, TokenType.OPERATOR)):
                            is_generic = True
                        elif (prev_token.type == TokenType.OPERATOR and
                              prev_token.value == '>' and
                              next_token.type == TokenType.IDENTIFIER):
                            is_generic = True

                    if is_generic:
                        in_generic = True
                        generic_depth += 1
                        token.type = TokenType.GENERIC_START

                    self._tokens.append(token)
                    continue

                if token_value == '>':
                    if in_generic:
                        generic_depth -= 1
                        if generic_depth == 0:
                            in_generic = False

                        # Emit a specialized token for the generic operator
                        token.type = TokenType.GENERIC_END

                    self._tokens.append(token)
                    continue

                # Handle method references
                if token_value == '::':
                    # Change the token type for the operator itself
                    token.type = TokenType.METHOD_REFERENCE_OPERATOR
                    self._tokens.append(token)
                    continue

                # Handle property access chains
                if token_value == '.':
                    in_element = True
                    self._tokens.append(token)
                    continue

                in_element = False
                self._tokens.append(token)
                continue

            if token.type != TokenType.IDENTIFIER or in_import:
                self._tokens.append(token)
                continue

            # Handle identifier tokens based on context
            if in_generic and generic_depth > 0:
                # Inside generic parameters, create specialized tokens
                next_token = lexer.peek_next_token()
                if next_token and next_token.type == TokenType.KEYWORD and next_token.value == 'extends':
                    # This is a bounded type parameter
                    token.type = TokenType.TYPE_PARAMETER
                    self._tokens.append(token)
                else:
                    # This is a generic type
                    token.type = TokenType.GENERIC_TYPE
                    self._tokens.append(token)

                continue

            self._handle_identifier(token, lexer, in_element)

        parser_state = JavaParserState()
        parser_state.continuation_state = (
            1 if lexer_state.in_block_comment or lexer_state.in_javadoc else 0
        )
        parser_state.parsing_continuation = (
            lexer_state.in_block_comment or lexer_state.in_javadoc
        )
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_generic = in_generic
        parser_state.generic_depth = generic_depth
        return parser_state

    def _handle_identifier(self, token: Token, lexer: 'JavaLexer', in_element: bool) -> None:
        """
        Handle identifier tokens based on context.

        Args:
            token: The identifier token to process
            lexer: The lexer instance for lookahead
            in_element: Whether we're in a property access chain
        """
        next_token = lexer.peek_next_token()

        if not next_token:
            self._tokens.append(token)
            return

        if next_token.type == TokenType.OPERATOR:
            # Method call
            if next_token.value == '(':
                token.type = TokenType.FUNCTION_OR_METHOD
                self._tokens.append(token)
                return

            # Generic type parameters
            if next_token.value == '<':
                # This might be a generic type instantiation
                after_generic = self._peek_after_generic_close(lexer)
                if (after_generic and after_generic.type == TokenType.OPERATOR and
                        after_generic.value == '('):
                    # Generic method call
                    token.type = TokenType.GENERIC_METHOD
                    self._tokens.append(token)
                    return

        # Property or element access
        if in_element:
            token.type = TokenType.ELEMENT
            self._tokens.append(token)
            return

        # Regular identifier
        self._tokens.append(token)

    def _get_last_token(self) -> Optional[Token]:
        """
        Get the last token.
        
        Returns:
            The last token, or None if no such token exists
        """
        if self._tokens:
            return self._tokens[-1]

        return None

    def _peek_after_generic_close(self, lexer: 'JavaLexer') -> Optional[Token]:
        """
        Look ahead to find the token after a generic type parameter list closes.
        
        Args:
            lexer: The lexer instance for lookahead

        Returns:
            The token after the generic parameter list closes, or None if not found
        """
        peek_depth = 1
        token = None
        token_count = 0  # Count how many tokens we look ahead

        # First look ahead to find the matching '>'
        while True:
            peeked_token = lexer.peek_next_token(offset=token_count)
            if not peeked_token:
                break

            token_count += 1
            if peeked_token.type == TokenType.OPERATOR:
                if peeked_token.value == '<':
                    peek_depth += 1
                elif peeked_token.value == '>':
                    peek_depth -= 1
                    if peek_depth == 0:
                        # Found the closing '>', peek at next token
                        token = lexer.peek_next_token(offset=token_count)
                        break

        return token
