from dataclasses import dataclass
from typing import Optional, List

from humbug.syntax.csharp.csharp_lexer import CSharpLexer
from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage


@dataclass
class CSharpParserState(ParserState):
    """
    State information for the C# parser.

    Attributes:
        in_element: Indicates if we're currently parsing an element access
        in_generic: Indicates if we're currently parsing generic type parameters
        generic_depth: Tracks nested depth of generic type parameters
        in_linq: Indicates if we're currently parsing a LINQ expression
        in_using: Indicates if we're currently parsing a using directive
    """
    in_element: bool = False
    in_generic: bool = False
    generic_depth: int = 0
    in_linq: bool = False
    in_using: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.CSHARP)
class CSharpParser(Parser):
    """
    Parser for C# code.

    This parser processes tokens from the C# lexer and generates specialized token types:
    - GENERIC_TYPE: Identifiers used as generic type parameters
    - TYPE_PARAMETER: Type parameters in generic declarations
    - FUNCTION_OR_METHOD: Method calls
    - ELEMENT: Property or field access
    - LINQ_KEYWORD: Keywords used in LINQ expressions
    """

    def parse(self, prev_parser_state: Optional[CSharpParserState], input_str: str) -> CSharpParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Raises:
            None
        """
        in_element = False
        in_generic = False
        generic_depth = 0
        in_linq = False
        in_attribute = False
        in_using = False
        prev_lexer_state = None

        if prev_parser_state:
            in_element = prev_parser_state.in_element
            in_generic = prev_parser_state.in_generic
            generic_depth = prev_parser_state.generic_depth
            in_linq = prev_parser_state.in_linq
            in_using = prev_parser_state.in_using
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = CSharpLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            # Check for using directive
            if token.type == TokenType.KEYWORD and token.value == 'using':
                in_using = True
                self._tokens.append(token)
                continue

            # Check for LINQ query expression keywords
            if token.type == TokenType.KEYWORD and token.value in self._get_linq_keywords():
                if token.value == 'from':
                    # Start of a LINQ expression
                    in_linq = True

                if in_linq:
                    self._tokens.append(Token(
                        type=TokenType.LINQ_KEYWORD,
                        value=token.value,
                        start=token.start
                    ))
                    continue

            # Handle attributes
            if token.type == TokenType.ATTRIBUTE:
                in_attribute = True
                self._tokens.append(token)
                continue

            # Handle operators which may indicate context changes
            if token.type == TokenType.OPERATOR:
                operator_value = token.value

                # Handle potential generic type parameters
                if operator_value == '<':
                    prev_token = self._get_last_non_whitespace_token()

                    # Check if this is a generic type or a less-than operator
                    is_generic = self._is_generic_start(prev_token, lexer)

                    if is_generic:
                        in_generic = True
                        generic_depth += 1
                        self._tokens.append(Token(
                            type=TokenType.GENERIC_START,
                            value=operator_value,
                            start=token.start
                        ))
                    else:
                        # This is a less-than operator
                        self._tokens.append(token)
                    continue

                elif operator_value == '>':
                    # Could be the end of a generic parameter list or a greater-than operator
                    if in_generic and generic_depth > 0:
                        generic_depth -= 1
                        if generic_depth == 0:
                            in_generic = False
                        self._tokens.append(Token(
                            type=TokenType.GENERIC_END,
                            value=operator_value,
                            start=token.start
                        ))
                    else:
                        # This is a greater-than operator
                        self._tokens.append(token)
                    continue

                elif operator_value == '.':
                    # Property or field access (but not in using directives)
                    if not in_using:
                        in_element = True
                    self._tokens.append(token)
                    continue

                elif operator_value == '?.':
                    # Null-conditional operator for property/field access
                    if not in_using:
                        in_element = True
                    self._tokens.append(token)
                    continue

                # Reset element access flag for most operators
                if operator_value not in ('.', '?.'):
                    in_element = False

                # Check for semicolon which ends using directive
                if operator_value == ';':
                    in_using = False

                self._tokens.append(token)
                continue

            # Handle identifiers based on context
            if token.type == TokenType.IDENTIFIER:
                # Skip if we're in an attribute declaration
                if in_attribute:
                    self._tokens.append(token)
                    continue

                # Check context to determine what kind of identifier this is
                if in_generic and generic_depth > 0:
                    next_token = lexer.peek_next_token()
                    if next_token and next_token.type == TokenType.OPERATOR and next_token.value in ('where', 'extends'):
                        # Type parameter with constraints
                        self._tokens.append(Token(
                            type=TokenType.TYPE_PARAMETER,
                            value=token.value,
                            start=token.start
                        ))
                    else:
                        # Generic type parameter
                        self._tokens.append(Token(
                            type=TokenType.GENERIC_TYPE,
                            value=token.value,
                            start=token.start
                        ))
                    continue

                # Check if this is a method call
                next_token = lexer.peek_next_token()
                if next_token and next_token.type == TokenType.OPERATOR and next_token.value == '(':
                    self._tokens.append(Token(
                        type=TokenType.FUNCTION_OR_METHOD,
                        value=token.value,
                        start=token.start
                    ))
                    continue

                # Check if this is a generic method call
                if next_token and next_token.type == TokenType.OPERATOR and next_token.value == '<':
                    # Look ahead to determine if this is a generic method
                    is_generic_method = self._is_generic_method(token, lexer)
                    if is_generic_method:
                        self._tokens.append(Token(
                            type=TokenType.GENERIC_METHOD,
                            value=token.value,
                            start=token.start
                        ))
                        continue

                # Check if this is a property or field access, but not in a using directive
                if in_element and not in_using:
                    self._tokens.append(Token(
                        type=TokenType.ELEMENT,
                        value=token.value,
                        start=token.start
                    ))
                    continue

                # Regular identifier
                self._tokens.append(token)
                continue

            # For all other token types, just add them as-is
            self._tokens.append(token)

        # Create and return the parser state
        parser_state = CSharpParserState()
        parser_state.continuation_state = 1 if (
            lexer_state.in_block_comment or
            lexer_state.in_xml_doc or
            lexer_state.in_verbatim_string
        ) else 0
        parser_state.parsing_continuation = (
            lexer_state.in_block_comment or
            lexer_state.in_xml_doc or
            lexer_state.in_verbatim_string
        )
        parser_state.lexer_state = lexer_state
        parser_state.in_element = in_element
        parser_state.in_generic = in_generic
        parser_state.generic_depth = generic_depth
        parser_state.in_linq = in_linq
        parser_state.in_using = in_using
        return parser_state

    def _get_last_non_whitespace_token(self) -> Optional[Token]:
        """
        Get the last token that isn't whitespace.

        Returns:
            The last non-whitespace token, or None if no such token exists
        """
        for token in reversed(self._tokens):
            return token

        return None

    def _is_generic_start(self, prev_token: Optional[Token], lexer: CSharpLexer) -> bool:
        """
        Determine if a '<' operator is the start of generic type parameters.

        Args:
            prev_token: The token before the '<' operator
            lexer: The lexer instance for lookahead

        Returns:
            True if this is a generic type parameter list, False otherwise
        """
        if not prev_token:
            return False

        # '<' can start generics if preceded by an identifier and followed by
        # another identifier, keyword, or '?'
        if prev_token.type in (TokenType.IDENTIFIER, TokenType.FUNCTION_OR_METHOD):
            next_token = lexer.peek_next_token()
            if next_token:
                if next_token.type in (TokenType.IDENTIFIER, TokenType.KEYWORD):
                    return True

                if next_token.type == TokenType.OPERATOR and next_token.value in ('?', 'in', 'out'):
                    return True

        return False

    def _is_generic_method(self, token: Token, lexer: CSharpLexer) -> bool:
        """
        Determine if an identifier followed by '<' is a generic method call.

        Args:
            token: The identifier token
            lexer: The lexer instance for lookahead

        Returns:
            True if this is a generic method call, False otherwise
        """
        # Skip over the whitespace and '<'
        token_pos = 1
        current_token = lexer.peek_next_token(offset=token_pos)
        if not current_token or current_token.type != TokenType.OPERATOR or current_token.value != '<':
            return False

        token_pos += 1

        # Look for the matching '>'
        generic_depth = 1
        while True:
            current_token = lexer.peek_next_token(offset=token_pos)
            if not current_token:
                return False

            token_pos += 1

            if current_token.type == TokenType.OPERATOR:
                if current_token.value == '<':
                    generic_depth += 1
                elif current_token.value == '>':
                    generic_depth -= 1
                    if generic_depth == 0:
                        break

        # After the '>', we should see a '(' for a method call
        current_token = lexer.peek_next_token(offset=token_pos)
        return current_token and current_token.type == TokenType.OPERATOR and current_token.value == '('

    def _get_linq_keywords(self) -> List[str]:
        """
        Get the list of keywords commonly used in LINQ expressions.

        Returns:
            List of LINQ keywords
        """
        return [
            'from', 'where', 'select', 'group', 'into', 'orderby',
            'join', 'let', 'in', 'on', 'equals', 'by', 'ascending',
            'descending'
        ]
