from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.scheme_lexer import SchemeLexer
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.parser_registry import ParserRegistry


@dataclass
class SchemeParserState(ParserState):
    """
    State information for the Scheme parser.

    Attributes:
        paren_depth: Current depth of nested parentheses
        in_vector: Whether we're currently parsing a vector
    """
    paren_depth: int = 0
    in_vector: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.SCHEME)
class SchemeParser(Parser):
    """
    Parser for R5RS Scheme code.

    This parser processes tokens from the Scheme lexer and handles special cases
    like nested expressions and vectors.
    """

    def parse(self, prev_parser_state: Optional[SchemeParserState], input_str: str) -> SchemeParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser tracks nested parentheses depth and vector contexts to maintain
            proper state across multiple parse calls.
        """
        paren_depth = 0
        in_vector = False
        prev_lexer_state = None

        if prev_parser_state:
            paren_depth = prev_parser_state.paren_depth
            in_vector = prev_parser_state.in_vector
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = SchemeLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token(['WHITESPACE'])
            if not token:
                break

            # Handle vector start
            if token.type == 'VECTOR_START':
                in_vector = True
                paren_depth += 1
                self._tokens.append(token)
                continue

            # Handle opening parentheses
            if token.type == 'LPAREN':
                paren_depth += 1
                self._tokens.append(token)
                continue

            # Handle closing parentheses
            if token.type == 'RPAREN':
                if paren_depth > 0:
                    paren_depth -= 1
                    if paren_depth == 0:
                        in_vector = False
                self._tokens.append(token)
                continue

            # Special handling for identifiers in first position after opening parenthesis
            if (token.type == 'IDENTIFIER' and
                    self._tokens and
                    self._tokens[-1].type in ('LPAREN', 'VECTOR_START')):
                # Check if it's a special form
                if self._is_special_form(token.value):
                    self._tokens.append(Token(
                        type='SPECIAL_FORM',
                        value=token.value,
                        start=token.start
                    ))
                    continue

                # Check if it's a standard procedure
                if self._is_standard_procedure(token.value):
                    self._tokens.append(Token(
                        type='PROCEDURE',
                        value=token.value,
                        start=token.start
                    ))
                    continue

            self._tokens.append(token)

        parser_state = SchemeParserState()
        parser_state.continuation_state = paren_depth
        parser_state.lexer_state = lexer_state
        parser_state.paren_depth = paren_depth
        parser_state.in_vector = in_vector
        return parser_state

    def _is_special_form(self, value: str) -> bool:
        """
        Check if a given value is a Scheme special form.

        Args:
            value: The string to check

        Returns:
            True if the value is a special form, False otherwise
        """
        special_forms = {
            'define', 'set!', 'let', 'let*', 'letrec', 
            'begin', 'if', 'cond', 'case', 'and', 'or',
            'lambda', 'delay', 'quasiquote', 'unquote',
            'unquote-splicing'
        }
        return value.lower() in special_forms

    def _is_standard_procedure(self, value: str) -> bool:
        """
        Check if a given value is a Scheme standard procedure.

        Args:
            value: The string to check

        Returns:
            True if the value is a standard procedure, False otherwise
        """
        procedures = {
            # Type predicates
            'boolean?', 'pair?', 'symbol?', 'number?', 'char?', 'string?',
            'vector?', 'procedure?', 'null?',
            # Basic arithmetic
            '+', '-', '*', '/', 'quotient', 'remainder', 'modulo',
            # List operations
            'car', 'cdr', 'cons', 'list', 'append', 'reverse', 'list-ref',
            # String operations
            'string-append', 'string-length', 'substring',
            # Vector operations
            'vector', 'vector-ref', 'vector-set!', 'make-vector',
            # I/O procedures
            'display', 'newline', 'read', 'write', 'load',
            # Other essential procedures
            'eq?', 'eqv?', 'equal?', 'apply', 'map', 'force', 'eval'
        }
        return value.lower() in procedures