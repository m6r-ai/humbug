"""Parser for AIFPL expressions."""

from dataclasses import dataclass
from typing import List, Union

from aifpl.aifpl_error import AIFPLParseError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


# S-Expression types
Atom = Union[int, float, complex, str, bool]
SExpression = Union[Atom, List['SExpression']]


@dataclass
class ParsedExpression:
    """Wrapper to track position info for error reporting."""
    expr: SExpression
    start_pos: int
    end_pos: int


class AIFPLParser:
    """Parses tokens into an Abstract Syntax Tree."""

    def __init__(self, tokens: List[AIFPLToken]):
        """
        Initialize parser with tokens.

        Args:
            tokens: List of tokens to parse
        """
        self.tokens = tokens
        self.pos = 0
        self.current_token: AIFPLToken | None = tokens[0] if tokens else None

    def parse(self) -> ParsedExpression:
        """
        Parse tokens into AST.

        Returns:
            Parsed expression tree

        Raises:
            AIFPLParseError: If parsing fails
        """
        if self.current_token is None or self.current_token.type == AIFPLTokenType.EOF:
            raise AIFPLParseError("Empty expression")

        expr = self._parse_expression()

        if self.current_token is None or self.current_token.type != AIFPLTokenType.EOF:
            current_value = self.current_token.value if self.current_token else "EOF"
            current_pos = self.current_token.position if self.current_token else "end"
            raise AIFPLParseError(f"Unexpected token after expression: {current_value} at position {current_pos}")

        return expr

    def _parse_expression(self) -> ParsedExpression:
        """Parse a single expression (atom or list)."""
        if self.current_token is None:
            raise AIFPLParseError("Unexpected end of input, expected expression")

        start_pos = self.current_token.position

        if self.current_token.type == AIFPLTokenType.LPAREN:
            return self._parse_list(start_pos)

        if self.current_token.type in (AIFPLTokenType.NUMBER, AIFPLTokenType.SYMBOL,
                                       AIFPLTokenType.STRING, AIFPLTokenType.BOOLEAN):
            return self._parse_atom(start_pos)

        raise AIFPLParseError(f"Unexpected token: {self.current_token.value} at position {self.current_token.position}")

    def _parse_list(self, start_pos: int) -> ParsedExpression:
        """Parse (operator arg1 arg2 ...)"""
        self._consume(AIFPLTokenType.LPAREN)

        elements = []
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            if self.current_token.type == AIFPLTokenType.EOF:
                raise AIFPLParseError(f"Unclosed parenthesis starting at position {start_pos}")
            elements.append(self._parse_expression().expr)

        if self.current_token is None:
            raise AIFPLParseError(f"Unterminated list starting at position {start_pos}, expected ')'")

        end_pos = self.current_token.position
        self._consume(AIFPLTokenType.RPAREN)

        if not elements:
            raise AIFPLParseError(f"Empty list at position {start_pos}")

        return ParsedExpression(elements, start_pos, end_pos)

    def _parse_atom(self, start_pos: int) -> ParsedExpression:
        """Parse an atomic value (number, string, boolean, or symbol)."""
        assert self.current_token is not None, "_parse_atom called with None token"

        token = self.current_token
        self._advance()

        end_pos = start_pos + token.length
        return ParsedExpression(token.value, start_pos, end_pos)

    def _consume(self, expected_type: AIFPLTokenType) -> None:
        """Consume a token of the expected type."""
        if self.current_token is None:
            raise AIFPLParseError(f"Unexpected end of input, expected {expected_type.value}")

        if self.current_token.type != expected_type:
            raise AIFPLParseError(
                f"Expected {expected_type.value}, got {self.current_token.value} at position {self.current_token.position}"
            )

        self._advance()

    def _advance(self) -> None:
        """Move to the next token."""
        self.pos += 1
        if self.pos < len(self.tokens):
            self.current_token = self.tokens[self.pos]

        else:
            self.current_token = None
