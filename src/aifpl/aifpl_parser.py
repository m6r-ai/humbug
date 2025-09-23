"""Parser for AIFPL expressions."""

from typing import List

from aifpl.aifpl_error import AIFPLParseError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_value import AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList


class AIFPLParser:
    """Parses tokens into an Abstract Syntax Tree using pure list representation."""

    def __init__(self, tokens: List[AIFPLToken]):
        """
        Initialize parser with tokens.

        Args:
            tokens: List of tokens to parse
        """
        self.tokens = tokens
        self.pos = 0
        self.current_token: AIFPLToken | None = tokens[0] if tokens else None

    def parse(self) -> AIFPLValue:
        """
        Parse tokens into AST.

        Returns:
            Parsed expression

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

    def _parse_expression(self) -> AIFPLValue:
        """Parse a single expression (atom or list)."""
        if self.current_token is None:
            raise AIFPLParseError("Unexpected end of input, expected expression")

        start_pos = self.current_token.position

        if self.current_token.type == AIFPLTokenType.LPAREN:
            return self._parse_list(start_pos)

        if self.current_token.type == AIFPLTokenType.QUOTE:
            return self._parse_quoted_expression()

        if self.current_token.type in (AIFPLTokenType.NUMBER, AIFPLTokenType.SYMBOL,
                                       AIFPLTokenType.STRING, AIFPLTokenType.BOOLEAN):
            return self._parse_atom()

        raise AIFPLParseError(f"Unexpected token: {self.current_token.value} at position {self.current_token.position}")

    def _parse_list(self, start_pos: int) -> AIFPLList:
        """Parse (element1 element2 ...) - everything is just a list."""
        self._consume(AIFPLTokenType.LPAREN)

        elements = []
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            if self.current_token.type == AIFPLTokenType.EOF:
                raise AIFPLParseError(f"Unclosed parenthesis starting at position {start_pos}")

            elements.append(self._parse_expression())

        if self.current_token is None:
            raise AIFPLParseError(f"Unterminated list starting at position {start_pos}, expected ')'")

        self._consume(AIFPLTokenType.RPAREN)

        # Create AIFPLList - no special handling for lambda, let, function calls
        # Everything is just data (lists and atoms)
        return AIFPLList(tuple(elements))

    def _parse_quoted_expression(self) -> AIFPLList:
        """
        Parse 'expr and convert to (quote expr).

        Returns:
            AIFPLList representing (quote expr)

        Raises:
            AIFPLParseError: If quote is incomplete or malformed
        """
        quote_pos = self.current_token.position if self.current_token else 0
        self._consume(AIFPLTokenType.QUOTE)

        # Check if we have something to quote
        if self.current_token is None or self.current_token.type == AIFPLTokenType.EOF:
            raise AIFPLParseError(f"Incomplete quote at position {quote_pos}: expected expression after '")

        # Parse the expression to be quoted
        quoted_expr = self._parse_expression()

        # Transform 'expr into (quote expr)
        quote_symbol = AIFPLSymbol("quote", quote_pos)
        return AIFPLList((quote_symbol, quoted_expr))

    def _parse_atom(self) -> AIFPLValue:
        """Parse an atomic value and convert to appropriate AIFPLValue."""
        assert self.current_token is not None, "_parse_atom called with None token"

        token = self.current_token
        self._advance()

        # Convert tokens to appropriate AIFPLValue types
        if token.type == AIFPLTokenType.SYMBOL:
            return AIFPLSymbol(token.value, token.position)

        if token.type == AIFPLTokenType.NUMBER:
            return AIFPLNumber(token.value)

        if token.type == AIFPLTokenType.STRING:
            return AIFPLString(token.value)

        if token.type == AIFPLTokenType.BOOLEAN:
            return AIFPLBoolean(token.value)

        raise AIFPLParseError(f"Unexpected token type: {token.type}")

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