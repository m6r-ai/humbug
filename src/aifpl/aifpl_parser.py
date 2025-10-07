"""Parser for AIFPL expressions with detailed error messages."""

from typing import List
from aifpl.aifpl_error import AIFPLParseError, ErrorMessageBuilder
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_value import AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList


class AIFPLParser:
    """Parses tokens into an Abstract Syntax Tree using pure list representation with detailed error messages."""

    def __init__(self, tokens: List[AIFPLToken], expression: str = ""):
        """
        Initialize parser with tokens and original expression.

        Args:
            tokens: List of tokens to parse
            expression: Original expression string for error context
        """
        self.tokens = tokens
        self.pos = 0
        self.current_token: AIFPLToken | None = tokens[0] if tokens else None
        self.expression = expression
        self.message_builder = ErrorMessageBuilder()

    def parse(self) -> AIFPLValue:
        """
        Parse tokens into AST with detailed error reporting.

        Returns:
            Parsed expression

        Raises:
            AIFPLParseError: If parsing fails with detailed context
        """
        if self.current_token is None:
            raise AIFPLParseError(
                message="Empty expression",
                expected="Valid AIFPL expression",
                example="(+ 1 2) or 42 or \"hello\"",
                suggestion="Provide a complete expression to evaluate",
                context="Expression cannot be empty or contain only whitespace"
            )

        expr = self._parse_expression()

        if self.current_token is not None:
            current_value = self.current_token.value if self.current_token else "EOF"
            current_pos = self.current_token.position if self.current_token else len(self.expression)

            raise AIFPLParseError(
                message="Unexpected token after complete expression",
                position=current_pos,
                received=f"Found: {current_value}",
                expected="End of expression",
                example="Correct: (+ 1 2)\\nIncorrect: (+ 1 2) extra",
                suggestion="Remove extra tokens or combine into single expression",
                context="Each evaluation can only handle one complete expression"
            )

        return expr

    def _parse_expression(self) -> AIFPLValue:
        """Parse a single expression with detailed error reporting."""
        assert self.current_token is not None, "Current token must not be None here"
        start_pos = self.current_token.position
        token = self.current_token

        if token.type == AIFPLTokenType.LPAREN:
            return self._parse_list(start_pos)

        if token.type == AIFPLTokenType.QUOTE:
            return self._parse_quoted_expression()

        if token.type == AIFPLTokenType.SYMBOL:
            self._advance()
            return AIFPLSymbol(token.value, token.position)

        if token.type == AIFPLTokenType.NUMBER:
            self._advance()
            return AIFPLNumber(token.value)

        if token.type == AIFPLTokenType.STRING:
            self._advance()
            return AIFPLString(token.value)

        if token.type == AIFPLTokenType.BOOLEAN:
            self._advance()
            return AIFPLBoolean(token.value)

        # Enhanced error for unexpected tokens
        assert token.type == AIFPLTokenType.RPAREN, f"Unexpected token type ({token.type}) encountered"
        token_value = token.value
        token_type = token.type.name

        raise AIFPLParseError(
            message=f"Unexpected token: {token_value}",
            position=start_pos,
            received=f"Token: {token_value} (type: {token_type})",
            expected="Number, string, boolean, symbol, '(', or '",
            example="Valid starts: 42, \"hello\", #t, symbol, (, '",
            suggestion="List expressions must start with '(' and quoted expressions with '",
            context=f"Token '{token_value}' cannot start an expression"
        )

    def _parse_list(self, start_pos: int) -> AIFPLList:
        """Parse (element1 element2 ...) with detailed error reporting."""
        self._advance()  # consume '('

        elements = []
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            elements.append(self._parse_expression())

        if self.current_token is None:
            raise AIFPLParseError(
                message="Unterminated list - missing closing parenthesis",
                position=start_pos,
                expected="Closing parenthesis ')'",
                example="Correct: (+ 1 2)\\nIncorrect: (+ 1 2",
                suggestion="Add ')' to close the list",
                context=f"List starting at position {start_pos} was never closed"
            )

        self._advance()  # consume ')'

        return AIFPLList(tuple(elements))

    def _parse_quoted_expression(self) -> AIFPLList:
        """
        Parse 'expr and convert to (quote expr) with detailed error reporting.

        Returns:
            AIFPLList representing (quote expr)

        Raises:
            AIFPLParseError: If quote is incomplete or malformed
        """
        quote_pos = self.current_token.position if self.current_token else 0
        self._advance()  # consume quote

        # Check if we have something to quote
        if self.current_token is None:
            raise AIFPLParseError(
                message="Incomplete quote expression",
                position=quote_pos,
                received="Quote symbol ' with nothing to quote",
                expected="Expression after quote symbol",
                example="Correct: '(a b c) or 'symbol\\nIncorrect: ' (nothing after)",
                suggestion="Add an expression after the ' symbol",
                context="Quote symbol must be followed by something to quote"
            )

        # Parse the expression to be quoted
        quoted_expr = self._parse_expression()

        # Transform 'expr into (quote expr)
        quote_symbol = AIFPLSymbol("quote", quote_pos)
        return AIFPLList((quote_symbol, quoted_expr))

    def _advance(self) -> None:
        """Move to the next token."""
        self.pos += 1
        if self.pos < len(self.tokens):
            self.current_token = self.tokens[self.pos]

        else:
            self.current_token = None
