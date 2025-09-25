"""Parser for AIFPL expressions with detailed error messages."""

from typing import List
from aifpl.aifpl_detailed_error import (
    AIFPLDetailedParseError, ErrorContext, ErrorMessageBuilder
)
from aifpl.aifpl_error import AIFPLParseError
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

    def _create_detailed_error(
        self,
        message: str,
        position: int = None,
        context: str = None,
        expected: str = None,
        received: str = None,
        suggestion: str = None,
        example: str = None,
        error_type: str = "general"
    ) -> AIFPLDetailedParseError:
        """Create a detailed parse error with full context."""
        
        # Use current token position if none provided
        if position is None and self.current_token:
            position = self.current_token.position
        
        error_context = None
        if position is not None and self.expression:
            line, column = self.message_builder.position_to_line_column(
                self.expression, position
            )
            expr_context = self.message_builder.get_expression_context(
                self.expression, position
            )
            error_context = ErrorContext(
                line=line,
                column=column,
                expression=expr_context,
                position=position
            )
        
        # Add common mistake suggestions if none provided
        if not suggestion:
            suggestion = self.message_builder.get_common_mistakes_suggestion(error_type, context or "")
        
        return AIFPLDetailedParseError(
            message=message,
            context=context,
            expected=expected,
            received=received,
            suggestion=suggestion,
            example=example,
            error_context=error_context
        )

    def parse(self) -> AIFPLValue:
        """
        Parse tokens into AST with detailed error reporting.

        Returns:
            Parsed expression

        Raises:
            AIFPLDetailedParseError: If parsing fails with detailed context
        """
        if self.current_token is None or self.current_token.type == AIFPLTokenType.EOF:
            raise self._create_detailed_error(
                message="Empty expression",
                expected="Valid AIFPL expression",
                example="(+ 1 2) or 42 or \"hello\"",
                suggestion="Provide a complete expression to evaluate",
                context="Expression cannot be empty or contain only whitespace"
            )

        expr = self._parse_expression()

        if self.current_token is None or self.current_token.type != AIFPLTokenType.EOF:
            current_value = self.current_token.value if self.current_token else "EOF"
            current_pos = self.current_token.position if self.current_token else len(self.expression)
            
            raise self._create_detailed_error(
                message="Unexpected token after complete expression",
                position=current_pos,
                received=f"Found: {current_value}",
                expected="End of expression",
                example="Correct: (+ 1 2)\nIncorrect: (+ 1 2) extra",
                suggestion="Remove extra tokens or combine into single expression",
                context="Each evaluation can only handle one complete expression"
            )

        return expr

    def _parse_expression(self) -> AIFPLValue:
        """Parse a single expression with detailed error reporting."""
        if self.current_token is None:
            raise self._create_detailed_error(
                message="Unexpected end of input",
                expected="Valid expression (atom or list)",
                example="42, \"hello\", (+ 1 2), or 'symbol",
                suggestion="Provide a complete expression",
                context="Expression ended unexpectedly"
            )

        start_pos = self.current_token.position

        if self.current_token.type == AIFPLTokenType.LPAREN:
            return self._parse_list(start_pos)

        if self.current_token.type == AIFPLTokenType.QUOTE:
            return self._parse_quoted_expression()

        if self.current_token.type in (AIFPLTokenType.NUMBER, AIFPLTokenType.SYMBOL,
                                       AIFPLTokenType.STRING, AIFPLTokenType.BOOLEAN):
            return self._parse_atom()

        # Enhanced error for unexpected tokens
        token_value = self.current_token.value
        token_type = self.current_token.type.name
        
        suggestions = {
            AIFPLTokenType.RPAREN: "Missing opening parenthesis '(' or extra closing parenthesis",
            AIFPLTokenType.EOF: "Expression ended unexpectedly"
        }
        
        suggestion = suggestions.get(self.current_token.type, f"'{token_value}' is not a valid start of expression")

        raise self._create_detailed_error(
            message=f"Unexpected token: {token_value}",
            position=start_pos,
            received=f"Token: {token_value} (type: {token_type})",
            expected="Number, string, boolean, symbol, '(', or '",
            example="Valid starts: 42, \"hello\", #t, symbol, (, '",
            suggestion=suggestion,
            context=f"Token '{token_value}' cannot start an expression"
        )

    def _parse_list(self, start_pos: int) -> AIFPLList:
        """Parse (element1 element2 ...) with detailed error reporting."""
        self._consume(AIFPLTokenType.LPAREN)

        elements = []
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            if self.current_token.type == AIFPLTokenType.EOF:
                raise self._create_detailed_error(
                    message="Unclosed parenthesis",
                    position=start_pos,
                    received="Reached end of expression",
                    expected="Closing parenthesis ')'",
                    example="Correct: (+ 1 2)\nIncorrect: (+ 1 2",
                    suggestion="Add missing ')' at the end",
                    context=f"Opening '(' at position {start_pos} was never closed"
                )

            elements.append(self._parse_expression())

        if self.current_token is None:
            raise self._create_detailed_error(
                message="Unterminated list - missing closing parenthesis",
                position=start_pos,
                expected="Closing parenthesis ')'",
                example="Correct: (+ 1 2)\nIncorrect: (+ 1 2",
                suggestion="Add ')' to close the list",
                context=f"List starting at position {start_pos} was never closed"
            )

        self._consume(AIFPLTokenType.RPAREN)

        return AIFPLList(tuple(elements))

    def _parse_quoted_expression(self) -> AIFPLList:
        """
        Parse 'expr and convert to (quote expr) with detailed error reporting.

        Returns:
            AIFPLList representing (quote expr)

        Raises:
            AIFPLDetailedParseError: If quote is incomplete or malformed
        """
        quote_pos = self.current_token.position if self.current_token else 0
        self._consume(AIFPLTokenType.QUOTE)

        # Check if we have something to quote
        if self.current_token is None or self.current_token.type == AIFPLTokenType.EOF:
            raise self._create_detailed_error(
                message="Incomplete quote expression",
                position=quote_pos,
                received="Quote symbol ' with nothing to quote",
                expected="Expression after quote symbol",
                example="Correct: '(a b c) or 'symbol\nIncorrect: ' (nothing after)",
                suggestion="Add an expression after the ' symbol",
                context="Quote symbol must be followed by something to quote"
            )

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
        """Consume a token of the expected type with detailed error reporting."""
        if self.current_token is None:
            raise self._create_detailed_error(
                message="Unexpected end of input",
                expected=f"Token of type {expected_type.value}",
                example=f"Expected: {expected_type.value}",
                suggestion="Complete the expression",
                context="Expression ended before required token"
            )

        if self.current_token.type != expected_type:
            current_value = self.current_token.value
            current_pos = self.current_token.position
            
            # Special handling for common mistakes
            if expected_type == AIFPLTokenType.RPAREN and self.current_token.type == AIFPLTokenType.EOF:
                suggestion = "Add missing ')' to close parentheses"
            elif expected_type == AIFPLTokenType.LPAREN and self.current_token.type == AIFPLTokenType.RPAREN:
                suggestion = "Check parentheses balance - may have extra ')'"
            else:
                suggestion = f"Replace '{current_value}' with '{expected_type.value}'"
            
            raise self._create_detailed_error(
                message=f"Expected {expected_type.value}, got {current_value}",
                position=current_pos,
                received=f"Found: {current_value}",
                expected=f"Required: {expected_type.value}",
                suggestion=suggestion,
                context=f"Parser expected {expected_type.value} at this position"
            )

        self._advance()

    def _advance(self) -> None:
        """Move to the next token."""
        self.pos += 1
        if self.pos < len(self.tokens):
            self.current_token = self.tokens[self.pos]

        else:
            self.current_token = None