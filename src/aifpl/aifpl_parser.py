"""Parser for AIFPL expressions with detailed error messages."""

from typing import List
from dataclasses import dataclass
from aifpl.aifpl_error import AIFPLParseError, ErrorMessageBuilder
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_value import AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList


@dataclass
class ParenStackFrame:
    """Represents an unclosed opening parenthesis with context."""
    position: int
    expression_type: str
    context_snippet: str


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

        # Paren stack for tracking unclosed expressions
        self.paren_stack: List[ParenStackFrame] = []

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

    def _push_paren_frame(self, position: int) -> None:
        """
        Push a new opening paren onto the tracking stack.

        Args:
            position: Character position of the opening paren
        """
        expr_type = self._detect_expression_type(position)
        snippet = self._get_context_snippet(position, length=30)

        frame = ParenStackFrame(
            position=position,
            expression_type=expr_type,
            context_snippet=snippet
        )

        self.paren_stack.append(frame)

    def _pop_paren_frame(self) -> None:
        """Pop an opening paren from the stack when it's successfully closed."""
        assert self.paren_stack, "Paren stack underflow - trying to pop from empty stack"
        self.paren_stack.pop()

    def _detect_expression_type(self, position: int) -> str:
        """
        Detect what type of expression starts at this position.

        Looks at the first symbol after the opening paren to classify
        the expression type (let, lambda, if, etc.)

        Args:
            position: Character position of the opening paren

        Returns:
            Human-readable expression type string
        """
        # Skip past the opening paren and whitespace
        i = position + 1
        while i < len(self.expression) and self.expression[i].isspace():
            i += 1

        if i >= len(self.expression):
            return "list"

        # Read the first symbol
        symbol_start = i
        while i < len(self.expression) and (self.expression[i].isalnum() or self.expression[i] in '-+*/?_'):
            i += 1

        first_symbol = self.expression[symbol_start:i]

        # Classify based on first symbol
        special_forms = {
            'let': 'let binding',
            'lambda': 'lambda function',
            'if': 'if expression',
            'match': 'match expression',
            'quote': 'quote expression',
            'map': 'map call',
            'filter': 'filter call',
            'fold': 'fold call',
            'define': 'define',
            'cond': 'cond expression',
            'and': 'and expression',
            'or': 'or expression',
            'not': 'not expression',
        }

        return special_forms.get(first_symbol, 'list/function call')

    def _get_context_snippet(self, position: int, length: int = 30) -> str:
        """
        Get a snippet of code starting at position for error display.

        Args:
            position: Starting character position
            length: Maximum length of snippet

        Returns:
            Formatted context snippet with ellipsis if truncated
        """
        end = min(position + length, len(self.expression))
        snippet = self.expression[position:end]

        # Clean up whitespace for display (collapse multiple spaces)
        snippet = ' '.join(snippet.split())

        # Add ellipsis if truncated
        if end < len(self.expression):
            snippet += "..."

        return snippet

    def _create_enhanced_unterminated_error(self, start_pos: int) -> AIFPLParseError:
        """
        Create enhanced error message with paren stack information.

        Args:
            start_pos: Position where the unterminated list started

        Returns:
            AIFPLParseError with detailed stack trace
        """
        depth = len(self.paren_stack)

        # Build stack trace showing all unclosed expressions
        stack_lines = []
        for i, frame in enumerate(self.paren_stack, 1):
            stack_lines.append(
                f"  {i}. {frame.expression_type} at position {frame.position}: "
                f"{frame.context_snippet}"
            )

        stack_trace = "\n".join(stack_lines) if stack_lines else "  (no unclosed expressions)"

        # Build closing parens suggestion with spaces for readability
        if depth > 1:
            closing_parens = " ) " * depth
            closing_parens = closing_parens.strip()

        else:
            closing_parens = ")"

        # Create the context message
        context_msg = (
            f"Reached end of input at depth {depth}.\n\n"
            f"Unclosed expressions:\n{stack_trace}"
        )

        # Determine singular vs plural
        paren_word = "parenthesis" if depth == 1 else "parentheses"

        return AIFPLParseError(
            message=f"Unterminated list - missing {depth} closing {paren_word}",
            position=start_pos,
            expected=f'Add "{closing_parens}" to close all expressions',
            example="Correct: (+ 1 2)\nIncorrect: (+ 1 2",
            suggestion=f"Add {depth} closing {paren_word}: {closing_parens}",
            context=context_msg
        )

    def _parse_list(self, start_pos: int) -> AIFPLList:
        """Parse (element1 element2 ...) with enhanced error tracking."""
        # Push opening paren onto tracking stack
        self._push_paren_frame(start_pos)

        self._advance()  # consume '('

        elements = []
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            elements.append(self._parse_expression())

        if self.current_token is None:
            # Use enhanced error with stack trace
            raise self._create_enhanced_unterminated_error(start_pos)

        # Pop from stack when successfully closed
        self._pop_paren_frame()

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
