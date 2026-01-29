"""Parser for AIFPL expressions with detailed error messages."""

from typing import List
from dataclasses import dataclass
from aifpl.aifpl_error import AIFPLParseError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex,
    AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList
)


@dataclass
class ParenStackFrame:
    """Represents an unclosed opening parenthesis with context."""
    line: int
    column: int
    parser: 'AIFPLParser'  # Reference to parser for lazy evaluation
    _expression_type: str | None = None  # Cached lazily
    elements_parsed: int = 0
    last_complete_line: int | None = None
    last_complete_column: int | None = None
    related_symbol: str | None = None  # For bindings: the variable name
    incomplete_element_line: int | None = None
    incomplete_element_column: int | None = None

    def get_expression_type(self) -> str:
        """Lazily compute expression type only when needed."""
        if self._expression_type is None:
            self._expression_type = self.parser.detect_expression_type(self.line, self.column)

        return self._expression_type

    def set_expression_type(self, value: str) -> None:
        """Allow overriding expression type for more specific error messages."""
        self._expression_type = value

    def get_context_snippet(self) -> str:
        """Lazily compute context snippet only when needed."""
        return self.parser.get_context_snippet(self.line, self.column, length=30)


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

        # Paren stack for tracking unclosed expressions
        self.paren_stack: List[ParenStackFrame] = []

        # Track the line/column of the last token we consumed
        self.last_token_end_line: int = 1
        self.last_token_end_column: int = 1

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
            current_line = self.current_token.line if self.current_token else 1
            current_col = self.current_token.column if self.current_token else 1

            raise AIFPLParseError(
                message="Unexpected token after complete expression",
                line=current_line,
                column=current_col,
                received=f"Found: {current_value}",
                expected="End of expression",
                example="Correct: (+ 1 2)\\nIncorrect: (+ 1 2) extra",
                suggestion="Remove extra tokens or combine into single expression",
                context="Each evaluation can only handle one complete expression",
                source=self.expression
            )

        return expr

    def _parse_expression(self) -> AIFPLValue:
        """Parse a single expression with detailed error reporting."""
        assert self.current_token is not None, "Current token must not be None here"
        start_line = self.current_token.line
        start_col = self.current_token.column
        token = self.current_token

        if token.type == AIFPLTokenType.LPAREN:
            return self._parse_list(start_line, start_col)

        if token.type == AIFPLTokenType.QUOTE:
            return self._parse_quoted_expression()

        if token.type == AIFPLTokenType.SYMBOL:
            self._advance()
            return AIFPLSymbol(token.value)

        if token.type == AIFPLTokenType.INTEGER:
            self._advance()
            return AIFPLInteger(token.value)

        if token.type == AIFPLTokenType.FLOAT:
            self._advance()
            return AIFPLFloat(token.value)

        if token.type == AIFPLTokenType.COMPLEX:
            self._advance()
            return AIFPLComplex(token.value)

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
            line=token.line,
            column=token.column,
            received=f"Token: {token_value} (type: {token_type})",
            expected="Number, string, boolean, symbol, '(', or '",
            example="Valid starts: 42, \"hello\", #t, symbol, (, '",
            suggestion="List expressions must start with '(' and quoted expressions with '",
            context=f"Token '{token_value}' cannot start an expression",
            source=self.expression
        )

    def _push_paren_frame(self, line: int, column: int) -> ParenStackFrame:
        """
        Push a new opening paren onto the tracking stack.

        Args:
            line: Line number (1-indexed)
            column: Column number (1-indexed)

        Returns:
            The created frame (so caller can update it)
        """
        frame = ParenStackFrame(
            line=line,
            column=column,
            parser=self  # Pass parser reference for lazy evaluation
        )

        self.paren_stack.append(frame)
        return frame

    def _pop_paren_frame(self) -> None:
        """Pop an opening paren from the stack when it's successfully closed."""
        assert self.paren_stack, "Paren stack underflow - trying to pop from empty stack"
        self.paren_stack.pop()

    def _mark_element_start(self) -> None:
        """Mark that we're starting to parse a new element."""
        assert self.paren_stack, "Should only be called while parsing a list"
        assert self.current_token is not None, "Should not be called at EOF"
        frame = self.paren_stack[-1]
        frame.incomplete_element_line = self.current_token.line
        frame.incomplete_element_column = self.current_token.column

    def _update_frame_after_element(self) -> None:
        """Update the current frame after successfully parsing an element."""
        # This method is only called after _push_paren_frame, so stack is never empty
        assert self.paren_stack, "Frame stack should not be empty when updating after element"

        frame = self.paren_stack[-1]
        frame.elements_parsed += 1

        # Record line/column where the last element ended
        frame.last_complete_line = self.last_token_end_line
        frame.last_complete_column = self.last_token_end_column

        # Clear the incomplete element start since we just completed an element
        frame.incomplete_element_line = None
        frame.incomplete_element_column = None

    def _line_col_to_char(self,source: str, line: int, column: int) -> int:
        """
        Convert (line, column) to character position.

        Args:
            source: The source code string
            line: Line number (1-indexed)
            column: Column number (1-indexed)

        Returns:
            Character offset (0-indexed)
        """
        if line < 1 or column < 1:
            return 0

        lines = source.split('\n')

        # If line is beyond source, return end position
        if line > len(lines):
            return len(source)

        # Calculate position: sum of all previous lines + newlines + column offset
        pos = 0
        for i in range(line - 1):
            pos += len(lines[i]) + 1  # +1 for the newline character

        # Add column offset (column is 1-indexed, so subtract 1)
        pos += min(column - 1, len(lines[line - 1]))

        return pos

    def detect_expression_type(self, line: int, column: int) -> str:
        """
        Detect what type of expression starts at this position.

        Looks at the first symbol after the opening paren to classify
        the expression type (let, lambda, if, etc.)

        Args:
            line: Line number (1-indexed)
            column: Column number (1-indexed)

        Returns:
            Human-readable expression type string
        """
        # Convert line/column to character position
        position = self._line_col_to_char(self.expression, line, column)

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
            'letrec': 'letrec binding',
            'lambda': 'lambda function',
            'if': 'if expression',
            'match': 'match expression',
            'quote': 'quote expression',
            'and': 'and expression',
            'or': 'or expression'
        }

        return special_forms.get(first_symbol, 'list/function call')

    def get_context_snippet(self, line: int, column: int, length: int = 30) -> str:
        """
        Get a snippet of code starting at position for error display.

        Args:
            line: Line number (1-indexed)
            column: Column number (1-indexed)
            length: Maximum length of snippet

        Returns:
            Formatted context snippet with ellipsis if truncated
        """
        # Convert line/column to character position
        position = self._line_col_to_char(self.expression, line, column)

        end = min(position + length, len(self.expression))
        snippet = self.expression[position:end]

        # Clean up whitespace for display (collapse multiple spaces)
        snippet = ' '.join(snippet.split())

        # Add ellipsis if truncated
        if end < len(self.expression):
            snippet += "..."

        return snippet

    def _create_enhanced_unterminated_error(self, start_line: int, start_col: int) -> AIFPLParseError:
        """
        Create enhanced error message with paren stack information.

        Args:
            start_line: Line where the unterminated list started
            start_col: Column where the unterminated list started

        Returns:
            AIFPLParseError with detailed stack trace
        """
        depth = len(self.paren_stack)

        # Build stack trace showing all unclosed expressions
        stack_lines = []
        for i, frame in enumerate(self.paren_stack, 1):
            line = f"  {i}. {frame.get_expression_type()} at line {frame.line}, column {frame.column}"

            # Add related symbol if available (e.g., binding variable name)
            if frame.related_symbol:
                line += f" ('{frame.related_symbol}')"

            line += f": {frame.get_context_snippet()}"

            # Show how many elements were parsed
            if frame.elements_parsed > 0:
                line += f"\n     Parsed {frame.elements_parsed} complete element{'s' if frame.elements_parsed != 1 else ''}"

            # Check if we're in the middle of parsing an incomplete element
            if frame.incomplete_element_line is not None and frame.incomplete_element_column is not None:
                # This frame was parsing an element that's incomplete
                incomplete_snippet = self.get_context_snippet(
                    frame.incomplete_element_line, frame.incomplete_element_column, length=20
                )
                line += f"\n     Started parsing element {frame.elements_parsed + 1} at "
                line += f"line {frame.incomplete_element_line}, column {frame.incomplete_element_column}: {incomplete_snippet}"
                line += "\n     → This element is incomplete (see below)"

            else:
                # This is the innermost frame - show where to add closing paren
                if frame.last_complete_line is not None:
                    line += f"\n     → Needs ')' after line {frame.last_complete_line}, column {frame.last_complete_column}"

                else:
                    line += "\n     → Needs ')' to close this expression"

            stack_lines.append(line)

        stack_trace = "\n".join(stack_lines) if stack_lines else "  (no unclosed expressions)"

        closing_parens = ")" * depth

        # Create the context message
        context_msg = (
            f"Reached end of input at depth {depth}.\n\n"
            f"Unclosed expressions (innermost to outermost):\n{stack_trace}"
        )

        # Determine singular vs plural
        paren_word = "parenthesis" if depth == 1 else "parentheses"

        return AIFPLParseError(
            message=f"Unterminated list - missing {depth} closing {paren_word}",
            line=start_line,
            column=start_col,
            expected=f'Additional parentheses, "{closing_parens}", to close all expressions',
            example="Correct: (+ 1 2)\nIncorrect: (+ 1 2",
            suggestion="Close each incomplete expression with ')', working from innermost to outermost",
            context=context_msg,
            source=self.expression
        )

    def _parse_list(self, start_line: int, start_col: int) -> AIFPLList:
        """Parse (element1 element2 ...) with enhanced error tracking."""
        # Push opening paren onto tracking stack
        frame = self._push_paren_frame(start_line, start_col)

        self._advance()  # consume '('

        elements: List[AIFPLValue] = []

        # Check if this is a 'let' form to enable special tracking
        if (
            self.current_token and
            self.current_token.type == AIFPLTokenType.SYMBOL and
            self.current_token.value in ('let', 'letrec')
        ):
            return self._parse_let_with_tracking(start_line, start_col, frame, elements)

        # Regular list parsing
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            self._mark_element_start()
            elements.append(self._parse_expression())
            self._update_frame_after_element()

        if self.current_token is None:
            # Use enhanced error with stack trace
            raise self._create_enhanced_unterminated_error(start_line, start_col)

        # Pop from stack when successfully closed
        self._pop_paren_frame()

        self._advance()  # consume ')'

        return AIFPLList(tuple(elements))

    def _parse_let_with_tracking(
        self,
        start_line: int,
        start_col: int,
        _frame: ParenStackFrame,
        elements: List[AIFPLValue]
    ) -> AIFPLList:
        """
        Parse a 'let' form with special tracking for binding-level errors.

        Args:
            start_line: Line where the let started
            start_col: Column where the let started
            _frame: The paren stack frame for this let (unused but kept for consistency)
            elements: List to accumulate parsed elements

        Returns:
            Parsed let expression as AIFPLList
        """
        # Parse 'let' keyword
        self._mark_element_start()
        elements.append(self._parse_expression())
        self._update_frame_after_element()

        # Check for bindings list
        if self.current_token is None:
            raise self._create_enhanced_unterminated_error(start_line, start_col)

        # If we hit a closing paren right after 'let', just return what we have
        # and let the evaluator complain about the structure
        if self.current_token.type == AIFPLTokenType.RPAREN:
            self._pop_paren_frame()
            self._advance()  # consume ')'
            return AIFPLList(tuple(elements))

        # Parse bindings with special tracking
        self._mark_element_start()
        if self.current_token.type == AIFPLTokenType.LPAREN:
            bindings = self._parse_let_bindings()
            elements.append(bindings)
            self._update_frame_after_element()

        else:
            # Not our job to validate structure - just parse what's there
            elements.append(self._parse_expression())
            self._update_frame_after_element()

        # Parse body
        if self.current_token is None:
            raise self._create_enhanced_unterminated_error(start_line, start_col)

        if self.current_token.type != AIFPLTokenType.RPAREN:
            self._mark_element_start()
            elements.append(self._parse_expression())
            self._update_frame_after_element()

        # Expect closing paren
        if self.current_token is None:
            raise self._create_enhanced_unterminated_error(start_line, start_col)

        # Pop from stack when successfully closed
        self._pop_paren_frame()

        self._advance()  # consume ')'

        return AIFPLList(tuple(elements))

    def _parse_let_bindings(self) -> AIFPLList:
        """
        Parse the bindings list of a let or letrec form with per-binding tracking.

        Returns:
            AIFPLList of bindings
        """
        assert self.current_token is not None, "Current token must not be None here"
        bindings_start_line = self.current_token.line
        bindings_start_col = self.current_token.column

        # Push frame for bindings list
        bindings_frame = self._push_paren_frame(bindings_start_line, bindings_start_col)
        bindings_frame.set_expression_type("bindings list")

        self._advance()  # consume '('

        bindings: List[AIFPLValue] = []
        binding_index = 0

        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            binding_index += 1

            # Each binding should start with '('
            self._mark_element_start()
            if self.current_token.type == AIFPLTokenType.LPAREN:
                binding = self._parse_single_binding(binding_index)
                bindings.append(binding)
                self._update_frame_after_element()

            else:
                # Not a binding structure - just parse it and let evaluator complain
                bindings.append(self._parse_expression())
                self._update_frame_after_element()

        if self.current_token is None:
            # EOF while parsing bindings - create enhanced error
            raise self._create_incomplete_bindings_error(bindings, bindings_start_line, bindings_start_col)

        # Pop bindings frame
        self._pop_paren_frame()

        self._advance()  # consume ')'

        return AIFPLList(tuple(bindings))

    def _parse_single_binding(self, binding_index: int) -> AIFPLList:
        """
        Parse a single let or letrec binding with tracking.

        Args:
            binding_index: The index of this binding (1-based)

        Returns:
            AIFPLList representing the binding
        """
        assert self.current_token is not None, "Current token must not be None here"
        binding_start_line = self.current_token.line
        binding_start_col = self.current_token.column

        # Push frame for this binding
        binding_frame = self._push_paren_frame(binding_start_line, binding_start_col)
        binding_frame.set_expression_type(f"binding #{binding_index}")

        self._advance()  # consume '('

        elements = []

        # Parse variable name (if present)
        if self.current_token is not None and self.current_token.type == AIFPLTokenType.SYMBOL:
            var_name = self.current_token.value
            binding_frame.related_symbol = var_name
            binding_frame.set_expression_type(f"binding #{binding_index} ('{var_name}')")
            self._mark_element_start()
            elements.append(self._parse_expression())
            self._update_frame_after_element()

        elif self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            # Not a symbol, but parse it anyway (for error recovery)
            self._mark_element_start()
            elements.append(self._parse_expression())
            self._update_frame_after_element()

        # Parse value (if present)
        if self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            self._mark_element_start()
            elements.append(self._parse_expression())
            self._update_frame_after_element()

        # Parse any additional elements (evaluator will complain about wrong count)
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            self._mark_element_start()
            elements.append(self._parse_expression())
            self._update_frame_after_element()

        if self.current_token is None:
            # EOF while parsing binding
            raise self._create_enhanced_unterminated_error(binding_start_line, binding_start_col)

        # Pop binding frame
        self._pop_paren_frame()

        self._advance()  # consume ')'

        return AIFPLList(tuple(elements))

    def _create_incomplete_bindings_error(
        self,
        parsed_bindings: List[AIFPLValue],
        bindings_start_line: int,
        bindings_start_col: int
    ) -> AIFPLParseError:
        """
        Create enhanced error when EOF is reached while parsing let bindings.

        Args:
            parsed_bindings: List of successfully parsed bindings
            bindings_start_line: Line where bindings list started
            bindings_start_col: Column where bindings list started

        Returns:
            AIFPLParseError with detailed context
        """
        # Analyze the parsed bindings to show what completed successfully
        binding_summary = []
        for i, binding in enumerate(parsed_bindings, 1):
            if isinstance(binding, AIFPLList):
                symbol_binding = binding.get(0)
                if binding.length() >= 1 and isinstance(symbol_binding, AIFPLSymbol):
                    var_name = symbol_binding.name
                    status = "✓" if binding.length() == 2 else "✗"
                    binding_summary.append(f"  {i}. ({var_name} ...) {status}")

                else:
                    binding_summary.append(f"  {i}. <invalid binding> ✗")

            else:
                binding_summary.append(f"  {i}. <not a list> ✗")

        summary_text = "\n".join(binding_summary) if binding_summary else "  (no complete bindings)"

        # Build the enhanced error using the paren stack
        depth = len(self.paren_stack)
        assert depth >= 2, "Bindings error should have at least 2 frames on stack"

        # Get details from stack
        stack_lines = []
        for i, frame in enumerate(self.paren_stack, 1):
            line = f"  {i}. {frame.get_expression_type()} at line {frame.line}, column {frame.column}"

            if frame.elements_parsed > 0:
                line += f" - parsed {frame.elements_parsed} element{'s' if frame.elements_parsed != 1 else ''}"

            if frame.incomplete_element_line is not None and frame.incomplete_element_column is not None:
                incomplete_snippet = self.get_context_snippet(
                    frame.incomplete_element_line, frame.incomplete_element_column, length=20
                )
                line += f"\n     Started parsing element at line {frame.incomplete_element_line}, "
                line += f"column {frame.incomplete_element_column}: {incomplete_snippet}"
                line += "\n     → This element is incomplete"

            elif frame.last_complete_line:
                line += f"\n     → Needs ')' after line {frame.last_complete_line}, column {frame.last_complete_column}"

            stack_lines.append(line)

        stack_trace = "\n".join(stack_lines)

        # Build closing parens (always multiple since depth >= 2)
        closing_parens = " ) " * depth
        closing_parens = closing_parens.strip()

        paren_word = "parentheses"  # Always plural since depth >= 2

        context_msg = (
            f"Reached end of input while parsing let/letrec bindings.\n\n"
            f"Bindings parsed:\n{summary_text}\n\n"
            f"Unclosed expressions:\n{stack_trace}"
        )

        return AIFPLParseError(
            message=f"Incomplete let/letrec bindings - missing {depth} closing {paren_word}",
            line=bindings_start_line,
            column=bindings_start_col,
            expected=f'Add "{closing_parens}" to close all expressions',
            suggestion="Close each incomplete expression with ')', working from innermost to outermost",
            context=context_msg,
            example="(let (\n  (x 5)\n  (y (+ x 2))\n) body)",
            source=self.expression
        )

    def _parse_quoted_expression(self) -> AIFPLList:
        """
        Parse 'expr and convert to (quote expr) with detailed error reporting.

        Returns:
            AIFPLList representing (quote expr)

        Raises:
            AIFPLParseError: If quote is incomplete or malformed
        """
        quote_line = self.current_token.line if self.current_token else 1
        quote_col = self.current_token.column if self.current_token else 1
        self._advance()  # consume quote

        # Check if we have something to quote
        if self.current_token is None:
            raise AIFPLParseError(
                message="Incomplete quote expression",
                line=quote_line,
                column=quote_col,
                received="Quote symbol ' with nothing to quote",
                expected="Expression after quote symbol",
                example="Correct: '(a b c) or 'symbol\\nIncorrect: ' (nothing after)",
                suggestion="Add an expression after the ' symbol",
                context="Quote symbol must be followed by something to quote",
                source=self.expression
            )

        # Parse the expression to be quoted
        quoted_expr = self._parse_expression()

        # Transform 'expr into (quote expr)
        quote_symbol = AIFPLSymbol("quote")
        return AIFPLList((quote_symbol, quoted_expr))

    def _advance(self) -> None:
        """Move to the next token."""
        # Track where the current token ends before advancing
        assert self.current_token is not None, "Cannot advance when current token is None"
        # For simplicity, just track the end as the token's line/column + length
        # (This is approximate but good enough for error messages)
        self.last_token_end_line = self.current_token.line
        self.last_token_end_column = self.current_token.column + self.current_token.length

        self.pos += 1
        if self.pos < len(self.tokens):
            self.current_token = self.tokens[self.pos]

        else:
            self.current_token = None
