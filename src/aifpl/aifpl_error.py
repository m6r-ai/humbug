"""Enhanced exception classes for AIFPL (AI Functional Programming Language) with detailed context."""

import difflib


class AIFPLError(Exception):
    """Base exception for AIFPL errors with detailed context information."""

    def __init__(
        self,
        message: str,
        context: str | None = None,
        expected: str | None = None,
        received: str | None = None,
        suggestion: str | None = None,
        example: str | None = None,
        line: int | None = None,
        column: int | None = None,
        source: str | None = None,
        show_context: bool = True
    ):
        """
        Initialize detailed error.

        Args:
            message: Core error description
            context: Additional context information
            expected: What was expected
            received: What was actually received
            suggestion: Suggestion for fixing the error
            example: Example of correct usage
            line: Line number (1-indexed)
            column: Column number (1-indexed)
            source: Source code for context display
            show_context: Whether to show source code context
        """
        self.message = message
        self.context = context
        self.expected = expected
        self.received = received
        self.suggestion = suggestion
        self.example = example
        self.line = line
        self.column = column
        self.source = source
        self.show_context = show_context

        super().__init__(self._format_detailed_message())

    def _get_context_lines(
        self,
        source: str,
        line_num: int,
        before: int = 2,
        after: int = 2
    ) -> list[tuple[int, str]]:
        """
        Get lines of context around a specific line.

        Args:
            source: The source code string
            line_num: Line number (1-indexed)
            before: Number of lines before to include
            after: Number of lines after to include

        Returns:
            List of (line_number, line_content) tuples
        """
        lines = source.split('\n')
        total_lines = len(lines)

        # Calculate range
        start_line = max(1, line_num - before)
        end_line = min(total_lines, line_num + after)

        result = []
        for i in range(start_line, end_line + 1):
            if 1 <= i <= total_lines:
                result.append((i, lines[i - 1]))

        return result

    def _format_context_with_marker(
        self,
        source: str,
        line_num: int,
        column: int | None = None,
        before: int = 2,
        after: int = 2,
        marker: str = "^"
    ) -> str:
        """
        Format source context with a marker pointing to the error location.

        Args:
            source: The source code string
            line_num: Line number (1-indexed)
            column: Column number (1-indexed), optional
            before: Number of lines before to include
            after: Number of lines after to include
            marker: Character to use for marking the position

        Returns:
            Formatted string with context and marker
        """
        context_lines = self._get_context_lines(source, line_num, before, after)

        if not context_lines:
            return "(no context available)"

        # Calculate max line number width for alignment
        max_line_num = max(ln for ln, _ in context_lines)
        line_num_width = len(str(max_line_num))

        result_lines = []
        for ln, content in context_lines:
            # Mark the error line with an indicator
            indicator = "→" if ln == line_num else " "
            line_str = f"  {indicator} {ln:>{line_num_width}}: {content}"
            result_lines.append(line_str)

            # Add marker line if this is the error line and column is specified
            if ln == line_num and column is not None:
                # Calculate padding: "  " + indicator + " " + line_num + ": " + (column - 1)
                # The column is 1-indexed, so column 1 is the first char (needs 0 extra spaces)
                padding = 2 + 1 + 1 + line_num_width + 2 + (column - 1)
                marker_line = " " * padding + marker
                result_lines.append(marker_line)

        return "\n".join(result_lines)

    def _format_detailed_message(self) -> str:
        """Format the error message with all available details."""
        parts = [f"Error: {self.message}"]

        # Add position information if available
        if self.line is not None and self.column is not None:
            parts.append(f"Location: Line {self.line}, Column {self.column}")

        # Add source code context if available
        if self.show_context and self.source is not None:
            if self.line is not None and self.column is not None:
                context_str = self._format_context_with_marker(
                    self.source, self.line, self.column, before=2, after=1
                )
                parts.append(f"\nSource Context:\n{context_str}")

        # Add received/expected information
        if self.received:
            parts.append(f"Received: {self.received}")

        if self.expected:
            parts.append(f"Expected: {self.expected}")

        # Add context
        if self.context:
            parts.append(f"Context: {self.context}")

        # Add suggestion
        if self.suggestion:
            parts.append(f"Suggestion: {self.suggestion}")

        # Add example
        if self.example:
            parts.append(f"Example: {self.example}")

        return "\n".join(parts)


class AIFPLTokenError(AIFPLError):
    """Tokenization errors with detailed context."""


class AIFPLParseError(AIFPLError):
    """Parsing errors with detailed context."""


class AIFPLEvalError(AIFPLError):
    """Evaluation errors with detailed context."""


class ErrorMessageBuilder:
    """Helper class for building detailed error messages."""

    @staticmethod
    def suggest_similar_functions(target: str, available_functions: list[str], max_suggestions: int = 3) -> list[str]:
        """Suggest similar function names using fuzzy matching."""
        matches = difflib.get_close_matches(target, available_functions, n=max_suggestions, cutoff=0.6)
        return matches

    @staticmethod
    def create_function_example(func_name: str) -> str:
        """Create usage example for common functions."""
        examples = {
            # Arithmetic
            '+': "(+ 1 2 3) → 6",
            '-': "(- 10 3) → 7",
            '*': "(* 2 3 4) → 24",
            '/': "(/ 12 3) → 4",
            '//': "(// 7 3) → 2",
            '%': "(% 7 3) → 1",
            '**': "(** 2 3) → 8",

            # Comparison
            '=': "(= 1 1 1) → #t",
            '!=': "(!= 1 2) → #t",
            '<': "(< 1 2 3) → #t",
            '>': "(> 3 2 1) → #t",
            '<=': "(<= 1 1 2) → #t",
            '>=': "(>= 3 2 2) → #t",

            # Boolean
            'and': "(and #t #t #f) → #f",
            'or': "(or #f #t) → #t",
            'not': "(not #t) → #f",
            'if': "(if (> 5 3) \"yes\" \"no\") → \"yes\"",

            # Lists
            'list': "(list 1 2 3) → (1 2 3)",
            'cons': "(cons 1 (list 2 3)) → (1 2 3)",
            'first': "(first (list 1 2 3)) → 1",
            'rest': "(rest (list 1 2 3)) → (2 3)",
            'length': "(length (list 1 2 3)) → 3",
            'append': "(append (list 1 2) (list 3 4)) → (1 2 3 4)",

            # Strings
            'string-append': "(string-append \"hello\" \" \" \"world\") → \"hello world\"",
            'string-length': "(string-length \"hello\") → 5",
            'substring': "(substring \"hello\" 1 4) → \"ell\"",

            # Higher-order
            'map': "(map (lambda (x) (* x 2)) (list 1 2 3)) → (2 4 6)",
            'filter': "(filter (lambda (x) (> x 0)) (list -1 2 -3 4)) → (2 4)",
            'fold': "(fold + 0 (list 1 2 3 4)) → 10",

            # Math
            'sin': "(sin (* pi 0.5)) → 1.0",
            'sqrt': "(sqrt 16) → 4.0",
            'abs': "(abs -5) → 5",
            'round': "(round 3.7) → 4",

            # Let and Lambda
            'let': "(let ((x 5) (y 10)) (+ x y)) → 15",
            'lambda': "((lambda (x) (* x x)) 5) → 25"
        }

        return examples.get(func_name, f"({func_name} ...)")
