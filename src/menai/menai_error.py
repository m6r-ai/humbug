"""Enhanced exception classes for Menai (AI Functional Programming Language) with detailed context."""

from typing import Any


class MenaiError(Exception):
    """Base exception for Menai errors with detailed context information."""

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


class MenaiTokenError(MenaiError):
    """Tokenization errors with detailed context."""


class MenaiParseError(MenaiError):
    """Parsing errors with detailed context."""


class MenaiEvalError(MenaiError):
    """Evaluation errors with detailed context."""


class MenaiModuleError(MenaiError):
    """Module system errors with detailed context."""


class MenaiModuleNotFoundError(MenaiModuleError):
    """Module file not found in search path."""

    def __init__(
        self,
        module_name: str,
        search_paths: list[str],
        **kwargs: Any
    ) -> None:
        """
        Initialize module not found error.

        Args:
            module_name: Name of module that wasn't found
            search_paths: List of paths that were searched
            **kwargs: Additional error context
        """
        searched = "\n    - ".join(f"{path}/{module_name}.menai" for path in search_paths)

        super().__init__(
            message=f"Module '{module_name}' not found",
            context=f"Searched in:\n    - {searched}",
            suggestion="Check module name spelling or add directory to module_path",
            **kwargs
        )


class MenaiCircularImportError(MenaiModuleError):
    """Circular dependency detected in module imports."""

    def __init__(
        self,
        import_chain: list[str],
        **kwargs: Any
    ):
        """
        Initialize circular import error.

        Args:
            import_chain: List of module names showing the circular dependency
            **kwargs: Additional error context
        """
        chain_str = " -> ".join(import_chain)

        super().__init__(
            message="Circular import detected",
            context=f"Import chain:\n    {chain_str}",
            suggestion="Break the cycle by extracting shared code to a third module",
            **kwargs
        )


class MenaiCancelledException(MenaiEvalError):
    """Execution was cancelled (typically due to timeout)."""

    def __init__(self, **kwargs: Any):
        """
        Initialize cancellation error.

        Args:
            **kwargs: Additional error context
        """
        super().__init__(
            message="Execution was cancelled",
            suggestion="The operation exceeded the allowed time limit or was explicitly cancelled",
            **kwargs
        )
