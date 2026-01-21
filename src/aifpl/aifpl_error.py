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
        position: int | None = None
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
            position: Character position where error occurred
        """
        self.message = message
        self.context = context
        self.expected = expected
        self.received = received
        self.suggestion = suggestion
        self.example = example
        self.position = position

        super().__init__(self._format_detailed_message())

    def _format_detailed_message(self) -> str:
        """Format the error message with all available details."""
        parts = [f"Error: {self.message}"]

        # Add position information if available
        if self.position is not None:
            parts.append(f"Position: {self.position}")

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
