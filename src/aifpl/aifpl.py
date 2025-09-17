"""Main AIFPL (AI Functional Programming Language) class."""

from typing import Union

from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer


class AIFPL:
    """
    AIFPL (AI Functional Programming Language) calculator with LISP-like syntax.

    This class provides a high-level interface for evaluating AIFPL expressions.
    It coordinates tokenization, parsing, and evaluation phases.
    """

    def __init__(self, max_depth: int = 100, imaginary_tolerance: float = 1e-10):
        """
        Initialize AIFPL calculator.

        Args:
            max_depth: Maximum recursion depth for expression evaluation
            imaginary_tolerance: Tolerance for considering imaginary part as zero
        """
        self.max_depth = max_depth
        self.imaginary_tolerance = imaginary_tolerance

    def evaluate(self, expression: str) -> Union[int, float, complex, str]:
        """
        Evaluate an AIFPL expression.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression

        Raises:
            AIFPLTokenError: If tokenization fails
            AIFPLParseError: If parsing fails
            AIFPLEvalError: If evaluation fails
        """
        # Phase 1: Tokenization
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize(expression)

        # Phase 2: Parsing
        parser = AIFPLParser(tokens)
        parsed = parser.parse()

        # Phase 3: Evaluation
        evaluator = AIFPLEvaluator(
            max_depth=self.max_depth,
            imaginary_tolerance=self.imaginary_tolerance
        )
        result = evaluator.evaluate(parsed.expr)

        # Simplify the result
        return evaluator.simplify_result(result)
