"""Main AIFPL (AI Functional Programming Language) class."""

from typing import Union

from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_value import AIFPLValue, AIFPLFunction


class AIFPL:
    """
    AIFPL (AI Functional Programming Language) calculator with LISP-like syntax.

    This class provides a high-level interface for evaluating AIFPL expressions.
    It coordinates tokenization, parsing, and evaluation phases.
    """

    def __init__(self, max_depth: int = 100, floating_point_tolerance: float = 1e-10):
        """
        Initialize AIFPL calculator.

        Args:
            max_depth: Maximum recursion depth for expression evaluation
            floating_point_tolerance: Tolerance for floating point comparisons and simplifications
        """
        self.max_depth = max_depth
        self.floating_point_tolerance = floating_point_tolerance

    def evaluate(self, expression: str) -> Union[int, float, complex, str, bool, list, AIFPLFunction]:
        """
        Evaluate an AIFPL expression and return Python-compatible result.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression converted to Python types

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
        parsed_expr = parser.parse()

        # Phase 3: Evaluation
        evaluator = AIFPLEvaluator(
            max_depth=self.max_depth,
            floating_point_tolerance=self.floating_point_tolerance
        )
        result = evaluator.evaluate(parsed_expr)

        # Simplify the result
        simplified = evaluator.simplify_result(result)

        # Convert to Python types for backward compatibility
        return self._aifpl_value_to_python(simplified)

    def evaluate_and_format(self, expression: str) -> str:
        """
        Evaluate an AIFPL expression and return formatted result.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            String representation of the result using LISP conventions

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
        parsed_expr = parser.parse()

        # Phase 3: Evaluation
        evaluator = AIFPLEvaluator(
            max_depth=self.max_depth,
            floating_point_tolerance=self.floating_point_tolerance
        )
        result = evaluator.evaluate(parsed_expr)

        # Simplify and format the result
        simplified = evaluator.simplify_result(result)
        return evaluator.format_result(simplified)

    def _aifpl_value_to_python(self, value: AIFPLValue) -> Union[int, float, complex, str, bool, list, AIFPLFunction]:
        """
        Convert AIFPLValue to Python types for backward compatibility.

        Args:
            value: AIFPLValue to convert

        Returns:
            Python equivalent of the value
        """
        if isinstance(value, AIFPLFunction):
            # Functions are returned as-is for backward compatibility
            return value

        # Use the value's to_python method
        return value.to_python()
