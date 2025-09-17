"""AIFPL (AI Functional Programming Language) calculator tool with LISP-like syntax."""

import asyncio
import logging
from typing import Any

from aifpl import AIFPL, AIFPLError
from ai_tool import (
    AITool, AIToolCall, AIToolDefinition, AIToolParameter, AIToolResult,
    AIToolExecutionError, AIToolTimeoutError, AIToolAuthorizationCallback
)


class AIFPLAITool(AITool):
    """AIFPL calculator tool with LISP-like syntax."""

    def __init__(self) -> None:
        """Initialize the AIFPL tool."""
        self._calculator = AIFPL()
        self._logger = logging.getLogger("AIFPLAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return AIToolDefinition(
            name="AIFPL",
            description=(
                "The AIFPL (AI Functional Programming Language) calculator uses LISP-like (S expression) syntax for "
                "mathematical expressions and string/boolean operations. "
                "Syntax: (operator arg1 arg2 ...)\n\n"
                "Supported operations:\n"
                "- Arithmetic: (+ 1 2 3), (- 10 3), (* 2 3 4), (/ 12 3), (// 7 3), (% 7 3), (** 2 3)\n"
                "- Trigonometry: (sin (* pi 0.5)), (cos 0), (tan (* pi 0.25))\n"
                "- Logarithms: (log e), (log10 100), (exp 1)\n"
                "- Other math: (sqrt 16), (abs -5), (round 3.7), (floor 3.7), (ceil 3.2)\n"
                "- Aggregation: (min 1 5 3), (max 1 5 3), (pow 2 3)\n"
                "- Bitwise: (bit-or 5 3), (bit-and 7 3), (bit-xor 5 3), (bit-not 5)\n"
                "- Bit shifts: (bit-shift-left 1 3), (bit-shift-right 8 2)\n"
                "- Base conversion: (bin 255), (hex 255), (oct 255)\n"
                "- Complex numbers: (complex 3 4), (+ 1 (* 2 j)), (real 3+4j), (imag 3+4j)\n"
                "- Comparison: (= 1 1), (< 1 2), (> 3 2), (<= 1 1), (>= 2 1)\n"
                "- Boolean operations: (and #t #f), (or #t #f), (not #t)\n"
                "- String operations: (string-append \"hello\" \" \" \"world\"), (string-length \"hello\")\n"
                "- String manipulation: (substring \"hello\" 1 4), (string-upcase \"hello\"), (string-downcase \"HELLO\")\n"
                "- String predicates: (string-contains? \"hello\" \"ell\"), (string-prefix? \"hello\" \"he\")\n"
                "- String conversion: (string->number \"42\"), (number->string 42)\n"
                "- Constants: pi, e, j, true, false\n"
                "- Literals: 42, 3.14, 0xFF, 0b1010, 0o755, \"hello\", #t, #f\n\n"
                "String literals support escape sequences:\n"
                "- \\\" (quote), \\\\ (backslash), \\n (newline), \\t (tab), \\r (carriage return)\n"
                "- \\uXXXX (Unicode code point with 4 hex digits)\n\n"
                "Important:\n"
                "- All operators use prefix notation: (+ 1 2) not (1 + 2)\n"
                "- Whitespace is required between all tokens\n"
                "- Arithmetic operators are variadic where sensible: (+ 1 2 3 4) = 10\n"
                "- String operations work only on strings, boolean operations only on booleans\n"
                "- Comparison operators work on numbers and return #t or #f\n"
                "- Results are simplified to real numbers when imaginary part is negligible\n"
                "- Bitwise operations only work on integers\n"
                "- Use explicit conversion functions: (string->number \"42\"), (number->string 42)\n"
                "- String indexing uses LISP convention: (substring \"hello\" 1 4) → \"ell\" (start=1, end=4 exclusive)\n"
                "- Do not use it for anything other than mathematical calculations and basic string/boolean operations\n"
            ),
            parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="A valid AIFPL expression using LISP-like S-expression syntax",
                    required=True
                )
            ]
        )

    def _evaluate_expression(self, expression: str) -> str:
        """
        Synchronous helper for expression evaluation.

        Args:
            expression: AIFPL expression to evaluate

        Returns:
            String representation of the result

        Raises:
            Various AIFPL-related exceptions
        """
        result = self._calculator.evaluate(expression)

        # Format boolean results in LISP style
        if isinstance(result, bool):
            return "#t" if result else "#f"

        return str(result)

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the AIFPL tool with timeout protection.

        Args:
            tool_call: Tool call containing the expression to evaluate
            requester_ref: Reference to the requester
            request_authorization: Function to call if we need to request authorization

        Returns:
            AIToolResult containing the calculation result

        Raises:
            AIToolExecutionError: If calculation fails or expression is invalid
            AIToolTimeoutError: If calculation takes too long
        """
        arguments = tool_call.arguments
        expression = arguments.get("expression", "")

        # Validate expression is provided
        if not expression:
            self._logger.error("AIFPL tool called without expression argument")
            raise AIToolExecutionError("Expression is required")

        # Validate expression is a string
        if not isinstance(expression, str):
            self._logger.error("AIFPL tool called with non-string expression: %s", type(expression).__name__)
            raise AIToolExecutionError("Expression must be a string")

        try:
            self._logger.debug("Evaluating AIFPL expression: %s", expression)

            # Run calculation with timeout protection
            try:
                result = await asyncio.wait_for(
                    asyncio.to_thread(self._evaluate_expression, expression),
                    timeout=5.0  # 5 seconds should be plenty for mathematical calculations
                )
            except asyncio.TimeoutError as e:
                self._logger.warning("AIFPL expression evaluation timed out: %s", expression)
                raise AIToolTimeoutError("AIFPL calculation timed out", 5.0) from e

            self._logger.debug("AIFPL evaluation successful: %s = %s", expression, result)

            return AIToolResult(
                id=tool_call.id,
                name="AIFPL",
                content=result
            )

        except AIToolTimeoutError:
            # Re-raise timeout errors
            raise

        except AIFPLError as e:
            self._logger.warning("AIFPL error in expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(str(e)) from e

        except ZeroDivisionError as e:
            self._logger.warning("Division by zero in AIFPL expression '%s'", expression, exc_info=True)
            raise AIToolExecutionError("Division by zero") from e

        except Exception as e:
            self._logger.error("Unexpected error evaluating AIFPL expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to evaluate AIFPL expression: {str(e)}") from e
