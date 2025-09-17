"""AIFPL (AI Functional Programming Language) tool with LISP-like syntax."""

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
        self._tool = AIFPL()
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
                "mathematical expressions, string/boolean operations, list manipulation, and conditional evaluation. "
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
                "- Conditionals: (if (> 5 3) \"yes\" \"no\"), (if #t 42 (/ 1 0))\n"
                "- String operations: (string-append \"hello\" \" \" \"world\"), (string-length \"hello\")\n"
                "- String manipulation: (substring \"hello\" 1 4), (string-upcase \"hello\"), (string-downcase \"HELLO\")\n"
                "- String predicates: (string-contains? \"hello\" \"ell\"), (string-prefix? \"hello\" \"he\")\n"
                "- String conversion: (string->number \"42\"), (number->string 42)\n\n"
                "Conditional Operations:\n"
                "- (if condition then-expr else-expr) → evaluates then-expr if condition is #t, else-expr if #f\n"
                "- (if (> 5 3) \"greater\" \"less\") → \"greater\"\n"
                "- (if (= 1 2) (+ 1 1) (* 2 2)) → 4\n"
                "- (if #t 42 (/ 1 0)) → 42 (division by zero not evaluated)\n"
                "- (if (null? my-list) \"empty\" (first my-list)) → safe list access\n"
                "- (if (member? \"x\" data) \"found\" \"not found\") → conditional based on list membership\n\n"
                "List Operations:\n"
                "- Construction: (list 1 2 3) → (1 2 3), (list \"a\" \"b\") → (\"a\" \"b\"), (list) → ()\n"
                "- Access: (first (list 1 2 3)) → 1, (rest (list 1 2 3)) → (2 3)\n"
                "- Properties: (length (list 1 2 3)) → 3, (null? (list)) → #t\n"
                "- Manipulation: (cons 1 (list 2 3)) → (1 2 3), (append (list 1 2) (list 3 4)) → (1 2 3 4)\n"
                "- Search: (member? 2 (list 1 2 3)) → #t, (member? 5 (list 1 2 3)) → #f\n"
                "- Utilities: (reverse (list 1 2 3)) → (3 2 1), (list-ref (list \"a\" \"b\" \"c\") 1) → \"b\"\n\n"
                "String-List Integration:\n"
                "- (string->list \"hello\") → (\"h\" \"e\" \"l\" \"l\" \"o\")\n"
                "- (list->string (list \"h\" \"i\")) → \"hi\"\n"
                "- (string-split \"a,b,c\" \",\") → (\"a\" \"b\" \"c\")\n"
                "- (string-join (list \"hello\" \"world\") \" \") → \"hello world\"\n\n"
                "List Type Rules:\n"
                "- Lists support mixed types: (list 1 \"hi\" #t) → (1 \"hi\" #t)\n"
                "- Lists don't work with arithmetic: (+ (list 1 2)) → ERROR\n"
                "- Lists don't work with comparisons except equality: (< (list 1) (list 2)) → ERROR\n"
                "- Only equality works: (= (list 1 2) (list 1 2)) → #t\n"
                "- List functions require list arguments: (first \"hello\") → ERROR\n"
                "- String functions require string arguments: (string-length (list \"hi\")) → ERROR\n\n"
                "Conditional Usage Patterns:\n"
                "- Safe operations: (if (> x 0) (/ 100 x) \"undefined\")\n"
                "- List processing: (if (null? items) \"no items\" (string-join items \", \"))\n"
                "- Data validation: (if (string-contains? input \"@\") \"email\" \"not email\")\n"
                "- Nested conditions: (if (> x 0) (if (> x 10) \"big\" \"small\") \"negative\")\n"
                "- Error avoidance: (if #f (undefined-operation) \"safe result\")\n\n"
                "Common Patterns:\n"
                "- Process CSV: (string-split \"name,age,city\" \",\") then manipulate list\n"
                "- Build strings: create list of parts, then (string-join parts \" \")\n"
                "- Character processing: (string->list text) for char-by-char operations\n"
                "- Conditional processing: (if condition safe-operation fallback-value)\n\n"
                "Constants: pi, e, j, true, false\n"
                "- Literals: 42, 3.14, 0xFF, 0b1010, 0o755, \"hello\", #t, #f\n\n"
                "String literals support escape sequences:\n"
                "- \\\" (quote), \\\\ (backslash), \\n (newline), \\t (tab), \\r (carriage return)\n"
                "- \\uXXXX (Unicode code point with 4 hex digits)\n\n"
                "Important:\n"
                "- All operators use prefix notation: (+ 1 2) not (1 + 2)\n"
                "- Whitespace is required between all tokens\n"
                "- Arithmetic operators are variadic where sensible: (+ 1 2 3 4) = 10\n"
                "- String operations work only on strings, boolean operations only on booleans\n"
                "- List operations work only on lists (except list? predicate)\n"
                "- Comparison operators work on numbers, lists only support equality\n"
                "- Results are simplified to real numbers when imaginary part is negligible\n"
                "- Bitwise operations only work on integers\n"
                "- Lists display as LISP notation: (1 2 3) not [1,2,3]\n"
                "- Booleans display as: #t or #f\n"
                "- Empty list displays as: ()\n"
                "- Use explicit conversion functions: (string->number \"42\"), (number->string 42)\n"
                "- String indexing uses LISP convention: (substring \"hello\" 1 4) → \"ell\" (start=1, end=4 exclusive)\n"
                "- List indexing is 0-based: (list-ref (list \"a\" \"b\" \"c\") 1) → \"b\"\n"
                "- Conditionals use lazy evaluation: unused branches are not evaluated\n"
                "- Conditions must be boolean: (if #t ...) works, (if 1 ...) does not\n"
                "- Do not use it for anything other than mathematical calculations, basic string/boolean operations, "
                "list manipulation, and conditional evaluation\n"
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
        # Use the new evaluate_and_format method for proper LISP formatting
        return self._tool.evaluate_and_format(expression)

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
