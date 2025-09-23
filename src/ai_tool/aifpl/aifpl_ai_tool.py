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
                "The AIFPL (AI Functional Programming Language) tool offers a LISP-like (S expression) syntax for "
                "mathematical expressions, string/boolean operations, list manipulation, conditional evaluation, "
                "and functional programming with lambda expressions and iteration. "
                "It is ideal for everything from simple calculations to complex algorithms. "
                "The language is a pure functional langage with no side effects so it does not require user approvals "
                "to use it (unlike other tools). "
                "When annotating AIFPL code, use triple backticks with `aifpl`\n\n"

                "Syntax: (operator arg1 arg2 ...)\n\n"

                "Quote - Data Literals and Code as Data:\n"
                "- (quote expr) → returns expr without evaluation\n"
                "- 'expr → shortcut for (quote expr)\n"
                "- '(+ 1 2 3) → (+ 1 2 3) (as data, not evaluated)\n"
                "- (list 'hello (+ 1 2) 'world) → (hello 3 world)\n"
                "- Enables symbolic programming: (first '(+ 1 2)) → +\n"
                "- Code and data have identical representation (homoiconicity)\n\n"

                "Arithmetic and Math:\n"
                "- Basic: (+ 1 2 3), (- 10 3), (* 2 3 4), (/ 12 3), (// 7 3), (% 7 3), (** 2 3)\n"
                "- Trig: (sin (* pi 0.5)), (cos 0), (tan (* pi 0.25))\n"
                "- Logs: (log e), (log10 100), (exp 1)\n"
                "- Other: (sqrt 16), (abs -5), (round 3.7), (floor 3.7), (ceil 3.2)\n"
                "- Aggregation: (min 1 5 3), (max 1 5 3), (pow 2 3)\n"
                "- Bitwise: (bit-or 5 3), (bit-and 7 3), (bit-xor 5 3), (bit-not 5)\n"
                "- Bit shifts: (bit-shift-left 1 3), (bit-shift-right 8 2)\n"
                "- Base conversion: (bin 255), (hex 255), (oct 255)\n\n"

                "Complex Numbers:\n"
                "- (complex 3 4) → (3+4j) (construct complex number)\n"
                "- (+ 1 (* 2 j)), constants: pi, e, j\n"
                "- (real 3+4j) → 3, (imag 3+4j) → 4\n"
                "- (real 42) → 42, (imag 42) → 0 (works on all numbers)\n\n"

                "Comparison and Boolean:\n"
                "- (= 1 1), (!= 1 2), (< 1 2), (> 3 2), (<= 1 1), (>= 2 1)\n"
                "- (and #t #f), (or #t #f), (not #t)\n"
                "- (if (> 5 3) \"yes\" \"no\"), lazy evaluation: (if #t 42 (/ 1 0))\n\n"

                "String Operations:\n"
                "- Basic: (string-append \"hello\" \" \" \"world\"), (string-length \"hello\")\n"
                "- Access: (string-ref \"hello\" 1) → \"e\" (character at 0-based index)\n"
                "- Manipulation: (substring \"hello\" 1 4), (string-upcase \"hello\"), (string-downcase \"HELLO\")\n"
                "- Utilities: (string-trim \"  hello  \"), (string-replace \"banana\" \"a\" \"o\")\n"
                "- Predicates: (string-contains? \"hello\" \"ell\"), (string-prefix? \"hello\" \"he\"), (string-suffix? \"hello\" \"lo\"), (string=? \"hi\" \"hi\")\n"
                "- Conversion: (string->number \"42\"), (number->string 42)\n"
                "- String-list: (string->list \"hi\") → (\"h\" \"i\"), (list->string (list \"h\" \"i\")) → \"hi\"\n"
                "- Split/join: (string-split \"a,b,c\" \",\") → (\"a\" \"b\" \"c\"), (string-join (list \"hello\" \"world\") \" \") → \"hello world\"\n\n"

                "List Operations:\n"
                "- Construction: (list 1 2 3), (cons 1 (list 2 3)), (append (list 1 2) (list 3 4))\n"
                "- Access: (first (list 1 2 3)), (rest (list 1 2 3)), (last (list 1 2 3))\n"
                "- Indexed access: (list-ref (list \"a\" \"b\" \"c\") 1) → \"b\" (0-based index)\n"
                "- Properties: (length (list 1 2 3)), (null? (list)), (member? 2 (list 1 2 3))\n"
                "- Utilities: (reverse (list 1 2 3)), (remove 2 (list 1 2 3 2 4)), (position 2 (list 1 2 3)) → 1 or #f\n"
                "- Slicing: (take 3 (list 1 2 3 4 5)), (drop 2 (list 1 2 3 4 5))\n\n"

                "Type Predicates:\n"
                "- (number? 42) → #t, excludes booleans: (number? #t) → #f\n"
                "- (integer? 42) → #t, (float? 3.14) → #t, (complex? (+ 1 j)) → #t\n"
                "- (string? \"hello\") → #t, (boolean? #t) → #t, (list? (list 1 2)) → #t\n"
                "- (function? (lambda (x) x)) → #t\n\n"

                "Lambda Functions:\n"
                "- (lambda (param1 param2 ...) body) → creates anonymous function\n"
                "- ((lambda (x) (* x x)) 5) → 25\n"
                "- Functions are first-class values with lexical scoping and closures\n"
                "- Tail recursion automatically optimized\n\n"

                "Local Bindings:\n"
                "- (let ((var1 val1) (var2 val2) ...) body) → sequential binding\n"
                "- (let ((x 5) (y (* x 2))) (+ x y)) → 15 (y can reference x)\n"
                "- Automatic dependency analysis for recursive bindings\n\n"

                "Higher-Order Functions:\n"
                "- (map func list) → (map (lambda (x) (* x 2)) (list 1 2 3)) → (2 4 6)\n"
                "- (filter predicate list) → (filter (lambda (x) (> x 0)) (list -1 2 -3 4)) → (2 4)\n"
                "- (fold func init list) → (fold + 0 (list 1 2 3 4)) → 10\n"
                "- (range start end [step]) → (range 1 5) → (1 2 3 4)\n"
                "- (find predicate list), (any? predicate list), (all? predicate list)\n\n"

                "Key Features:\n"
                "- Pure functional: no side effects, immutable data\n"
                "- Homoiconic: code and data use same representation\n"
                "- Tail call optimization prevents stack overflow\n"
                "- Strict type system with automatic numeric promotion\n"
                "- Mixed-type lists supported: (list 1 \"hi\" #t)\n"
                "- String literals support escapes: \\n, \\t, \\\", \\\\, \\uXXXX\n"
                "- Literals: 42, 3.14, 0xFF, 0b1010, \"hello\", #t, #f, ()\n"
                "- Constants: pi, e, j, true, false\n\n"

                "Important Notes:\n"
                "- Prefix notation: (+ 1 2) not (1 + 2)\n"
                "- Strict typing: string ops need strings, boolean ops need booleans\n"
                "- Lists only support = and != comparisons, not < > <= >=\n"
                "- Conditions must be boolean: (if #t ...) works, (if 1 ...) doesn't\n"
                "- Use for calculations, data processing, and functional programming only\n"
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
                    timeout=10.0  # Increased timeout for complex functional programming
                )
            except asyncio.TimeoutError as e:
                self._logger.warning("AIFPL expression evaluation timed out: %s", expression)
                raise AIToolTimeoutError("AIFPL calculation timed out", 10.0) from e

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
            # Check if this is a division by zero error specifically
            error_msg = str(e).lower()
            if "division by zero" in error_msg:
                raise AIToolExecutionError("Division by zero") from e
            raise AIToolExecutionError(str(e)) from e

        except ZeroDivisionError as e:
            # Handle the unlikely case where Python's ZeroDivisionError still gets through
            self._logger.warning("Division by zero in AIFPL expression '%s'", expression, exc_info=True)
            raise AIToolExecutionError("Division by zero") from e

        except Exception as e:
            self._logger.error("Unexpected error evaluating AIFPL expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to evaluate AIFPL expression: {str(e)}") from e
