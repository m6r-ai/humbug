"""AIFPL (AI Functional Programming Language) tool with LISP-like syntax."""

import asyncio
import json
import logging
from pathlib import Path
from typing import Any, Dict, List

from aifpl import AIFPL, AIFPLError, AIFPLCancelledException
from aifpl import AIFPLBufferingTraceWatcher
from ai_tool import (
    AITool, AIToolCall, AIToolDefinition, AIToolParameter, AIToolResult,
    AIToolExecutionError, AIToolTimeoutError, AIToolAuthorizationCallback,
    AIToolOperationDefinition
)


class AIFPLAITool(AITool):
    """AIFPL calculator tool with LISP-like syntax."""

    def __init__(self) -> None:
        """
        Initialize the AIFPL tool.
        """
        self._tool = AIFPL()
        self._logger = logging.getLogger("AIFPLAITool")
        self._module_path: List[str] = []

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return self._build_definition_from_operations(
            name="AIFPL",
            description_prefix=(
                "The AIFPL (AI Functional Programming Language) is a pure functional programming language. "
                "It is ideal for everything from simple calculations to complex algorithms. "
                "AIFPL has no side effects, so it does not require user approvals to use it. "
                "When annotating AIFPL code, use triple backticks with `aifpl`\n\n"
            ),
            additional_parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="A valid expression written in the AIFPL language",
                    required=True
                )
            ]
        )

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "evaluate": AIToolOperationDefinition(
                name="evaluate",
                handler=self._evaluate,
                extract_context=self._extract_evaluate_context,
                allowed_parameters={"expression"},
                required_parameters={"expression"},
                description="Evaluate an expression written in the AIFPL language"
            )
        }

    def set_module_path(self, module_path: list[str]) -> None:
        """
        Update the module search path.

        This should be called when the base directory changes (e.g., when switching
        mindspaces in Humbug) to ensure modules are loaded from the correct location.
        The module cache will be automatically cleared.

        Args:
            module_path: List of directories to search for modules.
                        Paths will be expanded and resolved.
        """
        # Expand path
        expanded_path = []
        for path in module_path:
            expanded = str(Path(path).expanduser().resolve())
            expanded_path.append(expanded)

        # Update the underlying AIFPL instance (this also clears the cache)
        self._tool.set_module_path(expanded_path)

        # Update our own reference
        self._module_path = module_path

    def module_path(self) -> List[str]:
        """
        Get the current module search path.

        Returns:
            List of directories in the module search path
        """
        return self._module_path

    def cancel(self) -> None:
        """
        Cancel any ongoing AIFPL evaluation.

        This signals the VM to stop execution at the next cancellation check point
        (typically within 1ms for CPU-intensive computations).

        This method is thread-safe and can be called while an evaluation is running
        in a thread pool.
        """
        self._tool.vm.cancel()

    def _evaluate_expression_sync(self, expression: str) -> tuple[str, List[str], bool]:
        """
        Synchronous helper for expression evaluation.

        Sets up trace collection and returns both result and traces.

        Args:
            expression: AIFPL expression to evaluate

        Returns:
            Tuple of (result_string, traces_list, was_clipped)

        Raises:
            Various AIFPL-related exceptions
        """
        # Set up trace watcher to collect any trace output
        watcher = AIFPLBufferingTraceWatcher(max_traces=200)
        self._tool.set_trace_watcher(watcher)

        try:
            result = self._tool.evaluate_and_format(expression)
            traces = watcher.get_traces()
            was_clipped = watcher.is_clipped()
            return result, traces, was_clipped

        finally:
            # Clean up watcher
            self._tool.set_trace_watcher(None)

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Evaluates expressions using a very efficient pure functional programming language"

    def get_detailed_help(self, operation: str | None = None) -> str:
        """
        Get detailed AIFPL documentation.

        For AIFPL, we provide comprehensive syntax documentation since
        the language has its own syntax that needs to be learned.
        """
        if operation is not None:
            # For specific operation, use default implementation
            return self._get_operation_help(operation)

        # Full tool help with comprehensive syntax guide
        return """# AIFPL Tool
Syntax: (operator arg1 arg2 ...)

## Key features:

- Pure functional: no side effects, immutable data
- Homoiconic: code and data use same representation (s-expressions)
- Tail call optimization prevents stack overflow
- Strict type system with automatic numeric promotion (integer → float → complex)
- Numeric promotion is one-way: types promote up but never downgrade automatically
- Mixed-type lists supported: (list 1 \"hi\" #t)
- String literals support escapes: \\n, \\t, \\\", \\\\, \\uXXXX
- Comments: use semicolon (;) for single-line comments, e.g., ; This is a comment
- Numeric literals: 42 (integer), 3.14 (float), 3+4j (complex), 5j (pure imaginary), j (1j)
- Other literals: #xFF (hex), #b1010 (binary), #o755 (octal), \"hello\" (string), #t/#f (boolean), () (empty list)
- Constants: pi, e

## Quote - data literals and code as data:

- (quote expr) → returns expr without evaluation
- 'expr → shortcut for (quote expr)
- '(+ 1 2 3) → (+ 1 2 3) (as data, not evaluated)
- (list 'hello (+ 1 2) 'world) → (hello 3 world)
- Enables symbolic programming: (first '(+ 1 2)) → +
- Code and data have identical representation (homoiconicity)

## Arithmetic and math:

- Basic: (+ 1 2 3), (- 10 3), (* 2 3 4), (/ 12 3), (// 7 3), (% 7 3), (** 2 3)
- Division (/) always returns float: (/ 10 2) → 5.0, (/ 10 3) → 3.333...; use (//) for integer division
- Trig: (sin (* pi 0.5)), (cos 0), (tan (* pi 0.25))
- Logs: (log e), (log10 100), (exp 1)
- Other: (sqrt 16), (abs -5)
- Round to integer: (round 3.7) → 4, (floor 3.7) → 3, (ceil 3.2) → 4
- Aggregation: (min 1 5 3), (max 1 5 3), (pow 2 3)
- Bitwise: (bit-or 5 3), (bit-and 7 3), (bit-xor 5 3), (bit-not 5)
- Bit shifts: (bit-shift-left 1 3), (bit-shift-right 8 2)
- Base conversion: (bin 255), (hex 255), (oct 255)

## Complex numbers:

- Literals: 3+4j, 5j, 1j
- Pure imaginary: 4j, -5j, 1.5e2j → 4j, -5j, 150j
- Complex: 3+4j, 3-4j, 1e2+3e-1j → (3+4j), (3-4j), (100+0.3j)
- (complex 3 4) → (3+4j) (construct complex number)
- Use in expressions: (+ 1 2+3j) → (3+3j), (* 2 3+4j) → (6+8j)
- (real 3+4j) → 3, (imag 3+4j) → 4, (abs 3+4j) → 5.0
- (real 42) → 42, (imag 42) → 0 (works on all numbers)

## Type construction and conversion:

- (integer x) → convert to integer (truncates toward zero): (integer 3.7) → 3, (integer -2.9) → -2
- (float x) → convert to float: (float 42) → 42.0, (float 3) → 3.0
- (complex real imag) → construct complex: (complex 3 4) → (3+4j)
- Use for explicit type control and conversions between numeric types

## Comparison and boolean:

- (= 1 1), (!= 1 2), (< 1 2), (> 3 2), (<= 1 1), (>= 2 1)
- (and #t #f), (or #t #f), (not #t)
- (if (> 5 3) "yes" "no"), lazy evaluation: (if #t 42 (/ 1 0))

## String operations:

- Basic: (string-append "hello" " " "world"), (string-length "hello")
- Access: (string-ref "hello" 1) → "e" (character at 0-based index)
- Manipulation: (substring "hello" 1 4), (string-upcase "hello"), (string-downcase "HELLO")
- Utilities: (string-trim "  hello  "), (string-replace "banana" "a" "o")
- Predicates: (string-contains? "hello" "ell"), (string-prefix? "hello" "he"), (string-suffix? "hello" "lo"), (string=? "hi" "hi")
- Conversion: (string->number "42"), (number->string 42)
- String-list: (string->list "hi") → ("h" "i"), (list->string (list "h" "i")) → "hi"
- Split/join: (string-split "a,b,c" ",") → ("a" "b" "c"), (string-join (list "hello" "world") " ") → "hello world"

## List operations:

- Uses proper lists only, not cons cells, and the cons operator requires that the second argument is a list
- Construction: (list 1 2 3), (cons 1 (list 2 3)), (append (list 1 2) (list 3 4))
- Access: (first (list 1 2 3)), (rest (list 1 2 3)), (last (list 1 2 3))
- Indexed access: (list-ref (list "a" "b" "c") 1) → "b" (0-based index)
- Properties: (length (list 1 2 3)) [also works with alists], (null? (list)), (member? 2 (list 1 2 3))
- Utilities: (reverse (list 1 2 3)), (remove 2 (list 1 2 3 2 4)), (position 2 (list 1 2 3)) → 1 or #f
- Slicing: (take 3 (list 1 2 3 4 5)), (drop 2 (list 1 2 3 4 5))

## Association lists (alists):

- Immutable key-value mappings with O(1) lookup performance
- Construction: (alist (list "name" "Alice") (list "age" 30))
- Access: (alist-get my-alist "key"), (alist-get my-alist "key" "default")
- Modification: (alist-set my-alist "key" value), (alist-remove my-alist "key")
- Queries: (alist-has? my-alist "key"), (alist-keys my-alist), (alist-values my-alist), (alist-length my-alist)
- Merging: (alist-merge alist1 alist2) - second wins on conflicts
- Type checking: (alist? value)
- Nested alists: (alist (list "user" (alist (list "name" "Bob") (list "id" 123))))
- Works with functional operations: (map f (alist-keys data)), (filter pred (alist-values data))
"- Pattern matching: (match data ((alist? a) ...) (_ ...))\n"
"- Maintains insertion order, optimized for data processing workflows\n\n"

## Type predicates:

- (number? 42) → #t, excludes booleans: (number? #t) → #f
- (integer? 42) → #t, (integer? 3.14) → #f, (integer? (round 3.7)) → #t
- (float? 3.14) → #t, (float? 42) → #f, (float? (/ 1 2)) → #t
- (complex? (+ 1 j)) → #t, (complex? 42) → #f
- (string? "hello") → #t, (boolean? #t) → #t, (list? (list 1 2)) → #t, (alist? (alist ...)) → #t
- (function? (lambda (x) x)) → #t

## Lambda functions:

- (lambda (param1 param2 ...) body) → creates anonymous function
- (lambda (param1 . rest) body) → variadic: rest receives remaining args as a list
- (lambda (. rest) body) → fully variadic: rest receives all args as a list
- ((lambda (x) (* x x)) 5) → 25
- ((lambda (. args) (fold + 0 args)) 1 2 3 4 5) → 15 (variadic sum)
- ((lambda (x . rest) (cons x (reverse rest))) 1 2 3) → (1 3 2)
- Functions are first-class values with lexical scoping and closures
- Tail recursion automatically optimized
- Variadic functions accept any number of args beyond their fixed params; rest param is always a list (possibly empty)

## Local bindings:

- (let ((var1 val1) (var2 val2) ...) body) → parallel binding (bindings independent)
- (let ((x 5) (y 10)) (+ x y)) → 15 (x and y don't reference each other)
- Bindings in let cannot reference each other; use let* for sequential bindings
- (let* ((var1 val1) (var2 val2) ...) body) → sequential binding
- (let* ((x 5) (y (* x 2))) (+ x y)) → 15 (y can reference x)
- (let* ((x 1) (x (+ x 10))) x) → 11 (shadowing works in let*)
- Use let for independent bindings, let* for sequential dependencies

## Recursive bindings:

- (letrec ((var1 val1) (var2 val2) ...) body) → recursive binding
- (letrec ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 5)) → 120
- Supports self-recursion and mutual recursion
- Use only when you need functions that reference themselves

## Higher-order functions:

- (map func list) → (map (lambda (x) (* x 2)) (list 1 2 3)) → (2 4 6)
- (filter predicate list) → (filter (lambda (x) (> x 0)) (list -1 2 -3 4)) → (2 4)
- (fold func init list) → (fold + 0 (list 1 2 3 4)) → 10
- (range start end [step]) → (range 1 5) → (1 2 3 4)
- (find predicate list), (any? predicate list), (all? predicate list)

## Pattern matching:

- (match expression (pattern1 result1) (pattern2 result2) (_ default)) → powerful declarative dispatch
- Literal patterns: (match x (42 "found") ("hello" "greeting") (_ "other"))
- Variable binding: (match x ((number? n) (* n 2)) ((string? s) (string-upcase s)))
- Wildcard patterns: _ matches anything without binding
- Type patterns: (number? var), (string? var), (list? var), (boolean? var), (function? var)
- Empty list: (match lst (() "empty") ((x) "singleton") (_ "multiple"))
- List destructuring: (match lst ((a b c) (+ a b c)) ((head . tail) (cons head tail)))
- Nested patterns: (match data (((number? x) (string? y)) (list x y)) (_ "no match"))
- First match wins: patterns are tested in order, use specific patterns before general ones
- Example: (match data (42 "answer") ((number? n) (* n 2)) ((string? s) (string-upcase s))
((head . tail) (list head (length tail))) (_ \"unknown\"))

## Module system:

- (import \"module-name\") → load and return a module (compile-time operation)
- Modules are just .aifpl files that return a value (typically an alist of functions)
- Modules are cached after first load for performance
- Circular imports are detected and prevented with clear error messages
- Example module (math_utils.aifpl):
  ```aifpl
  (let ((square (lambda (x) (* x x)))
        (cube (lambda (x) (* x x x))))
    (alist
      (list \"square\" square)
      (list \"cube\" cube)))
  ```
- Using a module:
  ```aifpl
  (let ((math (import \"math_utils\")))
    ((alist-get math \"square\") 5))  → 25
  ```
- Modules can import other modules (transitive dependencies)
- Private functions: functions not in the exported alist are private to the module
- Module names can include subdirectories: (e.g. import \"lib/helpers\")
- Available modules can be found in the module search path directories

## Debugging with trace:

- (trace message1 message2 ... messageN expr) → special form for debugging
- Emits messages BEFORE evaluating expr, then returns expr's value
- Messages are evaluated and converted to strings for output
- Traces appear in the tool result context for inspection
- Example: (trace \"Computing factorial\" (factorial 5))
- Multiple messages: (trace \"x=\" x \"y=\" y (+ x y))
- Useful for debugging recursive functions and complex algorithms
- Trace output shows execution order, helping identify logic issues

## Important notes:

- cons behavior is not the same as traditional LISP: second arg must be a list
- Strict typing: string ops need strings, boolean ops need booleans
- Lists only support = and != comparisons, not < > <= >=
- Conditions must be boolean: (if #t ...) works, (if 1 ...) doesn't
- Use for calculations, data processing, and functional programming only
- The user will not see the AIFPL code or AIFPL results directly; if you want to show either, you must format it as a message to the user.
"""

    def _extract_evaluate_context(self, arguments: Dict[str, Any]) -> str | None:
        """
        Extract context for evaluate operation.

        Args:
            arguments: Tool arguments

        Returns:
            Context string if applicable, otherwise None
        """
        expression = arguments.get("expression", "")
        return f"`expression` is:\n```aifpl\n{expression}\n```"

    async def _evaluate(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Evaluate an AIFPL expression with timeout protection.

        Args:
            tool_call: Tool call containing the expression to evaluate
            request_authorization: Authorization callback (not used for AIFPL)

        Returns:
            AIToolResult containing the calculation result

        Raises:
            AIToolExecutionError: If calculation fails or expression is invalid
            AIToolTimeoutError: If calculation takes too long
        """
        arguments = tool_call.arguments
        expression = arguments.get("expression", "")

        # Validate expression type
        if not isinstance(expression, str):
            self._logger.error("AIFPL tool called with non-string expression: %s", type(expression).__name__)
            raise AIToolExecutionError("Expression must be a string")

        try:
            self._logger.debug("Evaluating AIFPL expression: %s", expression)

            # Run calculation with timeout protection and cancellation support
            # We use a Task so we can cancel the thread execution via the VM's cancel() method
            # Create a task for the thread execution
            task = asyncio.create_task(
                asyncio.to_thread(self._evaluate_expression_sync, expression)
            )

            try:
                # Wait for the task with timeout
                result, traces, watcher_clipped = await asyncio.wait_for(
                    task,
                    timeout=10.0  # Increased timeout for complex functional programming
                )

            except asyncio.TimeoutError:
                # On timeout, signal the VM to cancel execution
                # This will cause the VM to raise AIFPLCancelledException at the next check point
                self._logger.warning("AIFPL expression evaluation timed out, requesting cancellation: %s", expression)
                self._tool.vm.cancel()

                # The task is already cancelled by wait_for, so we don't need to wait for it again
                # Just signal the VM to cancel and let the thread finish on its own
                # If the VM responds to cancellation, the thread will complete soon
                # If not, the thread will be orphaned but won't block the event loop
                if not task.done():
                    # Task is still running - give it a moment to respond to cancellation
                    try:
                        await asyncio.wait_for(task, timeout=1.0)

                    except (asyncio.TimeoutError, asyncio.CancelledError, AIFPLCancelledException):
                        pass  # Expected - task still running, cancelled, or VM responded to cancellation

                    except Exception as e:
                        self._logger.debug("Unexpected exception during cancellation grace period: %s", e)

                raise AIToolTimeoutError("AIFPL calculation timed out", 10.0)  # pylint: disable=raise-missing-from

            self._logger.debug("AIFPL evaluation successful: %s = %s", expression, result)

            # Build context only if traces exist
            trace_str = ""

            if traces:
                trace_str = "\n".join(traces)

            result_object = {
                "result": result,
                "trace_data": trace_str,
                "trace_data_clipped": "yes" if watcher_clipped else "no"
            }

            return AIToolResult(
                id=tool_call.id,
                name="AIFPL",
                content=json.dumps(result_object, indent=2),
                context="json"
            )

        except AIToolTimeoutError:
            # Re-raise timeout errors
            raise

        except AIFPLCancelledException as e:
            # Treat cancellation as a timeout (which is what triggered it)
            self._logger.info("AIFPL expression was cancelled: %s", expression)
            raise AIToolTimeoutError("AIFPL calculation timed out", 10.0) from e

        except AIFPLError as e:
            self._logger.warning("AIFPL error in expression '%s': %s", expression, str(e), exc_info=True)
            # Check if this is a division by zero error specifically
            error_msg = str(e).lower()
            if "division by zero" in error_msg:
                raise AIToolExecutionError("Division by zero") from e

            raise AIToolExecutionError(str(e)) from e

        except Exception as e:
            self._logger.error("Unexpected error evaluating AIFPL expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to evaluate AIFPL expression: {str(e)}") from e
