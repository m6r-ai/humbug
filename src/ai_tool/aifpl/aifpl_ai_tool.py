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

## Introduction

- AIFPL (AI Functional Programming Language) is a pure functional programming language designed for efficient expression evaluation.
- It is designed for AIs to use, human users are secondary.
- Operations are runtime typed but most operations will strictly only work on one specific type.
- Pure functional: no side effects, immutable data
- Homoiconic: code and data use same representation (s-expressions)
- Tail call optimization prevents stack overflow
- Strict type system: no implicit coercion between numeric types; use typed operators (integer+, float*, complex/) to enforce types explicitly
- Mixed-type lists supported: (list 1 \"hi\" #t)
- String literals support escapes: \\n, \\t, \\\", \\\\, \\uXXXX
- Comments: use semicolon (;) for single-line comments, e.g., ; This is a comment
- Numeric literals: 42 (integer), 3.14 (float), 3+4j (complex number), 5j (pure imaginary), j (1j)
- Other literals: #xFF (hex), #b1010 (binary), #o755 (octal), \"hello\" (string), #t/#f (boolean), () (empty list)
- Constants: pi, e

## Quote - data literals and code as data:

- (quote expr) → returns expr without evaluation
- 'expr → shortcut for (quote expr)
- '(integer+ 1 2 3) → (integer+ 1 2 3) (as data, not evaluated)
- (list 'hello (integer+ 1 2) 'world) → (hello 3 world)
- Enables symbolic programming: (list-first '(integer+ 1 2)) → integer+
- Code and data have identical representation (homoiconicity)

## Arithmetic and math:

- Bitwise: (bit-or 5 3), (bit-and 7 3), (bit-xor 5 3), (bit-not 5)
- Bit shifts: (bit-shift-left 1 3), (bit-shift-right 8 2)
- Base conversion: (bin 255), (hex 255), (oct 255)

## Typed arithmetic operators:

### Integer arithmetic (all args must be integers; type error otherwise):

- (integer+ 1 2 3) → 6, (integer- 10 3) → 7, (integer* 2 3 4) → 24
- (integer/ 7 3) → 2 (floor division), (integer/ -7 2) → -4
- (integer-neg 5) → -5 (unary negation); (integer- 5) is an error (requires 2+ args)
- (integer+) → 0, (integer*) → 1 (zero-arg identities)
- (integer% 7 3) → 1 (modulo), (integer% -7 2) → 1, (integer% 7 -3) → -2 (result takes sign of divisor)
- (integer-abs -5) → 5 (absolute value)
- (integer-expt 2 10) → 1024, (integer-expt 3 0) → 1, (integer-expt 0 0) → 1 (exact arbitrary-precision integer exponentiation)
- (integer-expt base exp) requires non-negative exponent; raises error for negative exponent (result would not be an integer)

### Float arithmetic (all args must be floats; use (integer->float x) to convert integers):

- (float+ 1.0 2.0 3.0) → 6.0, (float- 10.0 3.0) → 7.0
- (float* 2.0 3.0) → 6.0, (float/ 10.0 4.0) → 2.5
- (float-neg 3.0) → -3.0; (float- 3.0) and (float/ 4.0) are errors (require 2+ args)
- (float-expt 2.0 10.0) → 1024.0
- (float+) → 0.0, (float*) → 1.0 (zero-arg identities)
- (float// 7.0 2.0) → 3.0 (floor division), (float% 7.0 3.0) → 1.0 (modulo)
- (float-floor 3.7) → 3.0, (float-ceil 3.2) → 4.0, (float-round 3.5) → 4.0 (all return float)
- (float-min 1.0 2.0) → 1.0, (float-max 1.0 2.0) → 2.0
- Transcendentals: (float-sin 0.0) → 0.0, (float-cos 0.0) → 1.0, (float-tan 0.0) → 0.0
- (float-log 1.0) → 0.0, (float-log10 1.0) → 0.0, (float-exp 0.0) → 1.0
- (float-sqrt 4.0) → 2.0, (float-abs -3.0) → 3.0
- float-log/float-log10 of zero → -inf; negative arg is a runtime error
- float-sqrt of negative → runtime error (use complex-sqrt instead)
- (float-abs -3.0) → 3.0 (absolute value)

### Complex arithmetic (all args must be complex; use (float->complex r i) to construct):

- (complex+ (float->complex 1.0 2.0) 3+4j) → 4+6j
- (complex- (float->complex 5.0 3.0) (float->complex 2.0 1.0)) → 3+2j
- (complex* 1+2j (float->complex 3.0 4.0)) → -5+10j
- (complex/ 4.0+2.0j (float->complex 1.0 1.0)) → 3-1j
- (complex-neg 3.0+4.0j) → -3-4j
- (complex-abs 3.0+4.0j) → 5.0 (returns magnitude as float, not complex)
- (complex+) → 0+0j, (complex*) → 1+0j (zero-arg identities)
- Transcendentals: complex-sin, complex-cos, complex-tan, complex-log, complex-log10, complex-exp, complex-sqrt
- Exponentials: complex-expt

## Complex numbers:

- Literals: 3+4j, 5j, 1j
- Pure imaginary: 4j, -5j, 1.5e2j → 4j, -5j, 150j
- Complex: 3+4j, 3-4j, 1e2+3e-1j → (3+4j), (3-4j), (100+0.3j)
- (complex-real 3+4j) → 3.0, (complex-imag 3+4j) → 4.0, (complex-abs 3+4j) → 5.0
- (complex-real z) → float real part, (complex-imag z) → float imaginary part (complex args only)

## Type construction and conversion:

- (float->integer x) → convert float to integer (truncates toward zero): (float->integer 3.7) → 3, (float->integer -2.9) → -2
- (integer->float x) → convert integer to float: (integer->float 42) → 42.0, (integer->float 3) → 3.0
- (float->complex real [imag]) → construct complex from one or two floats: (float->complex 3.0 4.0) → 3+4j, (float->complex 3.0) → 3+0j
- (integer->complex real [imag]) → construct complex from one or two integers: (integer->complex 3) → 3+0j, (integer->complex 1 -2) → 1-2j
- (integer->string 42) → "42", (float->string 3.14) → "3.14", (complex->string 3+4j) → "3+4j"
- These are the primary way to move between numeric types; there is no automatic promotion

## Comparison and boolean:

### Type-specific equality and inequality (strict: both args must be the same type):

- integer: (integer=? 1 1), (integer!=? 1 2)
- float: (float=? 1.0 1.0), (float!=? 1.0 2.0)
- complex: (complex=? 1+2j 1+2j), (complex!=? 1+2j 1+3j)
- string: (string=? "hi" "hi"), (string!=? "hi" "bye")
- boolean: (boolean=? #t #t), (boolean!=? #t #f)
- list: (list=? (list 1 2) (list 1 2)), (list!=? (list 1 2) (list 1 3))
- alist: (alist=? a1 a2), (alist!=? a1 a2)

### Type-specific ordered comparisons (strict: both args must be the same type):

- integer: (integer<? 1 2), (integer>? 3 2), (integer<=? 1 1), (integer>=? 2 1)
- float: (float<? 1.0 2.0), (float>? 3.0 2.0), (float<=? 1.0 1.0), (float>=? 2.0 1.0)
- string: (string<? "apple" "banana"), (string>? "b" "a"), (string<=? "a" "a"), (string>=? "b" "a")
- String ordering is Unicode codepoint order (same as Python str), not locale-aware collation
- Complex numbers have no ordering; use (complex-abs z) to compare magnitudes as floats

- (and #t #f), (or #t #f), (boolean-not #t)
- (if (integer>? 5 3) "yes" "no"), lazy evaluation: (if #t 42 0)

## String operations:

- Basic: (string-concat "hello" " " "world"), (string-length "hello")
- Access: (string-ref "hello" 1) → "e" (character at 0-based index)
- Manipulation: (string-slice "hello" 1 4), (string-slice "hello" 2) → "llo", (string-upcase "hello"), (string-downcase "HELLO")
- Utilities: (string-trim "  hello  ") → "hello",(string-trim-left "  hello  ") → "hello  ",  (string-trim-right "  hello  ") → "  hello", (string-replace "banana" "a" "o")
- Equality/ordering predicates: (string=? "hi" "hi"), (string!=? "hi" "bye"), (string<? "apple" "banana"), (string>? "b" "a"), (string<=? "a" "a"), (string>=? "b" "a")
- Search predicates: (string-prefix? "hello" "he"), (string-suffix? "hello" "lo")
- Search index: (string-index "hello" "l") → 2, (string-index "hello" "z") → #f (not found)
- Conversion: (string->number "42") → 42, (string->number "3.14") → 3.14, (string->number "1+2j") → 1+2j
- (string->number "#xFF") → 255, (string->number "#b1010") → 10, (string->number "#o755") → 493, (string->number "-#xFF") → -255
- (string->number "hello") → #f (returns #f for any unparseable string; raises a type error if argument is not a string)
- (integer->string 42), (float->string 3.14), (complex->string 3+4j)
- Split/join: (string->list "hello") → ("h" "e" "l" "l" "o"), (string->list "a,b,c" ",") → ("a" "b" "c")
- Split/join: (list->string (list "h" "i")) → "hi", (list->string (list "a" "b" "c") ",") → "a,b,c"

## List operations:

- Uses proper lists only, not cons cells
- Construction: (list 1 2 3), (list-prepend lst item), (list-append lst item), (list-concat lst1 lst2), (list-concat) → ()
- (list-prepend (list 2 3) 1) → (1 2 3), (list-append (list 1 2) 3) → (1 2 3)
- (list-concat (list 1 2) (list 3 4)) → (1 2 3 4), (list-concat) → () (zero-arg identity)
- Access: (list-first (list 1 2 3)), (list-rest (list 1 2 3)), (list-last (list 1 2 3))
- Indexed access: (list-ref (list "a" "b" "c") 1) → "b" (0-based index)
- Properties: (list-length (list 1 2 3)), (list-null? (list)), (list-member? (list 1 2 3) 2)
- Utilities: (list-reverse (list 1 2 3)), (list-remove (list 1 2 3 2 4) 2), (list-index (list 1 2 3) 2) → 1, (list-index (list 1 2 3) 42) → #f (not found)
- Slicing: (list-slice lst start) → from start to end, (list-slice lst start end) → from start to end (exclusive)
- (list-slice (list 1 2 3 4 5) 2) → (3 4 5), (list-slice (list 1 2 3 4 5) 1 3) → (2 3)

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
- Pattern matching: (match data ((alist? a) ...) (_ ...))
- Maintains insertion order, optimized for data processing workflows

## Type predicates:

- (integer? 42) → #t, (integer? 3.14) → #f
- (float? 3.14) → #t, (float? 42) → #f, (float? (float/ 1.0 2.0)) → #t
- (complex? (float->complex 1.0 1.0)) → #t, (complex? 42) → #f
- (string? "hello") → #t, (boolean? #t) → #t, (list? (list 1 2)) → #t, (alist? (alist ...)) → #t
- (function? (lambda (x) x)) → #t, (function? integer+) → #t
- (symbol? 'foo) → #t, (symbol? "foo") → #f

## Symbol operations:

- (symbol? x) → #t if x is a symbol (produced by quote)
- (symbol=? a b) → #t if a and b are the same symbol
- (symbol!=? a b) → #t if a and b are different symbols
- (symbol->string 'foo) → "foo" (extracts the symbol name as a string)
- Symbols are produced only by quote: 'foo, '(a b c) contains symbols a, b, c
- Example: (map symbol->string '(foo bar baz)) → ("foo" "bar" "baz")

## Function operations:

- (apply f args) → call function f with elements of list args as individual arguments
- (apply integer+ (list 1 2 3)) → 6, (apply list (list 1 2 3)) → (1 2 3)
- (apply f (list)) → calls f with zero arguments (f must accept zero args)
- apply respects all arity rules: fixed-arity functions are checked, variadic args are packed
- (function-min-arity f) → integer: minimum number of arguments f requires
- (function-min-arity integer-abs) → 1, (function-min-arity integer+) → 0, (function-min-arity (lambda (x . rest) x)) → 1
- (function-variadic? f) → #t if f accepts more arguments than its minimum (has a rest parameter)
- (function-variadic? integer+) → #t, (function-variadic? integer-abs) → #f
- (function-accepts? f n) → #t if calling f with exactly n arguments satisfies its arity requirements
- (function-accepts? integer-abs 1) → #t, (function-accepts? integer-abs 2) → #f
- (function-accepts? integer+ 0) → #t, (function-accepts? integer+ 99) → #t
- (function-accepts? (lambda (x . rest) x) 0) → #f, (function-accepts? (lambda (x . rest) x) 3) → #t
- (function=? f g) → #t if f and g are the same function object (identity, not structural equality)
- (function!=? f g) → #t if f and g are different function objects
- (function=? integer+ integer+) → #t, (function=? integer+ integer*) → #f

## Lambda functions:

- (lambda (param1 param2 ...) body) → creates anonymous function
- (lambda (param1 . rest) body) → variadic: rest receives remaining args as a list
- (lambda (. rest) body) → fully variadic: rest receives all args as a list
- ((lambda (x) (integer* x x)) 5) → 25
- ((lambda (. args) (fold integer+ 0 args)) 1 2 3 4 5) → 15 (variadic sum)
- ((lambda (x . rest) (list-prepend (list-reverse rest) x)) 1 2 3) → (1 3 2)
- Functions are first-class values with lexical scoping and closures
- Tail recursion automatically optimized
- Variadic functions accept any number of args beyond their fixed params; rest param is always a list (possibly empty)

## Local bindings:

- (let ((var1 val1) (var2 val2) ...) body) → parallel binding (bindings independent)
- (let ((x 5) (y 10)) (integer+ x y)) → 15 (x and y don't reference each other)
- Bindings in let cannot reference each other; use let* for sequential bindings
- (let* ((var1 val1) (var2 val2) ...) body) → sequential binding
- (let* ((x 5) (y (integer* x 2))) (integer+ x y)) → 15 (y can reference x)
- (let* ((x 1) (x (integer+ x 10))) x) → 11 (shadowing works in let*)
- Use let for independent bindings, let* for sequential dependencies

## Recursive bindings:

- (letrec ((var1 val1) (var2 val2) ...) body) → recursive binding
- (letrec ((fact (lambda (n) (if (integer<=? n 1) 1 (integer* n (fact (integer- n 1))))))) (fact 5)) → 120
- Supports self-recursion and mutual recursion
- Use only when you need functions that reference themselves

## Higher-order functions:

- (map func list) → (map (lambda (x) (integer* x 2)) (list 1 2 3)) → (2 4 6)
- (filter predicate list) → (filter (lambda (x) (integer>? x 0)) (list -1 2 -3 4)) → (2 4)
- (fold func init list) → (fold integer+ 0 (list 1 2 3 4)) → 10
- (range start end [step]) → (range 1 5) → (1 2 3 4), integers only
- (find predicate list), (any? predicate list), (all? predicate list)

## Pattern matching:

- (match expression (pattern1 result1) (pattern2 result2) (_ default)) → powerful declarative dispatch
- Literal patterns: (match x (42 "found") ("hello" "greeting") (_ "other"))
- Variable binding: (match x ((integer? n) (integer* n 2)) ((string? s) (string-upcase s)))
- Wildcard patterns: _ matches anything without binding
- Type patterns: (integer? var), (string? var), (list? var), (boolean? var), (function? var)
- Empty list: (match lst (() "empty") ((x) "singleton") (_ "multiple"))
- List destructuring: (match lst ((a b c) (integer+ a b c)) ((head . tail) (list-prepend tail head)))
- Nested patterns: (match data (((integer? x) (string? y)) (list x y)) (_ "no match"))
- First match wins: patterns are tested in order, use specific patterns before general ones
- Example: (match data (42 "answer") ((integer? n) (integer* n 2)) ((string? s) (string-upcase s))
- ((head . tail) (list head (list-length tail))) (_ \"unknown\"))

## Module system:

- (import \"module-name\") → load and return a module (compile-time operation)
- Modules are just .aifpl files that return a value (typically an alist of functions)
- Modules are cached after first load for performance
- Circular imports are detected and prevented with clear error messages
- Example module (math_utils.aifpl):
  ```aifpl
  (let ((square (lambda (x) (integer* x x)))
        (cube (lambda (x) (integer* x (integer* x x)))))
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
- Multiple messages: (trace \"x=\" x \"y=\" y (integer+ x y))
- Useful for debugging recursive functions and complex algorithms
- Trace output shows execution order, helping identify logic issues

## Raising errors:

- (error "message") → special form that raises a runtime error with the given message
- The argument must be a string literal (not a variable or expression)
- Raises immediately; no value is ever returned
- Valid in any expression position, including inside lambda bodies, let bindings, and match arms
- Used to signal invalid arguments or unrecoverable conditions
- Example: (if (integer<? n 0) (error "n must be non-negative") (float-sqrt (integer->float n)))

## Important notes:

- list-prepend and list-append both take list first, item second: (list-prepend lst item), (list-append lst item)
- string-concat joins strings: (string-concat "hello" " " "world") → "hello world"
- Strict typing: string ops need strings, boolean ops need booleans
- float-floor, float-ceil, float-round all return float, not integer; use (float->integer (float-round x)) to get an integer
- All comparison operators are type-specific: use integer=?, float<?, string>=? etc.
- Lists and alists support =? and !=? only; they have no ordering
- Conditions must be boolean: (if #t ...) works, (if 1 ...) doesn't - there is no concept of "truthiness"
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
