"""Evaluator for AIFPL Abstract Syntax Trees."""

import cmath
import math
from typing import Any, Dict, List, Union, Optional

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_parser import SExpression, LambdaExpr, LetExpr, FunctionCall, StringLiteral
from aifpl.aifpl_environment import Environment, LambdaFunction, TailCall, CallStack


class AIFPLEvaluator:
    """Evaluates AIFPL Abstract Syntax Trees."""

    # Mathematical constants
    CONSTANTS = {
        'pi': math.pi,
        'e': math.e,
        'j': 1j,
        'true': True,
        'false': False,
    }

    # Operator and function definitions
    OPERATORS: Dict[str, Dict[str, Any]] = {
        # Conditional operators
        'if': {'type': 'special', 'args': 3, 'lazy_evaluation': True},

        # Arithmetic operators
        '+': {'type': 'variadic', 'min_args': 0, 'identity': 0},
        '-': {'type': 'variadic', 'min_args': 1},
        '*': {'type': 'variadic', 'min_args': 0, 'identity': 1},
        '/': {'type': 'variadic', 'min_args': 2},
        '//': {'type': 'binary'},
        '%': {'type': 'binary'},
        '**': {'type': 'binary'},

        # Comparison operators
        '=': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '!=': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '<': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '>': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '<=': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},
        '>=': {'type': 'variadic', 'min_args': 2, 'returns_boolean': True},

        # Boolean operators
        'and': {'type': 'special', 'min_args': 0, 'identity': True, 'boolean_only': True, 'lazy_evaluation': True},
        'or': {'type': 'special', 'min_args': 0, 'identity': False, 'boolean_only': True, 'lazy_evaluation': True},
        'not': {'type': 'unary', 'boolean_only': True},

        # Bitwise operators
        'bit-or': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-and': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-xor': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-not': {'type': 'unary', 'bitwise': True},
        'bit-shift-left': {'type': 'binary', 'bitwise': True},
        'bit-shift-right': {'type': 'binary', 'bitwise': True},

        # Mathematical functions
        'sin': {'type': 'unary'},
        'cos': {'type': 'unary'},
        'tan': {'type': 'unary'},
        'log': {'type': 'unary'},
        'log10': {'type': 'unary'},
        'exp': {'type': 'unary'},
        'sqrt': {'type': 'unary'},
        'abs': {'type': 'unary'},
        'round': {'type': 'unary', 'real_only': True},
        'floor': {'type': 'unary', 'real_only': True},
        'ceil': {'type': 'unary', 'real_only': True},
        'min': {'type': 'variadic', 'min_args': 1},
        'max': {'type': 'variadic', 'min_args': 1},
        'pow': {'type': 'binary'},

        # Base conversion functions
        'bin': {'type': 'unary', 'integer_only': True, 'returns_string': True},
        'hex': {'type': 'unary', 'integer_only': True, 'returns_string': True},
        'oct': {'type': 'unary', 'integer_only': True, 'returns_string': True},

        # Complex number functions
        'real': {'type': 'unary'},
        'imag': {'type': 'unary'},
        'complex': {'type': 'binary'},

        # String functions
        'string-append': {'type': 'variadic', 'min_args': 0, 'identity': '', 'string_only': True},
        'string-length': {'type': 'unary', 'string_only': True},
        'substring': {'type': 'ternary', 'string_only': True},
        'string-upcase': {'type': 'unary', 'string_only': True},
        'string-downcase': {'type': 'unary', 'string_only': True},
        'string-ref': {'type': 'binary', 'string_only': True},
        'string->number': {'type': 'unary', 'string_only': True},
        'number->string': {'type': 'unary', 'converts_to_string': True},
        'string-trim': {'type': 'unary', 'string_only': True},
        'string-replace': {'type': 'ternary', 'string_only': True},

        # String predicates
        'string-contains?': {'type': 'binary', 'string_only': True, 'returns_boolean': True},
        'string-prefix?': {'type': 'binary', 'string_only': True, 'returns_boolean': True},
        'string-suffix?': {'type': 'binary', 'string_only': True, 'returns_boolean': True},
        'string=?': {'type': 'variadic', 'min_args': 2, 'string_only': True, 'returns_boolean': True},

        # List construction and manipulation
        'list': {'type': 'variadic', 'min_args': 0, 'list_operation': True},
        'cons': {'type': 'binary', 'list_operation': True},
        'append': {'type': 'variadic', 'min_args': 2, 'list_operation': True},
        'reverse': {'type': 'unary', 'list_operation': True},

        # List access and properties
        'first': {'type': 'unary', 'list_operation': True},
        'rest': {'type': 'unary', 'list_operation': True},
        'last': {'type': 'unary', 'list_operation': True},
        'list-ref': {'type': 'binary', 'list_operation': True},
        'length': {'type': 'unary', 'list_operation': True},

        # List predicates
        'null?': {'type': 'unary', 'list_operation': True, 'returns_boolean': True},
        'list?': {'type': 'unary', 'returns_boolean': True},
        'member?': {'type': 'binary', 'list_operation': True, 'returns_boolean': True},

        # List utilities
        'remove': {'type': 'binary', 'list_operation': True},
        'position': {'type': 'binary', 'returns_boolean_or_value': True},

        # String-list conversion
        'string->list': {'type': 'unary', 'string_only': True, 'returns_list': True},
        'list->string': {'type': 'unary', 'converts_to_string': True},
        'string-split': {'type': 'binary', 'string_only': True, 'returns_list': True},
        'string-join': {'type': 'binary', 'converts_to_string': True},

        # Type predicates
        'number?': {'type': 'unary', 'returns_boolean': True},
        'integer?': {'type': 'unary', 'returns_boolean': True},
        'float?': {'type': 'unary', 'returns_boolean': True},
        'complex?': {'type': 'unary', 'returns_boolean': True},
        'string?': {'type': 'unary', 'returns_boolean': True},
        'boolean?': {'type': 'unary', 'returns_boolean': True},
        'function?': {'type': 'unary', 'returns_boolean': True},

        # Functional iteration operations
        'map': {'type': 'binary', 'higher_order': True},
        'filter': {'type': 'binary', 'higher_order': True},
        'fold': {'type': 'ternary', 'higher_order': True},
        'range': {'type': 'variadic', 'min_args': 2, 'max_args': 3, 'higher_order': True},
        'find': {'type': 'binary', 'higher_order': True},
        'any?': {'type': 'binary', 'higher_order': True, 'returns_boolean': True},
        'all?': {'type': 'binary', 'higher_order': True, 'returns_boolean': True},
        'take': {'type': 'binary', 'list_operation': True},
        'drop': {'type': 'binary', 'list_operation': True},
    }

    # Tolerance for considering imaginary part as zero
    IMAGINARY_TOLERANCE = 1e-10

    def __init__(self, max_depth: int = 100, imaginary_tolerance: float = 1e-10):
        """
        Initialize evaluator.

        Args:
            max_depth: Maximum recursion depth
            imaginary_tolerance: Tolerance for considering imaginary part as zero
        """
        self.max_depth = max_depth
        self.imaginary_tolerance = imaginary_tolerance
        self.call_stack = CallStack()

        # NEW: Add call chain tracking for mutual recursion detection
        self.call_chain: List[LambdaFunction] = []

    # FIXED: Updated return type to include LambdaFunction
    def evaluate(
        self,
        expr: SExpression,
        env: Optional[Environment] = None,
        depth: int = 0
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """
        Recursively evaluate AST.

        Args:
            expr: Expression to evaluate
            env: Environment for variable lookups
            depth: Current recursion depth

        Returns:
            Evaluation result

        Raises:
            AIFPLEvalError: If evaluation fails
        """
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # Create global environment if none provided
        if env is None:
            env = Environment(name="global")
            # Add constants to global environment
            for name, value in self.CONSTANTS.items():
                env.define(name, value)

            # Add built-in operators to global environment as strings (operator names)
            # This allows symbol lookup to succeed, and the evaluator will handle them as built-ins
            for operator_name in self.OPERATORS.keys():
                env.define(operator_name, operator_name)

        try:
            return self._evaluate_expression(expr, env, depth)
        except AIFPLEvalError:
            # Re-raise AIFPL errors as-is
            raise
        except Exception as e:
            # Wrap other exceptions with context
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Unexpected error during evaluation: {e}\nCall stack:\n{stack_trace}") from e

    # FIXED: Updated return type to include LambdaFunction
    def _evaluate_expression(
        self,
        expr: SExpression,
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """Internal expression evaluation with type dispatch."""

        # Check depth limit at the start of every expression evaluation
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # Atom evaluation (literals only - NOT symbols)
        if isinstance(expr, (int, float, complex, bool)):
            return expr

        # String literal evaluation - return the string value directly
        if isinstance(expr, StringLiteral):
            return expr.value

        # Symbol lookup (strings that represent variable names)
        if isinstance(expr, str):
            try:
                return env.lookup(expr)
            except AIFPLEvalError as e:
                # Add more context to symbol lookup errors
                stack_trace = self.call_stack.format_stack_trace()
                if stack_trace.strip() != "(no function calls)":
                    raise AIFPLEvalError(f"{e}\nCall stack:\n{stack_trace}") from e
                else:
                    raise

        # Lambda expression - FIXED: Now allowed in return type
        if isinstance(expr, LambdaExpr):
            return LambdaFunction(
                parameters=expr.parameters,
                body=expr.body,
                closure_env=env,
                name="<lambda>"
            )

        # Let expression
        if isinstance(expr, LetExpr):
            return self._evaluate_let_expression(expr, env, depth + 1)

        # Function call
        if isinstance(expr, FunctionCall):
            return self._evaluate_function_call(expr, env, depth + 1)

        # Legacy list evaluation (for backwards compatibility)
        if isinstance(expr, list):
            if not expr:
                raise AIFPLEvalError("Cannot evaluate empty list")

            # Convert to FunctionCall for consistent handling
            func_call = FunctionCall(
                function=expr[0],
                arguments=expr[1:],
                position=0
            )
            return self._evaluate_function_call(func_call, env, depth + 1)

        raise AIFPLEvalError(f"Invalid expression type: {type(expr).__name__}")

    def _evaluate_let_expression(
        self,
        let_expr: LetExpr,
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """
        Evaluate let expression with sequential binding.

        Args:
            let_expr: Let expression to evaluate
            env: Current environment
            depth: Current recursion depth

        Returns:
            Result of evaluating the let body
        """
        # Create new environment for let bindings
        let_env = env.create_child("let")

        # Sequential binding: each binding can reference previous ones
        for var_name, var_expr in let_expr.bindings:
            try:
                var_value = self._evaluate_expression(var_expr, let_env, depth)
                let_env.define(var_name, var_value)
            except AIFPLEvalError as e:
                raise AIFPLEvalError(f"Error evaluating let binding '{var_name}': {e}") from e

        # Evaluate body in the let environment
        return self._evaluate_expression(let_expr.body, let_env, depth)

    def _evaluate_function_call(
        self,
        func_call: FunctionCall,
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """
        Evaluate function call with tail call optimization.

        Args:
            func_call: Function call to evaluate
            env: Current environment
            depth: Current recursion depth

        Returns:
            Result of the function call
        """
        # Check if this is a tail call that can be optimized
        return self._evaluate_tail_optimized_call(func_call, env, depth)

    def _evaluate_tail_optimized_call(
        self,
        func_call: FunctionCall,
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """
        Evaluate function call with tail call optimization.

        This method uses iteration instead of recursion for tail calls to prevent stack overflow.
        """
        current_call = func_call
        current_env = env

        while True:
            # Check if the function is a string (symbol) and not a known operator
            if isinstance(current_call.function, str):
                func_name = current_call.function
                # If it's not a known operator and not in the environment, it's an unknown operator
                if func_name not in self.OPERATORS and not current_env.has_binding(func_name):
                    raise AIFPLEvalError(f"Unknown operator: '{func_name}'")

            # Evaluate the function expression
            try:
                func_value = self._evaluate_expression(current_call.function, current_env, depth)
            except AIFPLEvalError as e:
                if "Undefined variable" in str(e) and isinstance(current_call.function, str):
                    func_name = current_call.function
                    if func_name not in self.OPERATORS:
                        raise AIFPLEvalError(f"Unknown operator: '{func_name}'") from e
                raise AIFPLEvalError(f"Error evaluating function expression: {e}") from e

            # Handle different types of functions
            if isinstance(func_value, LambdaFunction):
                # User-defined function call
                result = self._call_lambda_function(func_value, current_call.arguments, current_env, depth)

                # Check if result is a tail call
                if isinstance(result, TailCall):
                    # Continue the loop with the tail call
                    current_call = FunctionCall(
                        function=result.function,
                        arguments=result.arguments,
                        position=current_call.position
                    )
                    current_env = result.environment
                    continue
                else:
                    # Regular result, return it
                    return result

            elif isinstance(func_value, str):
                # String literals that evaluate to strings should not be treated as operators
                if isinstance(current_call.function, str):
                    # This is a symbol (identifier), so it can be a built-in operator
                    return self._apply_builtin_operator(func_value, current_call.arguments, current_env, depth + 1)
                # This is a string literal, so it's not a function
                raise AIFPLEvalError(f"Cannot call non-function value: {type(func_value).__name__}")

            else:
                raise AIFPLEvalError(f"Cannot call non-function value: {type(func_value).__name__}")

    def _call_lambda_function(
        self,
        func: LambdaFunction,
        args: List[SExpression],
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction, TailCall]:
        """
        Call a lambda function with given arguments.

        Args:
            func: Lambda function to call
            args: Argument expressions
            env: Current environment
            depth: Current recursion depth

        Returns:
            Function result or TailCall for optimization
        """
        # Check arity
        if len(args) != len(func.parameters):
            raise AIFPLEvalError(
                f"Function expects {len(func.parameters)} arguments, got {len(args)}. "
                f"Parameters: {func.parameters}"
            )

        # Evaluate arguments in current environment
        try:
            arg_values = [self._evaluate_expression(arg, env, depth) for arg in args]
        except AIFPLEvalError as e:
            raise AIFPLEvalError(f"Error evaluating function arguments: {e}") from e

        # Create new environment for function execution
        func_env = func.closure_env.create_child(f"{func.name}-call")

        # Bind parameters to arguments
        param_bindings = {}
        for param, arg_value in zip(func.parameters, arg_values):
            func_env.define(param, arg_value)
            param_bindings[param] = arg_value

        # Add call frame to stack for error reporting
        self.call_stack.push(
            function_name=func.name or "<lambda>",
            arguments=param_bindings,
            expression=str(func.body) if hasattr(func.body, '__str__') else "<body>"
        )

        # Track function in call chain for mutual recursion detection
        self.call_chain.append(func)

        try:
            # Enable tail call optimization with mutual recursion support
            result = self._evaluate_with_tail_detection(func.body, func_env, depth, func)
            return result

        finally:
            # Always pop the call frame and remove from call chain
            self.call_stack.pop()

            # Remove function from call chain
            if self.call_chain and self.call_chain[-1] is func:
                self.call_chain.pop()

    def _is_tail_call(self, expr: SExpression) -> bool:
        """
        Check if an expression could be a tail call.

        This is a simple check - actual tail position analysis happens elsewhere.
        Only FunctionCall objects can be tail calls.
        """
        return isinstance(expr, FunctionCall)

    def _is_in_tail_position(self, expr: SExpression, context_expr: SExpression) -> bool:
        """
        Check if an expression is in tail position within a context.

        Tail position means the result of expr becomes the result of context_expr.
        """
        # For if expressions, both then and else branches are in tail position
        if isinstance(context_expr, FunctionCall) and isinstance(context_expr.function, str):
            if context_expr.function == 'if' and len(context_expr.arguments) == 3:
                then_branch = context_expr.arguments[1]
                else_branch = context_expr.arguments[2]
                return expr == then_branch or expr == else_branch

        # For let expressions, the body is in tail position
        if isinstance(context_expr, LetExpr):
            return expr == context_expr.body

        # For lambda expressions, the body is in tail position
        if isinstance(context_expr, LambdaExpr):
            return expr == context_expr.body

        return False

    def _is_recursive_call(self, func_value: LambdaFunction, call_chain: List[LambdaFunction]) -> bool:
        """
        Check if a function call is recursive (simple or mutual).

        Args:
            func_value: The function being called
            call_chain: List of functions currently being executed

        Returns:
            True if this is a recursive call (simple or mutual)
        """
        # Simple recursion: calling the same function
        if call_chain and func_value == call_chain[-1]:
            return True

        # Mutual recursion: calling any function in the current call chain
        # Use object identity comparison since LambdaFunction objects are unique
        for chain_func in call_chain:
            if func_value is chain_func:
                return True

        return False

    def _evaluate_with_tail_detection(
        self,
        expr: SExpression,
        env: Environment,
        depth: int,
        current_function: LambdaFunction
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction, TailCall]:
        """
        Evaluate an expression with tail call detection.

        Args:
            expr: Expression to evaluate
            env: Environment
            depth: Current depth
            current_function: The function we're currently executing

        Returns:
            Either a regular result or a TailCall object for optimization
        """
        # FIXED: Check depth limit here too - this is where the deep recursion happens
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # Handle if expressions specially - branches are in tail position
        if isinstance(expr, FunctionCall) and isinstance(expr.function, str) and expr.function == 'if':
            if len(expr.arguments) != 3:
                raise AIFPLEvalError(f"if requires exactly 3 arguments, got {len(expr.arguments)}")

            condition_expr, then_expr, else_expr = expr.arguments

            # Evaluate condition (not in tail position)
            condition = self._evaluate_expression(condition_expr, env, depth + 1)

            if not isinstance(condition, bool):
                raise AIFPLEvalError(f"if requires boolean condition, got {type(condition).__name__}")

            # Evaluate chosen branch (in tail position)
            if condition:
                return self._evaluate_with_tail_detection(then_expr, env, depth + 1, current_function)

            return self._evaluate_with_tail_detection(else_expr, env, depth + 1, current_function)

        # Handle function calls - check for tail calls
        elif isinstance(expr, FunctionCall):
            # Evaluate the function
            func_value = self._evaluate_expression(expr.function, env, depth + 1)

            # If it's a lambda function, check for recursion (simple or mutual)
            if isinstance(func_value, LambdaFunction):
                # Use the call chain we're tracking
                if self._is_recursive_call(func_value, self.call_chain):
                    # This is a recursive call (simple or mutual)!
                    return TailCall(
                        function=expr.function,
                        arguments=expr.arguments,
                        environment=env
                    )

                # Don't fall back to regular recursion!
                return self._call_lambda_function(func_value, expr.arguments, env, depth + 1)

            # Built-in function, evaluate normally
            return self._evaluate_function_call(expr, env, depth + 1)

        # For other expressions, evaluate normally
        else:
            return self._evaluate_expression(expr, env, depth + 1)

    def _python_value_to_ast_node(self, value: Any) -> SExpression:
        """
        Convert a Python value to the appropriate AST node type.

        This is needed for higher-order functions that need to pass evaluated values
        as arguments to other functions.

        Args:
            value: Python value to convert

        Returns:
            Appropriate AST node
        """
        if isinstance(value, str):
            return StringLiteral(value)

        if isinstance(value, (int, float, complex, bool)):
            return value

        if isinstance(value, list):
            # Convert each element recursively
            list_elements = [self._python_value_to_ast_node(item) for item in value]
            return FunctionCall(function="list", arguments=list_elements, position=0)

        # For other types (like LambdaFunction), return as-is
        return value

    def _apply_builtin_operator(
        self,
        operator: str,
        args: List[SExpression],
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """Apply built-in operators and functions."""
        if operator not in self.OPERATORS:
            raise AIFPLEvalError(f"Unknown operator: '{operator}'")

        op_def: Dict[str, Any] = self.OPERATORS[operator]

        # Handle special forms that require lazy evaluation
        if op_def.get('type') == 'special':
            if operator == 'if':
                return self._apply_if_conditional(args, env, depth)

            if operator == 'and':
                return self._apply_and_short_circuit(args, env, depth)

            if operator == 'or':
                return self._apply_or_short_circuit(args, env, depth)

        # Handle higher-order functions (map, filter, fold, etc.)
        if op_def.get('higher_order'):
            return self._apply_higher_order_function(operator, args, env, depth)

        # For regular operators, evaluate arguments first - FIXED: Increment depth
        try:
            evaluated_args = [self._evaluate_expression(arg, env, depth + 1) for arg in args]

        except AIFPLEvalError as e:
            raise AIFPLEvalError(f"Error evaluating arguments for '{operator}': {e}") from e

        # Check argument count
        self._validate_arity(operator, op_def, evaluated_args)

        # Handle special mixed return types (position function)
        if op_def.get('returns_boolean_or_value'):
            return self._apply_mixed_return_operator(operator, evaluated_args)

        # FIXED: Handle operations that return booleans FIRST (before string_only check)
        if op_def.get('returns_boolean'):
            return self._apply_boolean_returning_operator(operator, evaluated_args)

        # Handle list operations
        if op_def.get('list_operation'):
            return self._apply_list_operator(operator, evaluated_args)

        # Handle special cases that return strings
        if op_def.get('returns_string'):
            return self._apply_string_function(operator, evaluated_args)

        # Handle functions that convert to strings
        if op_def.get('converts_to_string'):
            return self._apply_conversion_to_string(operator, evaluated_args)

        # Handle functions that return lists
        if op_def.get('returns_list'):
            return self._apply_list_returning_function(operator, evaluated_args)

        # Handle boolean-only operations (only NOT now, since AND/OR are special)
        if op_def.get('boolean_only'):
            return self._apply_boolean_operator(operator, evaluated_args)

        # Handle string-only operations (now only for non-boolean returning functions)
        if op_def.get('string_only'):
            return self._apply_string_operator(operator, op_def, evaluated_args)

        # Filter out string, boolean, and list arguments for mathematical operations
        for arg in evaluated_args:
            if isinstance(arg, (str, bool, list)):
                raise AIFPLEvalError(f"Operator '{operator}' cannot operate on {type(arg).__name__} arguments")

        # Handle bitwise operations (require integers)
        if op_def.get('bitwise'):
            return self._apply_bitwise_operator(operator, evaluated_args)

        # Handle real-only operations
        if op_def.get('real_only'):
            return self._apply_real_only_function(operator, evaluated_args)

        # Handle integer-only operations
        if op_def.get('integer_only'):
            return self._apply_integer_only_function(operator, evaluated_args)

        # Handle regular mathematical operations
        return self._apply_mathematical_operator(operator, op_def, evaluated_args)

    def _apply_mixed_return_operator(self, operator: str, args: List[Any]) -> Union[int, bool]:
        """Apply operators that return mixed types (like position)."""
        if operator == 'position':
            if len(args) != 2:
                raise AIFPLEvalError(f"position requires exactly 2 arguments, got {len(args)}")

            item, list_arg = args
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"position requires list as second argument, got {type(list_arg).__name__}")

            # Find first occurrence using same equality semantics as =
            for i, list_item in enumerate(list_arg):
                if self._values_equal(item, list_item):
                    return i

            return False  # Return #f if not found (consistent with find)

        raise AIFPLEvalError(f"Unknown mixed return operator: '{operator}'")

    def _apply_and_short_circuit(self, args: List[SExpression], env: Environment, depth: int) -> bool:
        """
        Handle AND with short-circuit evaluation.

        Args:
            args: List of unevaluated arguments
            env: Current environment
            depth: Current recursion depth

        Returns:
            Boolean result of AND operation
        """
        # Empty AND returns True (identity)
        if not args:
            return True

        # Evaluate arguments one by one, short-circuiting on first False
        for arg in args:
            result = self._evaluate_expression(arg, env, depth + 1)

            # Validate that result is boolean
            if not isinstance(result, bool):
                raise AIFPLEvalError(f"Operator 'and' requires boolean arguments, got {type(result).__name__}")

            # Short-circuit: if any argument is False, return False immediately
            if not result:
                return False

        # All arguments were True
        return True

    def _apply_or_short_circuit(self, args: List[SExpression], env: Environment, depth: int) -> bool:
        """
        Handle OR with short-circuit evaluation.

        Args:
            args: List of unevaluated arguments
            env: Current environment
            depth: Current recursion depth

        Returns:
            Boolean result of OR operation
        """
        # Empty OR returns False (identity)
        if not args:
            return False

        # Evaluate arguments one by one, short-circuiting on first True
        for arg in args:
            result = self._evaluate_expression(arg, env, depth + 1)

            # Validate that result is boolean
            if not isinstance(result, bool):
                raise AIFPLEvalError(f"Operator 'or' requires boolean arguments, got {type(result).__name__}")

            # Short-circuit: if any argument is True, return True immediately
            if result:
                return True

        # All arguments were False
        return False

    def _apply_higher_order_function(
        self,
        operator: str,
        args: List[SExpression],
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """Apply higher-order functions like map, filter, fold."""
        if operator == 'map':
            if len(args) != 2:
                raise AIFPLEvalError(f"map requires exactly 2 arguments (function, list), got {len(args)}")

            func_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth)
            if not isinstance(list_value, list):
                raise AIFPLEvalError(f"map requires list as second argument, got {type(list_value).__name__}")

            # Apply function to each element
            result = []
            for item in list_value:
                # FIXED: Convert Python values to appropriate AST nodes
                item_ast = self._python_value_to_ast_node(item)
                item_call = FunctionCall(function=func_expr, arguments=[item_ast], position=0)
                item_result = self._evaluate_function_call(item_call, env, depth + 1)
                result.append(item_result)

            return result

        if operator == 'filter':
            if len(args) != 2:
                raise AIFPLEvalError(f"filter requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth)
            if not isinstance(list_value, list):
                raise AIFPLEvalError(f"filter requires list as second argument, got {type(list_value).__name__}")

            # Filter elements based on predicate
            result = []
            for item in list_value:
                # FIXED: Convert Python values to appropriate AST nodes
                item_ast = self._python_value_to_ast_node(item)
                pred_call = FunctionCall(function=pred_expr, arguments=[item_ast], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, bool):
                    raise AIFPLEvalError(f"filter predicate must return boolean, got {type(pred_result).__name__}")

                if pred_result:
                    result.append(item)

            return result

        if operator == 'fold':
            if len(args) != 3:
                raise AIFPLEvalError(f"fold requires exactly 3 arguments (function, initial, list), got {len(args)}")

            func_expr, init_expr, list_expr = args

            # Evaluate initial value and list
            accumulator = self._evaluate_expression(init_expr, env, depth)
            list_value = self._evaluate_expression(list_expr, env, depth)
            if not isinstance(list_value, list):
                raise AIFPLEvalError(f"fold requires list as third argument, got {type(list_value).__name__}")

            # Fold over the list
            for item in list_value:
                # FIXED: Convert Python values to appropriate AST nodes
                acc_ast = self._python_value_to_ast_node(accumulator)
                item_ast = self._python_value_to_ast_node(item)
                fold_call = FunctionCall(function=func_expr, arguments=[acc_ast, item_ast], position=0)
                accumulator = self._evaluate_function_call(fold_call, env, depth + 1)

            return accumulator

        if operator == 'range':
            # FIXED: Check arity BEFORE evaluating arguments
            if len(args) < 2 or len(args) > 3:
                raise AIFPLEvalError(f"range requires 2 or 3 arguments (start, end[, step]), got {len(args)}")

            # Now evaluate arguments
            evaluated_args = [self._evaluate_expression(arg, env, depth) for arg in args]
            if len(evaluated_args) < 2 or len(evaluated_args) > 3:
                raise AIFPLEvalError(f"range requires 2 or 3 arguments (start, end[, step]), got {len(evaluated_args)}")

            if len(evaluated_args) == 2:
                start_val2, end_val2 = evaluated_args
                if not isinstance(start_val2, (int, float)):
                    raise AIFPLEvalError(f"range argument 1 must be numeric, got {type(start_val2).__name__}")

                if not isinstance(end_val2, (int, float)):
                    raise AIFPLEvalError(f"range argument 2 must be numeric, got {type(end_val2).__name__}")

                start_val = int(start_val2)
                end_val = int(end_val2)
                step_val: int = 1

            else:
                start_val3, end_val3, step_val3 = evaluated_args
                if not isinstance(start_val3, (int, float)):
                    raise AIFPLEvalError(f"range argument 1 must be numeric, got {type(start_val3).__name__}")

                if not isinstance(end_val3, (int, float)):
                    raise AIFPLEvalError(f"range argument 2 must be numeric, got {type(end_val3).__name__}")

                if not isinstance(step_val3, (int, float)):
                    raise AIFPLEvalError(f"range argument 3 must be numeric, got {type(step_val3).__name__}")

                start_val = int(start_val3)
                end_val = int(end_val3)
                step_val = int(step_val3)

            if step_val == 0:
                raise AIFPLEvalError("range step cannot be zero")

            # Generate range
            return list(range(start_val, end_val, step_val))

        if operator == 'find':
            if len(args) != 2:
                raise AIFPLEvalError(f"find requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth)
            if not isinstance(list_value, list):
                raise AIFPLEvalError(f"find requires list as second argument, got {type(list_value).__name__}")

            # Find first element matching predicate
            for item in list_value:
                # FIXED: Convert Python values to appropriate AST nodes
                item_ast = self._python_value_to_ast_node(item)
                pred_call = FunctionCall(function=pred_expr, arguments=[item_ast], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, bool):
                    raise AIFPLEvalError(f"find predicate must return boolean, got {type(pred_result).__name__}")

                if pred_result:
                    return item

            return False  # Return #f if not found

        if operator == 'any?':
            if len(args) != 2:
                raise AIFPLEvalError(f"any? requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth)
            if not isinstance(list_value, list):
                raise AIFPLEvalError(f"any? requires list as second argument, got {type(list_value).__name__}")

            # Check if any element matches predicate
            for item in list_value:
                # FIXED: Convert Python values to appropriate AST nodes
                item_ast = self._python_value_to_ast_node(item)
                pred_call = FunctionCall(function=pred_expr, arguments=[item_ast], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, bool):
                    raise AIFPLEvalError(f"any? predicate must return boolean, got {type(pred_result).__name__}")

                if pred_result:
                    return True

            return False

        if operator == 'all?':
            if len(args) != 2:
                raise AIFPLEvalError(f"all? requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth)
            if not isinstance(list_value, list):
                raise AIFPLEvalError(f"all? requires list as second argument, got {type(list_value).__name__}")

            # Check if all elements match predicate
            for item in list_value:
                # FIXED: Convert Python values to appropriate AST nodes
                item_ast = self._python_value_to_ast_node(item)
                pred_call = FunctionCall(function=pred_expr, arguments=[item_ast], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, bool):
                    raise AIFPLEvalError(f"all? predicate must return boolean, got {type(pred_result).__name__}")

                if not pred_result:
                    return False

            return True

        raise AIFPLEvalError(f"Higher-order function '{operator}' not yet implemented")

    def _apply_if_conditional(
        self,
        args: List[SExpression],
        env: Environment,
        depth: int
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """
        Handle if conditional with lazy evaluation of branches.

        Args:
            args: List of unevaluated arguments [condition, then-expr, else-expr]
            env: Current environment
            depth: Current recursion depth

        Returns:
            Result of evaluating the chosen branch

        Raises:
            AIFPLEvalError: If condition is not boolean or wrong number of arguments
        """
        if len(args) != 3:
            raise AIFPLEvalError(f"Operator 'if' requires exactly 3 arguments (condition, then, else), got {len(args)}")

        condition_expr, then_expr, else_expr = args

        # Evaluate condition first - FIXED: Increment depth
        condition = self._evaluate_expression(condition_expr, env, depth + 1)

        # Validate condition is boolean
        if not isinstance(condition, bool):
            raise AIFPLEvalError(f"Operator 'if' requires boolean condition, got {type(condition).__name__}")

        # Lazy evaluation: only evaluate the chosen branch - FIXED: Increment depth
        if condition:
            return self._evaluate_expression(then_expr, env, depth + 1)

        return self._evaluate_expression(else_expr, env, depth + 1)

    def _validate_arity(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> None:
        """Validate argument count for an operator."""
        op_type = op_def['type']
        arg_count = len(args)

        if op_type == 'unary' and arg_count != 1:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 1 argument, got {arg_count}")

        if op_type == 'binary' and arg_count != 2:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 2 arguments, got {arg_count}")

        if op_type == 'ternary' and arg_count != 3:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 3 arguments, got {arg_count}")

        if op_type == 'variadic':
            min_args = op_def.get('min_args', 0)
            max_args = op_def.get('max_args')

            if arg_count < min_args:
                raise AIFPLEvalError(f"Operator '{operator}' requires at least {min_args} arguments, got {arg_count}")

            if max_args is not None and arg_count > max_args:
                raise AIFPLEvalError(f"Operator '{operator}' accepts at most {max_args} arguments, got {arg_count}")

    def _apply_string_function(self, operator: str, args: List[Any]) -> str:
        """Apply functions that return strings."""
        if operator == 'bin':
            if len(args) != 1:
                raise AIFPLEvalError(f"bin requires exactly 1 argument, got {len(args)}")
            arg = self._to_integer(args[0], operator)
            return bin(arg)

        if operator == 'hex':
            if len(args) != 1:
                raise AIFPLEvalError(f"hex requires exactly 1 argument, got {len(args)}")
            arg = self._to_integer(args[0], operator)
            return hex(arg)

        if operator == 'oct':
            if len(args) != 1:
                raise AIFPLEvalError(f"oct requires exactly 1 argument, got {len(args)}")
            arg = self._to_integer(args[0], operator)
            return oct(arg)

        raise AIFPLEvalError(f"Unknown string function: '{operator}'")

    def _apply_conversion_to_string(self, operator: str, args: List[Any]) -> str:
        """Apply functions that convert values to strings."""
        if operator == 'number->string':
            if len(args) != 1:
                raise AIFPLEvalError(f"number->string requires exactly 1 argument, got {len(args)}")

            arg = args[0]
            if not isinstance(arg, (int, float, complex)):
                raise AIFPLEvalError(f"number->string requires numeric argument, got {type(arg).__name__}")

            return str(arg)

        if operator == 'list->string':
            if len(args) != 1:
                raise AIFPLEvalError(f"list->string requires exactly 1 argument, got {len(args)}")

            arg = args[0]
            if not isinstance(arg, list):
                raise AIFPLEvalError(f"list->string requires list argument, got {type(arg).__name__}")

            # Convert list of characters to string
            try:
                return ''.join(str(item) for item in arg)

            except Exception as e:
                raise AIFPLEvalError(f"Cannot convert list to string: {e}")

        if operator == 'string-join':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-join requires exactly 2 arguments, got {len(args)}")

            string_list, separator = args
            if not isinstance(string_list, list):
                raise AIFPLEvalError(f"string-join requires list as first argument, got {type(string_list).__name__}")

            if not isinstance(separator, str):
                raise AIFPLEvalError(f"string-join requires string as second argument, got {type(separator).__name__}")

            # Ensure all list elements are strings
            str_items = []
            for item in string_list:
                if not isinstance(item, str):
                    raise AIFPLEvalError(f"string-join requires list of strings, found {type(item).__name__}")
                str_items.append(item)

            return separator.join(str_items)

        raise AIFPLEvalError(f"Unknown string conversion function: '{operator}'")

    def _apply_list_returning_function(self, operator: str, args: List[Any]) -> list:
        """Apply functions that return lists."""
        if operator == 'string->list':
            if len(args) != 1:
                raise AIFPLEvalError(f"string->list requires exactly 1 argument, got {len(args)}")

            arg = args[0]
            if not isinstance(arg, str):
                raise AIFPLEvalError(f"string->list requires string argument, got {type(arg).__name__}")

            return list(arg)

        if operator == 'string-split':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-split requires exactly 2 arguments, got {len(args)}")

            string_arg, delimiter = args
            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"string-split requires string as first argument, got {type(string_arg).__name__}")

            if not isinstance(delimiter, str):
                raise AIFPLEvalError(f"string-split requires string as second argument, got {type(delimiter).__name__}")

            # FIXED: Handle empty separator case - split into individual characters
            if delimiter == "":
                return list(string_arg)

            return string_arg.split(delimiter)

        raise AIFPLEvalError(f"Unknown list-returning function: '{operator}'")

    def _apply_boolean_operator(self, operator: str, args: List[Any]) -> bool:
        """Apply boolean operators (only NOT now, since AND/OR are special)."""
        if operator == 'not':
            if len(args) != 1:
                raise AIFPLEvalError(f"not requires exactly 1 argument, got {len(args)}")

            arg = args[0]
            if not isinstance(arg, bool):
                raise AIFPLEvalError(f"Operator 'not' requires boolean arguments, got {type(arg).__name__}")

            return not arg

        raise AIFPLEvalError(f"Unknown boolean operator: '{operator}'")

    # FIXED: Updated return type to include complex
    def _apply_string_operator(
        self,
        operator: str,
        op_def: Dict[str, Any],
        args: List[Any]
    ) -> Union[str, int, float, bool, complex]:
        """Apply string operations."""
        # Special handling for functions that need integer indices
        if operator in ('substring', 'string-ref'):
            # First argument must be string
            if not isinstance(args[0], str):
                raise AIFPLEvalError(
                    f"Operator '{operator}' requires string as first argument, argument 1 is {type(args[0]).__name__}"
                )

            if operator == 'substring':
                if len(args) != 3:
                    raise AIFPLEvalError(f"substring requires exactly 3 arguments, got {len(args)}")

                string_arg, start_arg, end_arg = args

                # Convert start and end to integers (they might be passed as integers)
                if not isinstance(start_arg, int):
                    raise AIFPLEvalError(f"substring requires integer indices, start argument is {type(start_arg).__name__}")

                if not isinstance(end_arg, int):
                    raise AIFPLEvalError(f"substring requires integer indices, end argument is {type(end_arg).__name__}")

                # FIXED: Add proper bounds checking for substring
                string_len = len(string_arg)
                if start_arg < 0:
                    raise AIFPLEvalError(f"substring start index cannot be negative: {start_arg}")

                if end_arg < 0:
                    raise AIFPLEvalError(f"substring end index cannot be negative: {end_arg}")

                if start_arg > string_len:
                    raise AIFPLEvalError(f"substring start index out of range: {start_arg} (string length: {string_len})")

                if end_arg > string_len:
                    raise AIFPLEvalError(f"substring end index out of range: {end_arg} (string length: {string_len})")

                if start_arg > end_arg:
                    raise AIFPLEvalError(f"substring start index ({start_arg}) cannot be greater than end index ({end_arg})")

                return string_arg[start_arg:end_arg]

            if operator == 'string-ref':
                if len(args) != 2:
                    raise AIFPLEvalError(f"string-ref requires exactly 2 arguments, got {len(args)}")

                string_arg, index_arg = args

                # Index must be integer
                if not isinstance(index_arg, int):
                    raise AIFPLEvalError(f"string-ref requires integer index, got {type(index_arg).__name__}")

                # FIXED: Add bounds checking for string-ref
                string_len = len(string_arg)
                if index_arg < 0:
                    raise AIFPLEvalError(f"string-ref index out of range: {index_arg}")

                if index_arg >= string_len:
                    raise AIFPLEvalError(f"string-ref index out of range: {index_arg}")

                return string_arg[index_arg]

        if operator == 'string-trim':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-trim requires exactly 1 argument, got {len(args)}")

            string_arg = args[0]
            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"string-trim requires string argument, got {type(string_arg).__name__}")

            return string_arg.strip()

        if operator == 'string-replace':
            if len(args) != 3:
                raise AIFPLEvalError(f"string-replace requires exactly 3 arguments, got {len(args)}")

            string_arg, old_str, new_str = args
            if not isinstance(string_arg, str):
                raise AIFPLEvalError(f"string-replace requires string as first argument, got {type(string_arg).__name__}")

            if not isinstance(old_str, str):
                raise AIFPLEvalError(f"string-replace requires string as second argument, got {type(old_str).__name__}")

            if not isinstance(new_str, str):
                raise AIFPLEvalError(f"string-replace requires string as third argument, got {type(new_str).__name__}")

            # Non-overlapping replacement using Python's replace method
            return string_arg.replace(old_str, new_str)

        # For other string operations, validate all arguments are strings
        for i, arg in enumerate(args):
            if not isinstance(arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' requires string arguments, argument {i+1} is {type(arg).__name__}")

        if operator == 'string-append':
            if not args:
                return op_def.get('identity', '')
            return ''.join(args)

        if operator == 'string-length':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-length requires exactly 1 argument, got {len(args)}")
            return len(args[0])

        if operator == 'string-upcase':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-upcase requires exactly 1 argument, got {len(args)}")
            return args[0].upper()

        if operator == 'string-downcase':
            if len(args) != 1:
                raise AIFPLEvalError(f"string-downcase requires exactly 1 argument, got {len(args)}")
            return args[0].lower()

        if operator == 'string->number':
            if len(args) != 1:
                raise AIFPLEvalError(f"string->number requires exactly 1 argument, got {len(args)}")

            string_arg = args[0]
            try:
                # Try to parse as integer first
                if '.' not in string_arg and 'e' not in string_arg.lower() and 'j' not in string_arg.lower():
                    return int(string_arg)
                # Try complex number - FIXED: This can return complex which is now allowed
                elif 'j' in string_arg.lower():
                    return complex(string_arg)
                # Otherwise float
                else:
                    return float(string_arg)
            except ValueError:
                raise AIFPLEvalError(f"Cannot convert string to number: '{string_arg}'")

        raise AIFPLEvalError(f"Unknown string operator: '{operator}'")

    def _strict_equality_check(self, args: List[Any]) -> bool:
        """
        Perform strict type-aware equality check with numeric equivalence.

        Args:
            args: List of values to compare for equality

        Returns:
            True if all values are equal under AIFPL equality rules
        """
        if len(args) < 2:
            return True  # Vacuous truth for single argument

        first = args[0]

        # Check all remaining arguments against the first
        for arg in args[1:]:
            if not self._values_equal(first, arg):
                return False

        return True

    def _strict_inequality_check(self, args: List[Any]) -> bool:
        """
        Perform not-equal check - returns true if any inequality exists.

        Args:
            args: List of values to compare for inequality

        Returns:
            True if any values are not equal under AIFPL equality rules
        """
        if len(args) < 2:
            return False  # Vacuous case: single argument is equal to itself

        # Check if any pair is not equal
        for i in range(len(args)):
            for j in range(i + 1, len(args)):
                if not self._values_equal(args[i], args[j]):
                    return True

        return False  # All values are equal

    def _values_equal(self, a: Any, b: Any) -> bool:
        """
        Check if two values are equal under AIFPL equality rules.

        Rules:
        1. Same types: use Python equality
        2. Numeric types: allow int/float/complex equivalence
        3. Different non-numeric types: always False

        Args:
            a: First value
            b: Second value

        Returns:
            True if values are equal under AIFPL rules
        """
        # Same type - use Python equality
        if type(a) == type(b):
            return a == b

        # Both are numeric types - allow equivalence
        if self._is_numeric(a) and self._is_numeric(b):
            return self._numeric_equal(a, b)

        # Different non-numeric types are never equal
        return False

    def _is_numeric(self, value: Any) -> bool:
        """Check if a value is a numeric type (excluding booleans)."""
        # FIXED: Exclude booleans explicitly since bool is a subclass of int in Python
        return isinstance(value, (int, float, complex)) and not isinstance(value, bool)

    def _numeric_equal(self, a: Union[int, float, complex], b: Union[int, float, complex]) -> bool:
        """
        Check numeric equality allowing type promotion.

        Args:
            a: First numeric value
            b: Second numeric value

        Returns:
            True if numerically equivalent
        """
        # Handle complex numbers specially
        if isinstance(a, complex) or isinstance(b, complex):
            # Convert both to complex for comparison
            complex_a = complex(a) if not isinstance(a, complex) else a
            complex_b = complex(b) if not isinstance(b, complex) else b

            # Check if they're equal within tolerance for imaginary parts
            real_equal = abs(complex_a.real - complex_b.real) < 1e-15
            imag_equal = abs(complex_a.imag - complex_b.imag) < self.imaginary_tolerance

            return real_equal and imag_equal

        # Both are real numbers (int or float)
        return a == b

    def _apply_boolean_returning_operator(self, operator: str, args: List[Any]) -> bool:
        """Apply operators that return boolean values."""
        if operator in ('=', '!=', '<', '>', '<=', '>='):
            if len(args) < 2:
                raise AIFPLEvalError(f"Operator '{operator}' requires at least 2 arguments, got {len(args)}")

            # Handle equality with strict type checking
            if operator == '=':
                return self._strict_equality_check(args)

            # Handle inequality with strict type checking
            if operator == '!=':
                return self._strict_inequality_check(args)

            # For comparison operators, ensure all arguments are numeric
            for i, arg in enumerate(args):
                if isinstance(arg, (str, bool, list)):
                    raise AIFPLEvalError(
                        f"Operator '{operator}' requires numeric arguments, argument {i+1} is {type(arg).__name__}"
                    )

            # Check comparison chain
            for i in range(len(args) - 1):
                left, right = args[i], args[i + 1]

                if operator == '<' and not left < right:
                    return False

                if operator == '>' and not left > right:
                    return False

                if operator == '<=' and not left <= right:
                    return False

                if operator == '>=' and not left >= right:
                    return False

            return True

        # String predicates
        if operator == 'string-contains?':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-contains? requires exactly 2 arguments, got {len(args)}")

            string_arg, substring = args
            if not isinstance(string_arg, str) or not isinstance(substring, str):
                raise AIFPLEvalError("string-contains? requires string arguments")

            return substring in string_arg

        if operator == 'string-prefix?':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-prefix? requires exactly 2 arguments, got {len(args)}")

            string_arg, prefix = args
            if not isinstance(string_arg, str) or not isinstance(prefix, str):
                raise AIFPLEvalError("string-prefix? requires string arguments")

            return string_arg.startswith(prefix)

        if operator == 'string-suffix?':
            if len(args) != 2:
                raise AIFPLEvalError(f"string-suffix? requires exactly 2 arguments, got {len(args)}")

            string_arg, suffix = args
            if not isinstance(string_arg, str) or not isinstance(suffix, str):
                raise AIFPLEvalError("string-suffix? requires string arguments")

            return string_arg.endswith(suffix)

        if operator == 'string=?':
            if len(args) < 2:
                raise AIFPLEvalError(f"string=? requires at least 2 arguments, got {len(args)}")

            for arg in args:
                if not isinstance(arg, str):
                    raise AIFPLEvalError("string=? requires string arguments")

            first = args[0]
            return all(arg == first for arg in args[1:])

        # List predicates
        if operator == 'null?':
            if len(args) != 1:
                raise AIFPLEvalError(f"null? requires exactly 1 argument, got {len(args)}")

            arg = args[0]
            if not isinstance(arg, list):
                raise AIFPLEvalError(f"null? requires list argument, got {type(arg).__name__}")

            return len(arg) == 0

        if operator == 'list?':
            if len(args) != 1:
                raise AIFPLEvalError(f"list? requires exactly 1 argument, got {len(args)}")

            return isinstance(args[0], list)

        if operator == 'member?':
            if len(args) != 2:
                raise AIFPLEvalError(f"member? requires exactly 2 arguments, got {len(args)}")

            item, list_arg = args
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"member? requires list as second argument, got {type(list_arg).__name__}")

            return item in list_arg

        # Type predicates
        if operator == 'number?':
            if len(args) != 1:
                raise AIFPLEvalError(f"number? requires exactly 1 argument, got {len(args)}")

            return self._is_numeric(args[0])

        if operator == 'integer?':
            if len(args) != 1:
                raise AIFPLEvalError(f"integer? requires exactly 1 argument, got {len(args)}")

            return isinstance(args[0], int) and not isinstance(args[0], bool)

        if operator == 'float?':
            if len(args) != 1:
                raise AIFPLEvalError(f"float? requires exactly 1 argument, got {len(args)}")

            return isinstance(args[0], float)

        if operator == 'complex?':
            if len(args) != 1:
                raise AIFPLEvalError(f"complex? requires exactly 1 argument, got {len(args)}")

            return isinstance(args[0], complex)

        if operator == 'string?':
            if len(args) != 1:
                raise AIFPLEvalError(f"string? requires exactly 1 argument, got {len(args)}")

            return isinstance(args[0], str)

        if operator == 'boolean?':
            if len(args) != 1:
                raise AIFPLEvalError(f"boolean? requires exactly 1 argument, got {len(args)}")

            return isinstance(args[0], bool)

        if operator == 'function?':
            if len(args) != 1:
                raise AIFPLEvalError(f"function? requires exactly 1 argument, got {len(args)}")

            return isinstance(args[0], LambdaFunction)

        raise AIFPLEvalError(f"Unknown boolean-returning operator: '{operator}'")

    def _apply_bitwise_operator(self, operator: str, args: List[Any]) -> int:
        """Apply bitwise operators (require integer arguments)."""
        # Convert all arguments to integers
        int_args = []
        for i, arg in enumerate(args):
            try:
                int_arg = self._to_integer(arg, operator)
                int_args.append(int_arg)
            except AIFPLEvalError:
                raise AIFPLEvalError(f"Operator '{operator}' requires integer arguments, argument {i+1} is {type(arg).__name__}")

        if operator == 'bit-or':
            result = int_args[0]
            for arg in int_args[1:]:
                result |= arg
            return result

        if operator == 'bit-and':
            result = int_args[0]
            for arg in int_args[1:]:
                result &= arg
            return result

        if operator == 'bit-xor':
            result = int_args[0]
            for arg in int_args[1:]:
                result ^= arg
            return result

        if operator == 'bit-not':
            if len(int_args) != 1:
                raise AIFPLEvalError(f"bit-not requires exactly 1 argument, got {len(int_args)}")
            return ~int_args[0]

        if operator == 'bit-shift-left':
            if len(int_args) != 2:
                raise AIFPLEvalError(f"bit-shift-left requires exactly 2 arguments, got {len(int_args)}")
            return int_args[0] << int_args[1]

        if operator == 'bit-shift-right':
            if len(int_args) != 2:
                raise AIFPLEvalError(f"bit-shift-right requires exactly 2 arguments, got {len(int_args)}")
            return int_args[0] >> int_args[1]

        raise AIFPLEvalError(f"Unknown bitwise operator: '{operator}'")

    def _apply_real_only_function(self, operator: str, args: List[Any]) -> Union[int, float]:
        """Apply functions that only work with real numbers."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function '{operator}' requires exactly 1 argument, got {len(args)}")

        arg = args[0]

        # Extract real part if complex
        if isinstance(arg, complex):
            if abs(arg.imag) >= self.imaginary_tolerance:
                raise AIFPLEvalError(f"Function '{operator}' does not support complex numbers")
            arg = arg.real

        if operator == 'round':
            return round(arg)

        if operator == 'floor':
            return math.floor(arg)

        if operator == 'ceil':
            return math.ceil(arg)

        raise AIFPLEvalError(f"Unknown real-only function: '{operator}'")

    def _apply_integer_only_function(self, operator: str, args: List[Any]) -> int:
        """Apply functions that require integer arguments."""
        if len(args) != 1:
            raise AIFPLEvalError(f"Function '{operator}' requires exactly 1 argument, got {len(args)}")

        _arg = self._to_integer(args[0], operator)

        # These functions are handled in _apply_string_function
        # This method is for integer operations that return integers
        raise AIFPLEvalError(f"Unknown integer-only function: '{operator}'")

    def _apply_mathematical_operator(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> Union[int, float, complex]:
        """Apply mathematical operators."""
        if operator == '+':
            if not args:
                return op_def.get('identity', 0)

            # Promote types and sum
            promoted_args = self._promote_types(*args)
            return sum(promoted_args)

        if operator == '-':
            if len(args) == 1:
                # Unary minus
                return -args[0]

            # Variadic subtraction
            promoted_args = self._promote_types(*args)
            result = promoted_args[0]
            for arg in promoted_args[1:]:
                result -= arg

            return result

        if operator == '*':
            if not args:
                return op_def.get('identity', 1)

            # Promote types and multiply
            promoted_args = self._promote_types(*args)
            result = promoted_args[0]
            for arg in promoted_args[1:]:
                result *= arg

            return result

        if operator == '/':
            if len(args) < 2:
                raise AIFPLEvalError(f"Division requires at least 2 arguments, got {len(args)}")

            # Check for division by zero
            for i, arg in enumerate(args[1:], 1):
                if arg == 0:
                    raise AIFPLEvalError(f"Division by zero at argument {i+1}")

            # Promote types and divide
            promoted_args = self._promote_types(*args)
            result = promoted_args[0]
            for arg in promoted_args[1:]:
                result /= arg

            return result

        if operator == '//':
            if len(args) != 2:
                raise AIFPLEvalError(f"Floor division requires exactly 2 arguments, got {len(args)}")

            left, right = args
            if right == 0:
                raise AIFPLEvalError("Division by zero")

            return left // right

        if operator == '%':
            if len(args) != 2:
                raise AIFPLEvalError(f"Modulo requires exactly 2 arguments, got {len(args)}")

            left, right = args
            if right == 0:
                raise AIFPLEvalError("Modulo by zero")

            return left % right

        if operator == '**' or operator == 'pow':
            if len(args) != 2:
                raise AIFPLEvalError(f"Power requires exactly 2 arguments, got {len(args)}")

            base, exponent = args
            return base ** exponent

        # Mathematical functions
        if operator == 'sin':
            if len(args) != 1:
                raise AIFPLEvalError(f"sin requires exactly 1 argument, got {len(args)}")

            return cmath.sin(args[0]) if isinstance(args[0], complex) else math.sin(args[0])

        if operator == 'cos':
            if len(args) != 1:
                raise AIFPLEvalError(f"cos requires exactly 1 argument, got {len(args)}")

            return cmath.cos(args[0]) if isinstance(args[0], complex) else math.cos(args[0])

        if operator == 'tan':
            if len(args) != 1:
                raise AIFPLEvalError(f"tan requires exactly 1 argument, got {len(args)}")

            return cmath.tan(args[0]) if isinstance(args[0], complex) else math.tan(args[0])

        if operator == 'log':
            if len(args) != 1:
                raise AIFPLEvalError(f"log requires exactly 1 argument, got {len(args)}")

            arg = args[0]
            if isinstance(arg, complex) or (isinstance(arg, (int, float)) and arg < 0):
                return cmath.log(arg)

            return math.log(arg)

        if operator == 'log10':
            if len(args) != 1:
                raise AIFPLEvalError(f"log10 requires exactly 1 argument, got {len(args)}")
            arg = args[0]
            if isinstance(arg, complex) or (isinstance(arg, (int, float)) and arg < 0):
                return cmath.log10(arg)

            return math.log10(arg)

        if operator == 'exp':
            if len(args) != 1:
                raise AIFPLEvalError(f"exp requires exactly 1 argument, got {len(args)}")

            return cmath.exp(args[0]) if isinstance(args[0], complex) else math.exp(args[0])

        if operator == 'sqrt':
            if len(args) != 1:
                raise AIFPLEvalError(f"sqrt requires exactly 1 argument, got {len(args)}")
            arg = args[0]
            if isinstance(arg, complex) or (isinstance(arg, (int, float)) and arg < 0):
                return cmath.sqrt(arg)

            return math.sqrt(arg)

        if operator == 'abs':
            if len(args) != 1:
                raise AIFPLEvalError(f"abs requires exactly 1 argument, got {len(args)}")

            return abs(args[0])

        if operator == 'min':
            if not args:
                raise AIFPLEvalError("min requires at least 1 argument")

            return min(args)

        if operator == 'max':
            if not args:
                raise AIFPLEvalError("max requires at least 1 argument")

            return max(args)

        # Complex number functions
        if operator == 'real':
            if len(args) != 1:
                raise AIFPLEvalError(f"real requires exactly 1 argument, got {len(args)}")

            return self._extract_real_part(args[0])

        if operator == 'imag':
            if len(args) != 1:
                raise AIFPLEvalError(f"imag requires exactly 1 argument, got {len(args)}")

            return self._extract_imaginary_part(args[0])

        if operator == 'complex':
            if len(args) != 2:
                raise AIFPLEvalError(f"complex requires exactly 2 arguments, got {len(args)}")

            real_part, imag_part = args
            return complex(real_part, imag_part)

        raise AIFPLEvalError(f"Unknown mathematical operator: '{operator}'")

    def _apply_list_operator(self, operator: str, args: List[Any]) -> Union[list, bool, Any]:
        """Apply list operations."""
        if operator == 'list':
            return list(args)

        if operator == 'cons':
            if len(args) != 2:
                raise AIFPLEvalError(f"cons requires exactly 2 arguments, got {len(args)}")

            item, list_arg = args
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"cons requires list as second argument, got {type(list_arg).__name__}")

            return [item] + list_arg

        if operator == 'append':
            if len(args) < 2:
                raise AIFPLEvalError(f"append requires at least 2 arguments, got {len(args)}")

            # Validate all arguments are lists
            for i, arg in enumerate(args):
                if not isinstance(arg, list):
                    raise AIFPLEvalError(f"append requires list arguments, argument {i+1} is {type(arg).__name__}")

            # Concatenate all lists
            result = []
            for list_arg in args:
                result.extend(list_arg)
            return result

        if operator == 'reverse':
            if len(args) != 1:
                raise AIFPLEvalError(f"reverse requires exactly 1 argument, got {len(args)}")

            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"reverse requires list argument, got {type(list_arg).__name__}")

            return list(reversed(list_arg))

        if operator == 'first':
            if len(args) != 1:
                raise AIFPLEvalError(f"first requires exactly 1 argument, got {len(args)}")

            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"first requires list argument, got {type(list_arg).__name__}")

            if not list_arg:
                raise AIFPLEvalError("Cannot get first element of empty list")

            return list_arg[0]

        if operator == 'rest':
            if len(args) != 1:
                raise AIFPLEvalError(f"rest requires exactly 1 argument, got {len(args)}")

            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"rest requires list argument, got {type(list_arg).__name__}")

            if not list_arg:
                raise AIFPLEvalError("Cannot get rest of empty list")

            return list_arg[1:]

        # NEW: last operator
        if operator == 'last':
            if len(args) != 1:
                raise AIFPLEvalError(f"last requires exactly 1 argument, got {len(args)}")

            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"last requires list argument, got {type(list_arg).__name__}")

            if not list_arg:
                raise AIFPLEvalError("Cannot get last element of empty list")

            return list_arg[-1]

        if operator == 'list-ref':
            if len(args) != 2:
                raise AIFPLEvalError(f"list-ref requires exactly 2 arguments, got {len(args)}")

            list_arg, index = args
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"list-ref requires list as first argument, got {type(list_arg).__name__}")

            index = self._to_integer(index, operator)

            # FIXED: Add bounds checking for list-ref including negative indices
            list_len = len(list_arg)
            if index < 0:
                raise AIFPLEvalError(f"list-ref index out of range: {index}")
            if index >= list_len:
                raise AIFPLEvalError(f"list-ref index out of range: {index}")

            return list_arg[index]

        if operator == 'length':
            if len(args) != 1:
                raise AIFPLEvalError(f"length requires exactly 1 argument, got {len(args)}")

            list_arg = args[0]
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"length requires list argument, got {type(list_arg).__name__}")

            return len(list_arg)

        # NEW: remove operator
        if operator == 'remove':
            if len(args) != 2:
                raise AIFPLEvalError(f"remove requires exactly 2 arguments, got {len(args)}")

            item, list_arg = args
            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"remove requires list as second argument, got {type(list_arg).__name__}")

            # Remove all occurrences using same equality semantics as =
            result = []
            for list_item in list_arg:
                if not self._values_equal(item, list_item):
                    result.append(list_item)

            return result

        if operator == 'take':
            if len(args) != 2:
                raise AIFPLEvalError(f"take requires exactly 2 arguments, got {len(args)}")

            n, list_arg = args

            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"take requires list as second argument, got {type(list_arg).__name__}")

            n = self._to_integer(n, operator)
            if n < 0:
                raise AIFPLEvalError(f"take count cannot be negative: {n}")

            return list_arg[:n]

        if operator == 'drop':
            if len(args) != 2:
                raise AIFPLEvalError(f"drop requires exactly 2 arguments, got {len(args)}")

            n, list_arg = args

            if not isinstance(list_arg, list):
                raise AIFPLEvalError(f"drop requires list as second argument, got {type(list_arg).__name__}")

            n = self._to_integer(n, operator)
            if n < 0:
                raise AIFPLEvalError(f"drop count cannot be negative: {n}")

            return list_arg[n:]

        # Boolean-returning list operations are handled in _apply_boolean_returning_operator
        raise AIFPLEvalError(f"Unknown list operator: '{operator}'")

    def _extract_real_part(self, value: Union[int, float, complex]) -> Union[int, float]:
        """Extract the real part of a number."""
        if isinstance(value, complex):
            real_part = value.real
            # Convert to int if it's a whole number
            if isinstance(real_part, float) and real_part.is_integer():
                return int(real_part)
            return real_part

        # For real numbers, return as-is
        return value

    def _extract_imaginary_part(self, value: Union[int, float, complex]) -> Union[int, float]:
        """Extract the imaginary part of a number."""
        if isinstance(value, complex):
            imag_part = value.imag
            # Convert to int if it's a whole number
            if isinstance(imag_part, float) and imag_part.is_integer():
                return int(imag_part)
            return imag_part

        # For real numbers, imaginary part is 0
        return 0

    def _promote_types(self, *values: Any) -> tuple:
        """Promote arguments to common type: int → float → complex."""
        has_complex = any(isinstance(v, complex) for v in values)
        has_float = any(isinstance(v, float) for v in values)

        if has_complex:
            return tuple(complex(v) for v in values)

        if has_float:
            return tuple(float(v) for v in values)

        return values  # All integers

    def _to_integer(self, value: Union[int, float, complex], operation: str) -> int:
        """Convert a numeric value to integer for operations that require it."""
        if isinstance(value, complex):
            if abs(value.imag) >= self.imaginary_tolerance:
                raise AIFPLEvalError(f"Operation '{operation}' does not support complex numbers")

            value = value.real

        if isinstance(value, float):
            if not value.is_integer():
                raise AIFPLEvalError(f"Operation '{operation}' requires integer values, got float: {value}")

            value = int(value)

        return value

    def _is_close_to_nice_number(self, value: float) -> Union[float, None]:
        """
        Check if a float is very close to a 'nice' number and return the nice number if so.

        Args:
            value: Float value to check

        Returns:
            The nice number if close, None otherwise
        """
        # Check if it's close to common fractions with small denominators
        for denominator in range(1, 11):  # Check denominators 1-10
            for numerator in range(-50, 51):  # Check reasonable range
                nice_value = numerator / denominator
                if abs(value - nice_value) < self.imaginary_tolerance:
                    return nice_value

        return None

    def simplify_result(
        self,
        result: Union[int, float, complex, str, bool, list, LambdaFunction]
    ) -> Union[int, float, complex, str, bool, list, LambdaFunction]:
        """Simplify complex results to real numbers when imaginary part is negligible."""
        if isinstance(result, complex):
            # If imaginary part is effectively zero, return just the real part
            if abs(result.imag) < self.imaginary_tolerance:
                real_part = result.real
                # Convert to int if it's a whole number
                if isinstance(real_part, float) and real_part.is_integer():
                    return int(real_part)

                return real_part

        # For real numbers, convert float to int if it's a whole number
        if isinstance(result, float) and result.is_integer():
            return int(result)

        return result

    def _escape_string_for_lisp(self, s: str) -> str:
        """
        Escape a string for LISP display format.

        Only escapes the minimal necessary characters:
        - backslashes
        - double quotes
        - control characters (newline, tab, etc.)

        Preserves Unicode characters as-is.
        """
        result = []
        for char in s:
            if char == '"':
                result.append('\\"')

            elif char == '\\':
                result.append('\\\\')

            elif char == '\n':
                result.append('\\n')

            elif char == '\t':
                result.append('\\t')

            elif char == '\r':
                result.append('\\r')

            elif ord(char) < 32:  # Other control characters
                result.append(f'\\u{ord(char):04x}')

            else:
                result.append(char)  # Keep Unicode as-is

        return ''.join(result)

    def format_result(self, result: Union[int, float, complex, str, bool, list, LambdaFunction]) -> str:
        """
        Format result for display, using LISP conventions for lists and booleans.

        Args:
            result: The result to format

        Returns:
            String representation of the result
        """
        if isinstance(result, bool):
            return "#t" if result else "#f"

        if isinstance(result, str):
            escaped_content = self._escape_string_for_lisp(result)
            return f'"{escaped_content}"'

        if isinstance(result, float):
            nice_number = self._is_close_to_nice_number(result)
            if nice_number is not None:
                # If it's close to an integer, show as integer
                if nice_number == int(nice_number):
                    return str(int(nice_number))
                else:
                    return str(nice_number)
            else:
                # For other floats, use standard representation
                return str(result)

        if isinstance(result, list):
            # Format list in LISP notation: (element1 element2 ...)
            if not result:
                return "()"

            formatted_elements = []
            for element in result:
                formatted_elements.append(self.format_result(element))

            return f"({' '.join(formatted_elements)})"

        if isinstance(result, LambdaFunction):
            # Format lambda functions
            param_str = " ".join(result.parameters)
            return f"<lambda ({param_str})>"

        # For other types, use standard string representation
        return str(result)
