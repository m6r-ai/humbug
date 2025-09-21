"""Evaluator for AIFPL Abstract Syntax Trees using AIFPLValue hierarchy."""

import math
from typing import Any, Dict, List, Union, Optional

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_parser import AIFPLSExpression, AIFPLLambdaExpr, AIFPLLetExpr, AIFPLFunctionCall
from aifpl.aifpl_environment import AIFPLEnvironment, AIFPLFunction, AIFPLTailCall, AIFPLCallStack
from aifpl.aifpl_value import AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList, AIFPLRecursivePlaceholder
from aifpl.aifpl_evaluator_operators import AIFPLOperatorMixin
from aifpl.aifpl_dependency_analyzer import DependencyAnalyzer, BindingGroup


class AIFPLEvaluator(AIFPLOperatorMixin):
    """Evaluates AIFPL Abstract Syntax Trees using AIFPLValue objects."""

    # Mathematical constants
    CONSTANTS = {
        'pi': AIFPLNumber(math.pi),
        'e': AIFPLNumber(math.e),
        'j': AIFPLNumber(1j),
        'true': AIFPLBoolean(True),
        'false': AIFPLBoolean(False),
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
        self.call_stack = AIFPLCallStack()

        # Add call chain tracking for mutual recursion detection
        self.call_chain: List[AIFPLFunction] = []

    def evaluate(
        self,
        expr: AIFPLSExpression,
        env: Optional[AIFPLEnvironment] = None,
        depth: int = 0
    ) -> AIFPLValue:
        """
        Recursively evaluate AST.

        Args:
            expr: Expression to evaluate
            env: Environment for variable lookups
            depth: Current recursion depth

        Returns:
            Evaluation result as AIFPLValue

        Raises:
            AIFPLEvalError: If evaluation fails
        """
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # Create global environment if none provided
        if env is None:
            env = AIFPLEnvironment(name="global")
            # Add constants to global environment
            for name, value in self.CONSTANTS.items():
                env = env.define(name, value)

            # Add built-in operators to global environment as strings (operator names)
            # This allows symbol lookup to succeed, and the evaluator will handle them as built-ins
            for operator_name in self.OPERATORS:
                env = env.define(operator_name, AIFPLString(operator_name))

        try:
            return self._evaluate_expression(expr, env, depth)

        except AIFPLEvalError:
            # Re-raise AIFPL errors as-is
            raise

        except Exception as e:
            # Wrap other exceptions with context
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Unexpected error during evaluation: {e}\nCall stack:\n{stack_trace}") from e

    def _evaluate_expression(
        self,
        expr: AIFPLSExpression,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """Internal expression evaluation with type dispatch."""

        # Check depth limit at the start of every expression evaluation
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # AIFPLValue evaluation (atoms)
        if isinstance(expr, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            # Self-evaluating atoms
            return expr

        # Symbol lookup
        if isinstance(expr, AIFPLSymbol):
            try:
                return env.lookup(expr.name)

            except AIFPLEvalError as e:
                # Add more context to symbol lookup errors
                stack_trace = self.call_stack.format_stack_trace()
                if stack_trace.strip() != "(no function calls)":
                    raise AIFPLEvalError(f"{e}\nCall stack:\n{stack_trace}") from e

                raise

        # List evaluation - just return the list as-is
        if isinstance(expr, AIFPLList):
            return expr

        # Lambda expression
        if isinstance(expr, AIFPLLambdaExpr):
            return AIFPLFunction(
                parameters=tuple(expr.parameters),
                body=expr.body,
                closure_env=env,
                name="<lambda>"
            )

        # Let expression
        if isinstance(expr, AIFPLLetExpr):
            return self._evaluate_let_expression(expr, env, depth + 1)

        # Function call
        if isinstance(expr, AIFPLFunctionCall):
            return self._evaluate_function_call(expr, env, depth + 1)

        raise AIFPLEvalError(f"Invalid expression type: {type(expr).__name__}")

    def _evaluate_let_expression(
        self,
        let_expr: AIFPLLetExpr,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Evaluate let expression with automatic recursion detection.

        Args:
            let_expr: Let expression to evaluate
            env: Current environment
            depth: Current recursion depth

        Returns:
            Result of evaluating the let body
        """
        # Analyze dependencies
        analyzer = DependencyAnalyzer()
        binding_groups = analyzer.analyze_let_bindings(let_expr.bindings)

        # Evaluate groups in order
        current_env = env.create_child("let")

        for group in binding_groups:
            if group.is_recursive:
                current_env = self._evaluate_recursive_binding_group(group, current_env, depth)

            else:
                current_env = self._evaluate_sequential_binding_group(group, current_env, depth)

        # Evaluate body in the final environment
        return self._evaluate_expression(let_expr.body, current_env, depth)

    def _evaluate_sequential_binding_group(
        self,
        group: BindingGroup,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLEnvironment:
        """Evaluate a non-recursive binding group sequentially."""
        current_env = env

        for name, expr in group.bindings:
            try:
                value = self._evaluate_expression(expr, current_env, depth + 1)
                current_env = current_env.define(name, value)

            except AIFPLEvalError as e:
                raise AIFPLEvalError(f"Error evaluating let binding '{name}': {e}") from e

        return current_env

    def _evaluate_recursive_binding_group(
        self,
        group: BindingGroup,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLEnvironment:
        """Evaluate a recursive binding group using recursive placeholders."""
        # Step 1: Create environment with recursive placeholders
        recursive_env = env
        placeholders = {}

        for name, _ in group.bindings:
            placeholder = AIFPLRecursivePlaceholder(name)
            placeholders[name] = placeholder
            recursive_env = recursive_env.define(name, placeholder)

        # Step 2: Evaluate all binding expressions in the recursive environment
        resolved_values = {}
        for name, expr in group.bindings:
            try:
                value = self._evaluate_expression(expr, recursive_env, depth + 1)
                resolved_values[name] = value

            except AIFPLEvalError as e:
                raise AIFPLEvalError(f"Error evaluating recursive let binding '{name}': {e}") from e

        # Step 3: Update placeholders with resolved values
        for name, placeholder in placeholders.items():
            placeholder.resolve(resolved_values[name])

        # Step 4: Create final environment with resolved values
        final_env = env
        for name, value in resolved_values.items():
            final_env = final_env.define(name, value)

        return final_env

    def _evaluate_function_call(
        self,
        func_call: AIFPLFunctionCall,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
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
        func_call: AIFPLFunctionCall,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Evaluate function call with tail call optimization.

        This method uses iteration instead of recursion for tail calls to prevent stack overflow.
        """
        current_call = func_call
        current_env = env

        while True:
            # Check if the function is a symbol and not a known operator
            if isinstance(current_call.function, AIFPLSymbol):
                func_name = current_call.function.name
                # If it's not a known operator and not in the environment, it's an unknown operator
                if func_name not in self.OPERATORS and not current_env.has_binding(func_name):
                    raise AIFPLEvalError(f"Unknown operator: '{func_name}'")

            # Evaluate the function expression
            try:
                func_value = self._evaluate_expression(current_call.function, current_env, depth)
            except AIFPLEvalError as e:
                if "Undefined variable" in str(e) and isinstance(current_call.function, AIFPLSymbol):
                    func_name = current_call.function.name
                    if func_name not in self.OPERATORS:
                        raise AIFPLEvalError(f"Unknown operator: '{func_name}'") from e
                raise AIFPLEvalError(f"Error evaluating function expression: {e}") from e

            # Handle different types of functions
            if isinstance(func_value, AIFPLFunction):
                # User-defined function call
                result = self._call_lambda_function(func_value, current_call.arguments, current_env, depth)

                # Check if result is a tail call
                if isinstance(result, AIFPLTailCall):
                    # Continue the loop with the tail call
                    current_call = AIFPLFunctionCall(
                        function=result.function,
                        arguments=result.arguments,
                        position=current_call.position
                    )
                    current_env = result.environment
                    continue

                # Regular result, return it
                return result

            # Built-in operator (symbols evaluate to their name strings)
            if isinstance(func_value, AIFPLString) and isinstance(current_call.function, AIFPLSymbol):
                return self._apply_builtin_operator(func_value.value, current_call.arguments, current_env, depth + 1)

            raise AIFPLEvalError(f"Cannot call non-function value: {func_value.type_name()}")

    def _call_lambda_function(
        self,
        func: AIFPLFunction,
        args: List[AIFPLSExpression],
        env: AIFPLEnvironment,
        depth: int
    ) -> Union[AIFPLValue, AIFPLTailCall]:
        """
        Call a lambda function with given arguments.

        Args:
            func: Lambda function to call
            args: Argument expressions
            env: Current environment
            depth: Current recursion depth

        Returns:
            Function result or AIFPLTailCall for optimization
        """
        # Check arity
        if len(args) != len(func.parameters):
            raise AIFPLEvalError(
                f"Function expects {len(func.parameters)} arguments, got {len(args)}. "
                f"Parameters: {list(func.parameters)}"
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
            func_env = func_env.define(param, arg_value)
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

    def _is_recursive_call(self, func_value: AIFPLFunction, call_chain: List[AIFPLFunction]) -> bool:
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
        # Use object identity comparison since AIFPLFunction objects are unique
        for chain_func in call_chain:
            if func_value is chain_func:
                return True

        return False

    def _evaluate_with_tail_detection(
        self,
        expr: AIFPLSExpression,
        env: AIFPLEnvironment,
        depth: int,
        current_function: AIFPLFunction
    ) -> Union[AIFPLValue, AIFPLTailCall]:
        """
        Evaluate an expression with tail call detection.

        Args:
            expr: Expression to evaluate
            env: Environment
            depth: Current depth
            current_function: The function we're currently executing

        Returns:
            Either a regular result or a AIFPLTailCall object for optimization
        """
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # Handle if expressions specially - branches are in tail position
        if isinstance(expr, AIFPLFunctionCall) and isinstance(expr.function, AIFPLSymbol):
            func_name = expr.function.name
            if func_name == 'if':
                if len(expr.arguments) != 3:
                    raise AIFPLEvalError(f"if requires exactly 3 arguments, got {len(expr.arguments)}")

                condition_expr, then_expr, else_expr = expr.arguments

                # Evaluate condition (not in tail position)
                condition = self._evaluate_expression(condition_expr, env, depth + 1)

                if not isinstance(condition, AIFPLBoolean):
                    raise AIFPLEvalError(f"if requires boolean condition, got {condition.type_name()}")

                # Evaluate chosen branch (in tail position)
                if condition.value:
                    return self._evaluate_with_tail_detection(then_expr, env, depth + 1, current_function)

                return self._evaluate_with_tail_detection(else_expr, env, depth + 1, current_function)

        # Handle function calls - check for tail calls
        if isinstance(expr, AIFPLFunctionCall):
            # Evaluate the function
            func_value = self._evaluate_expression(expr.function, env, depth + 1)

            # If it's a lambda function, check for recursion (simple or mutual)
            if isinstance(func_value, AIFPLFunction):
                # Use the call chain we're tracking
                if self._is_recursive_call(func_value, self.call_chain):
                    # This is a recursive call (simple or mutual)!
                    return AIFPLTailCall(
                        function=expr.function,
                        arguments=expr.arguments,
                        environment=env
                    )

                # Don't fall back to regular recursion!
                return self._call_lambda_function(func_value, expr.arguments, env, depth + 1)

            # Built-in function, evaluate normally
            return self._evaluate_function_call(expr, env, depth + 1)

        # For other expressions, evaluate normally
        return self._evaluate_expression(expr, env, depth + 1)

    def _apply_builtin_operator(
        self,
        operator: str,
        args: List[AIFPLSExpression],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
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

        # For regular operators, evaluate arguments first
        try:
            evaluated_args = [self._evaluate_expression(arg, env, depth + 1) for arg in args]

        except AIFPLEvalError as e:
            raise AIFPLEvalError(f"Error evaluating arguments for '{operator}': {e}") from e

        # Check argument count
        self._validate_arity(operator, op_def, evaluated_args)

        # Handle special mixed return types (position function)
        if op_def.get('returns_boolean_or_value'):
            return self._apply_mixed_return_operator(operator, evaluated_args)

        # Handle operations that return booleans FIRST (before string_only check)
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
            if isinstance(arg, (AIFPLString, AIFPLBoolean, AIFPLList)):
                raise AIFPLEvalError(f"Operator '{operator}' cannot operate on {arg.type_name()} arguments")

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

    def _apply_if_conditional(
        self,
        args: List[AIFPLSExpression],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
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
        condition = self._evaluate_expression(condition_expr, env, depth + 1)

        # Validate condition is boolean
        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError(f"Operator 'if' requires boolean condition, got {condition.type_name()}")

        # Lazy evaluation: only evaluate the chosen branch
        if condition.value:
            return self._evaluate_expression(then_expr, env, depth + 1)

        return self._evaluate_expression(else_expr, env, depth + 1)

    def _apply_and_short_circuit(self, args: List[AIFPLSExpression], env: AIFPLEnvironment, depth: int) -> AIFPLBoolean:
        """Handle AND with short-circuit evaluation."""
        # Empty AND returns True (identity)
        if not args:
            return AIFPLBoolean(True)

        # Evaluate arguments one by one, short-circuiting on first False
        for arg in args:
            result = self._evaluate_expression(arg, env, depth + 1)

            # Validate that result is boolean
            if not isinstance(result, AIFPLBoolean):
                raise AIFPLEvalError(f"Operator 'and' requires boolean arguments, got {result.type_name()}")

            # Short-circuit: if any argument is False, return False immediately
            if not result.value:
                return AIFPLBoolean(False)

        # All arguments were True
        return AIFPLBoolean(True)

    def _apply_or_short_circuit(self, args: List[AIFPLSExpression], env: AIFPLEnvironment, depth: int) -> AIFPLBoolean:
        """Handle OR with short-circuit evaluation."""
        # Empty OR returns False (identity)
        if not args:
            return AIFPLBoolean(False)

        # Evaluate arguments one by one, short-circuiting on first True
        for arg in args:
            result = self._evaluate_expression(arg, env, depth + 1)

            # Validate that result is boolean
            if not isinstance(result, AIFPLBoolean):
                raise AIFPLEvalError(f"Operator 'or' requires boolean arguments, got {result.type_name()}")

            # Short-circuit: if any argument is True, return True immediately
            if result.value:
                return AIFPLBoolean(True)

        # All arguments were False
        return AIFPLBoolean(False)

    def _validate_arity(self, operator: str, op_def: Dict[str, Any], args: List[AIFPLValue]) -> None:
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

    # Mixed return operator
    def _apply_mixed_return_operator(self, operator: str, args: List[AIFPLValue]) -> AIFPLValue:
        """Apply operators that return mixed types (like position)."""
        if operator == 'position':
            if len(args) != 2:
                raise AIFPLEvalError(f"position requires exactly 2 arguments, got {len(args)}")

            item, list_arg = args
            list_val = self._ensure_list(list_arg, operator)

            pos = list_val.position(item)
            if pos is not None:
                return AIFPLNumber(pos)

            return AIFPLBoolean(False)

        raise AIFPLEvalError(f"Unknown mixed return operator: '{operator}'")

    def _apply_boolean_returning_operator(self, operator: str, args: List[AIFPLValue]) -> AIFPLBoolean:
        """Apply operators that return boolean values."""
        if operator in ('=', '!='):
            if len(args) < 2:
                raise AIFPLEvalError(f"Operator '{operator}' requires at least 2 arguments, got {len(args)}")

            if operator == '=':
                # All values must be equal
                first = args[0]
                return AIFPLBoolean(all(first == arg for arg in args[1:]))

            if operator == '!=':
                # Any values not equal
                for i, arg_i in enumerate(args):
                    for j in range(i + 1, len(args)):
                        if arg_i != args[j]:
                            return AIFPLBoolean(True)

                return AIFPLBoolean(False)

        if operator in ('<', '>', '<=', '>='):
            if len(args) < 2:
                raise AIFPLEvalError(f"Operator '{operator}' requires at least 2 arguments, got {len(args)}")

            # Ensure all arguments are numeric
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(f"Operator '{operator}' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            # Check comparison chain
            for i in range(len(args) - 1):
                left_val, right_val = args[i].value, args[i + 1].value

                if operator == '<' and not left_val < right_val:
                    return AIFPLBoolean(False)

                if operator == '>' and not left_val > right_val:
                    return AIFPLBoolean(False)

                if operator == '<=' and not left_val <= right_val:
                    return AIFPLBoolean(False)

                if operator == '>=' and not left_val >= right_val:
                    return AIFPLBoolean(False)

            return AIFPLBoolean(True)

        # Type predicates
        if operator == 'number?':
            return AIFPLBoolean(isinstance(args[0], AIFPLNumber))

        if operator == 'integer?':
            return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_integer())

        if operator == 'float?':
            return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_float())

        if operator == 'complex?':
            return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_complex())

        if operator == 'string?':
            return AIFPLBoolean(isinstance(args[0], AIFPLString))

        if operator == 'boolean?':
            return AIFPLBoolean(isinstance(args[0], AIFPLBoolean))

        if operator == 'list?':
            return AIFPLBoolean(isinstance(args[0], AIFPLList))

        if operator == 'function?':
            return AIFPLBoolean(isinstance(args[0], AIFPLFunction))

        # List predicates
        if operator == 'null?':
            list_val = self._ensure_list(args[0], operator)
            return AIFPLBoolean(list_val.is_empty())

        if operator == 'member?':
            if len(args) != 2:
                raise AIFPLEvalError(f"member? requires exactly 2 arguments, got {len(args)}")

            item, list_arg = args
            list_val = self._ensure_list(list_arg, operator)
            return AIFPLBoolean(list_val.contains(item))

        # String predicates - these are implemented in the mixin
        if operator in ('string-contains?', 'string-prefix?', 'string-suffix?', 'string=?'):
            return self._apply_string_operator(operator, self.OPERATORS[operator], args)

        raise AIFPLEvalError(f"Unknown boolean-returning operator: '{operator}'")

    def _apply_list_operator(self, operator: str, args: List[AIFPLValue]) -> AIFPLValue:
        """Apply list operations."""
        if operator == 'list':
            return AIFPLList(tuple(args))

        if operator == 'cons':
            if len(args) != 2:
                raise AIFPLEvalError(f"cons requires exactly 2 arguments, got {len(args)}")
            item, list_arg = args
            list_val = self._ensure_list(list_arg, operator)
            return list_val.cons(item)

        if operator == 'append':
            if len(args) < 2:
                raise AIFPLEvalError(f"append requires at least 2 arguments, got {len(args)}")

            # Validate all arguments are lists
            list_vals = [self._ensure_list(arg, operator) for arg in args]

            # Concatenate all lists
            result = list_vals[0]
            for list_val in list_vals[1:]:
                result = result.append_list(list_val)
            return result

        if operator == 'reverse':
            list_val = self._ensure_list(args[0], operator)
            return list_val.reverse()

        if operator == 'first':
            list_val = self._ensure_list(args[0], operator)
            try:
                return list_val.first()

            except IndexError as e:
                raise AIFPLEvalError(str(e)) from e

        if operator == 'rest':
            list_val = self._ensure_list(args[0], operator)
            try:
                return list_val.rest()

            except IndexError as e:
                raise AIFPLEvalError(str(e)) from e

        if operator == 'last':
            list_val = self._ensure_list(args[0], operator)
            try:
                return list_val.last()

            except IndexError as e:
                raise AIFPLEvalError(str(e)) from e

        if operator == 'list-ref':
            if len(args) != 2:
                raise AIFPLEvalError(f"list-ref requires exactly 2 arguments, got {len(args)}")

            list_val = self._ensure_list(args[0], operator)
            index = self._ensure_integer(args[1], operator)

            # Check for negative index (not allowed in AIFPL)
            if index < 0:
                raise AIFPLEvalError(f"list-ref index out of range: {index}")

            try:
                return list_val.get(index)

            except IndexError:
                raise AIFPLEvalError(f"list-ref index out of range: {index}")

        if operator == 'length':
            list_val = self._ensure_list(args[0], operator)
            return AIFPLNumber(list_val.length())

        if operator == 'remove':
            if len(args) != 2:
                raise AIFPLEvalError(f"remove requires exactly 2 arguments, got {len(args)}")

            item, list_arg = args
            list_val = self._ensure_list(list_arg, operator)
            return list_val.remove_all(item)

        if operator == 'take':
            if len(args) != 2:
                raise AIFPLEvalError(f"take requires exactly 2 arguments, got {len(args)}")

            n = self._ensure_integer(args[0], operator)
            list_val = self._ensure_list(args[1], operator)
            if n < 0:
                raise AIFPLEvalError(f"take count cannot be negative: {n}")

            return list_val.take(n)

        if operator == 'drop':
            if len(args) != 2:
                raise AIFPLEvalError(f"drop requires exactly 2 arguments, got {len(args)}")

            n = self._ensure_integer(args[0], operator)
            list_val = self._ensure_list(args[1], operator)
            if n < 0:
                raise AIFPLEvalError(f"drop count cannot be negative: {n}")

            return list_val.drop(n)

        raise AIFPLEvalError(f"Unknown list operator: '{operator}'")

    def _apply_boolean_operator(self, operator: str, args: List[AIFPLValue]) -> AIFPLValue:
        """Apply boolean operators."""
        if operator == 'not':
            if len(args) != 1:
                raise AIFPLEvalError(f"not requires exactly 1 argument, got {len(args)}")

            bool_val = self._ensure_boolean(args[0], operator)
            return AIFPLBoolean(not bool_val.value)

        raise AIFPLEvalError(f"Unknown boolean operator: '{operator}'")

    def _python_value_to_ast_node(self, value: AIFPLValue) -> AIFPLSExpression:
        """
        Convert an AIFPLValue to the appropriate AST node type for function calls.

        Args:
            value: AIFPLValue to convert

        Returns:
            Appropriate AST node
        """
        # AIFPLValue objects can be used directly as AST nodes in the new system
        return value

    def _apply_higher_order_function(
        self,
        operator: str,
        args: List[AIFPLSExpression],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """Apply higher-order functions like map, filter, fold."""
        if operator == 'map':
            if len(args) != 2:
                raise AIFPLEvalError(f"map requires exactly 2 arguments (function, list), got {len(args)}")

            func_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"map requires list as second argument, got {list_value.type_name()}")

            # Apply function to each element
            result_elements = []
            for item in list_value.elements:
                # Create a function call with the item as argument
                item_call = AIFPLFunctionCall(function=func_expr, arguments=[item], position=0)
                item_result = self._evaluate_function_call(item_call, env, depth + 1)
                result_elements.append(item_result)

            return AIFPLList(tuple(result_elements))

        if operator == 'filter':
            if len(args) != 2:
                raise AIFPLEvalError(f"filter requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"filter requires list as second argument, got {list_value.type_name()}")

            # Filter elements based on predicate
            result_elements = []
            for item in list_value.elements:
                # Create a function call with the item as argument
                pred_call = AIFPLFunctionCall(function=pred_expr, arguments=[item], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"filter predicate must return boolean, got {pred_result.type_name()}")


                if pred_result.value:
                    result_elements.append(item)

            return AIFPLList(tuple(result_elements))

        if operator == 'fold':
            if len(args) != 3:
                raise AIFPLEvalError(f"fold requires exactly 3 arguments (function, initial, list), got {len(args)}")

            func_expr, init_expr, list_expr = args

            # Evaluate initial value and list
            accumulator = self._evaluate_expression(init_expr, env, depth + 1)
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"fold requires list as third argument, got {list_value.type_name()}")

            # Fold over the list
            for item in list_value.elements:
                # Create a function call with accumulator and item as arguments
                fold_call = AIFPLFunctionCall(function=func_expr, arguments=[accumulator, item], position=0)
                accumulator = self._evaluate_function_call(fold_call, env, depth + 1)

            return accumulator

        if operator == 'range':
            # Check arity BEFORE evaluating arguments
            if len(args) < 2 or len(args) > 3:
                raise AIFPLEvalError(f"range requires 2 or 3 arguments (start, end[, step]), got {len(args)}")

            # Now evaluate arguments
            evaluated_args = [self._evaluate_expression(arg, env, depth + 1) for arg in args]

            if len(evaluated_args) == 2:
                start_val, end_val = evaluated_args
                if not isinstance(start_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 1 must be numeric, got {start_val.type_name()}")

                if not isinstance(end_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 2 must be numeric, got {end_val.type_name()}")

                start_int = int(start_val.value)
                end_int = int(end_val.value)
                step_int = 1

            else:
                start_val, end_val, step_val = evaluated_args
                if not isinstance(start_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 1 must be numeric, got {start_val.type_name()}")
                if not isinstance(end_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 2 must be numeric, got {end_val.type_name()}")

                if not isinstance(step_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 3 must be numeric, got {step_val.type_name()}")

                start_int = int(start_val.value)
                end_int = int(end_val.value)
                step_int = int(step_val.value)

            if step_int == 0:
                raise AIFPLEvalError("range step cannot be zero")

            # Generate range
            range_values = list(range(start_int, end_int, step_int))
            elements = tuple(AIFPLNumber(val) for val in range_values)
            return AIFPLList(elements)

        if operator == 'find':
            if len(args) != 2:
                raise AIFPLEvalError(f"find requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"find requires list as second argument, got {list_value.type_name()}")

            # Find first element matching predicate
            for item in list_value.elements:
                # Create a function call with the item as argument
                pred_call = AIFPLFunctionCall(function=pred_expr, arguments=[item], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"find predicate must return boolean, got {pred_result.type_name()}")

                if pred_result.value:
                    return item

            return AIFPLBoolean(False)  # Return #f if not found

        if operator == 'any?':
            if len(args) != 2:
                raise AIFPLEvalError(f"any? requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"any? requires list as second argument, got {list_value.type_name()}")

            # Check if any element matches predicate
            for item in list_value.elements:
                # Create a function call with the item as argument
                pred_call = AIFPLFunctionCall(function=pred_expr, arguments=[item], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"any? predicate must return boolean, got {pred_result.type_name()}")

                if pred_result.value:
                    return AIFPLBoolean(True)

            return AIFPLBoolean(False)

        if operator == 'all?':
            if len(args) != 2:
                raise AIFPLEvalError(f"all? requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"all? requires list as second argument, got {list_value.type_name()}")

            # Check if all elements match predicate
            for item in list_value.elements:
                # Create a function call with the item as argument
                pred_call = AIFPLFunctionCall(function=pred_expr, arguments=[item], position=0)
                pred_result = self._evaluate_function_call(pred_call, env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"all? predicate must return boolean, got {pred_result.type_name()}")

                if not pred_result.value:
                    return AIFPLBoolean(False)

            return AIFPLBoolean(True)

        raise AIFPLEvalError(f"Higher-order function '{operator}' not yet implemented")

    def simplify_result(self, result: AIFPLValue) -> AIFPLValue:
        """Simplify complex results to real numbers when imaginary part is negligible."""
        if isinstance(result, AIFPLNumber) and isinstance(result.value, complex):
            # If imaginary part is effectively zero, return just the real part
            if abs(result.value.imag) < self.imaginary_tolerance:
                real_part = result.value.real
                # Convert to int if it's a whole number
                if isinstance(real_part, float) and real_part.is_integer():
                    return AIFPLNumber(int(real_part))

                return AIFPLNumber(real_part)

        # For real numbers, convert float to int if it's a whole number
        if isinstance(result, AIFPLNumber) and isinstance(result.value, float) and result.value.is_integer():
            return AIFPLNumber(int(result.value))

        return result

    def format_result(self, result: AIFPLValue) -> str:
        """
        Format result for display, using LISP conventions for lists and booleans.

        Args:
            result: The result to format

        Returns:
            String representation of the result
        """
        if isinstance(result, AIFPLBoolean):
            return "#t" if result.value else "#f"

        if isinstance(result, AIFPLString):
            escaped_content = self._escape_string_for_lisp(result.value)
            return f'"{escaped_content}"'

        if isinstance(result, AIFPLNumber):
            if isinstance(result.value, float):
                nice_number = self._is_close_to_nice_number(result.value)
                if nice_number is not None:
                    # If it's close to an integer, show as integer
                    if nice_number == int(nice_number):
                        return str(int(nice_number))

                    return str(nice_number)

            return str(result.value)

        if isinstance(result, AIFPLList):
            # Format list in LISP notation: (element1 element2 ...)
            if result.is_empty():
                return "()"

            formatted_elements = []
            for element in result.elements:
                formatted_elements.append(self.format_result(element))

            return f"({' '.join(formatted_elements)})"

        if isinstance(result, AIFPLFunction):
            # Format lambda functions
            param_str = " ".join(result.parameters)
            return f"<lambda ({param_str})>"

        # For other types, use standard string representation
        return str(result)

    def _escape_string_for_lisp(self, s: str) -> str:
        """Escape a string for LISP display format."""
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

    def _is_close_to_nice_number(self, value: float) -> Union[float, None]:
        """Check if a float is very close to a 'nice' number and return the nice number if so."""
        # Check if it's close to common fractions with small denominators
        for denominator in range(1, 11):  # Check denominators 1-10
            for numerator in range(-50, 51):  # Check reasonable range
                nice_value = numerator / denominator
                if abs(value - nice_value) < self.imaginary_tolerance:
                    return nice_value

        return None
