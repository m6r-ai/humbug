"""Evaluator for AIFPL Abstract Syntax Trees using pure list representation."""

import cmath
from dataclasses import dataclass
import math
from typing import List, Union

from aifpl.aifpl_call_stack import AIFPLCallStack
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol,
    AIFPLList, AIFPLRecursivePlaceholder, AIFPLFunction, AIFPLBuiltinFunction
)
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer, AIFPLBindingGroup


@dataclass(frozen=True)
class AIFPLTailCall:
    """Represents a tail call to be optimized."""
    function: AIFPLValue
    arguments: List[AIFPLValue]
    environment: AIFPLEnvironment


class AIFPLEvaluator:
    """Evaluates AIFPL Abstract Syntax Trees using pure list representation."""

    # Mathematical constants
    CONSTANTS = {
        'pi': AIFPLNumber(math.pi),
        'e': AIFPLNumber(math.e),
        'j': AIFPLNumber(1j),
        'true': AIFPLBoolean(True),
        'false': AIFPLBoolean(False),
    }

    # List of all builtin functions for environment setup
    BUILTIN_FUNCTION_NAMES = [
        # Special forms (handled separately)
        'if', 'and', 'or',

        # Arithmetic functions
        '+', '-', '*', '/', '//', '%', '**',

        # Comparison functions
        '=', '!=', '<', '>', '<=', '>=',

        # Boolean functions
        'not',

        # Bitwise functions
        'bit-or', 'bit-and', 'bit-xor', 'bit-not', 'bit-shift-left', 'bit-shift-right',

        # Mathematical functions
        'sin', 'cos', 'tan', 'log', 'log10', 'exp', 'sqrt', 'abs', 'round', 'floor', 'ceil',
        'min', 'max', 'pow',

        # Base conversion functions
        'bin', 'hex', 'oct',

        # Complex number functions
        'real', 'imag', 'complex',

        # String functions
        'string-append', 'string-length', 'substring', 'string-upcase', 'string-downcase',
        'string-ref', 'string->number', 'number->string', 'string-trim', 'string-replace',

        # String predicates
        'string-contains?', 'string-prefix?', 'string-suffix?', 'string=?',

        # List construction and manipulation functions
        'list', 'cons', 'append', 'reverse',

        # List access and property functions
        'first', 'rest', 'last', 'list-ref', 'length',

        # List predicates
        'null?', 'list?', 'member?',

        # List utilities
        'remove', 'position',

        # String-list conversion functions
        'string->list', 'list->string', 'string-split', 'string-join',

        # Type predicates
        'number?', 'integer?', 'float?', 'complex?', 'string?', 'boolean?', 'function?',

        # Functional iterators
        'map', 'filter', 'fold', 'range', 'find', 'any?', 'all?', 'take', 'drop',
    ]

    def __init__(self, max_depth: int = 100, floating_point_tolerance: float = 1e-10):
        """
        Initialize evaluator.

        Args:
            max_depth: Maximum recursion depth
            floating_point_tolerance: Tolerance for floating point comparisons and simplifications
        """
        self.max_depth = max_depth
        self.floating_point_tolerance = floating_point_tolerance
        self.call_stack = AIFPLCallStack()

        # Add call chain tracking for mutual recursion detection
        self.call_chain: List[AIFPLFunction] = []

    def evaluate(
        self,
        expr: AIFPLValue,
        env: AIFPLEnvironment | None = None,
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

            # Add built-in functions to global environment.  This allows symbol lookup to succeed, and they
            # can be used in higher-order contexts
            for builtin_function_name in self.BUILTIN_FUNCTION_NAMES:
                env = env.define(builtin_function_name, AIFPLBuiltinFunction(builtin_function_name))

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
        expr: AIFPLValue,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """Internal expression evaluation with type dispatch."""

        # Check depth limit at the start of every expression evaluation
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # AIFPLValue evaluation - handle all value types that should self-evaluate
        if isinstance(expr, (AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLFunction, AIFPLBuiltinFunction)):
            # Self-evaluating values
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

        # List evaluation - check for special forms FIRST before any symbol evaluation
        if isinstance(expr, AIFPLList):
            # Empty list evaluates to itself
            if expr.is_empty():
                return expr

            # Non-empty list - check first element for special forms
            first_elem = expr.first()
            if isinstance(first_elem, AIFPLSymbol):
                # Handle special forms BEFORE attempting any symbol lookup
                if first_elem.name == "lambda":
                    return self._evaluate_lambda_form(expr, env, depth + 1)

                if first_elem.name == "let":
                    return self._evaluate_let_form(expr, env, depth + 1)

                # Regular function call (including built-ins and user functions)
                return self._evaluate_function_call(expr, env, depth + 1)

            # First element is not a symbol - evaluate as function call anyway
            return self._evaluate_function_call(expr, env, depth + 1)

        raise AIFPLEvalError(f"Invalid expression type: {type(expr).__name__}")

    def _evaluate_if_form(
        self,
        if_list: AIFPLList,
        env: AIFPLEnvironment,
        depth: int,
    ) -> AIFPLValue | AIFPLTailCall:
        """
        Evaluate (if condition then else) form.

        Args:
            if_list: List representing if expression
            env: Current environment
            depth: Current recursion depth
        Returns:
            Result of evaluating the if expression
        """
        if if_list.length() != 4:
            raise AIFPLEvalError(f"if requires exactly 3 arguments, got {if_list.length() - 1}")

        condition_expr = if_list.get(1)
        then_expr = if_list.get(2)
        else_expr = if_list.get(3)

        # Evaluate condition (not in tail position)
        condition = self._evaluate_expression(condition_expr, env, depth + 1)

        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError(f"if requires boolean condition, got {condition.type_name()}")

        # Evaluate chosen branch (in tail position)
        if condition.value:
            return self._evaluate_expression_with_tail_detection(then_expr, env, depth + 1)

        return self._evaluate_expression_with_tail_detection(else_expr, env, depth + 1)

    def _evaluate_lambda_form(
        self,
        lambda_list: AIFPLList,
        env: AIFPLEnvironment,
        _depth: int
    ) -> AIFPLFunction:
        """
        Evaluate (lambda (param1 param2 ...) body) form.

        Args:
            lambda_list: List representing lambda expression
            env: Current environment
            _depth: Current recursion depth

        Returns:
            AIFPLFunction object
        """
        if lambda_list.length() != 3:
            raise AIFPLEvalError(
                f"Lambda expression requires exactly 3 elements: (lambda (params...) body), got {lambda_list.length()}"
            )

        # Extract parameter list
        param_expr = lambda_list.get(1)

        # Extract parameters and ensure they're all symbols
        raw_parameters: List[AIFPLValue] = []

        if isinstance(param_expr, AIFPLList):
            # (param1 param2 ...) or ()
            raw_parameters = list(param_expr.elements)

        else:
            # Single parameter without parentheses (not standard but handle gracefully)
            if not isinstance(param_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    f"Lambda parameter list must be a list or symbol, got {type(param_expr).__name__}"
                )

            raw_parameters = [param_expr]

        # Validate parameters are all symbols and convert them
        parameters: List[str] = []
        for param in raw_parameters:
            if not isinstance(param, AIFPLSymbol):
                raise AIFPLEvalError(f"Lambda parameter must be a symbol, got {type(param).__name__}")

            parameters.append(param.name)

        # Check for duplicate parameters
        if len(parameters) != len(set(parameters)):
            duplicates = [p for p in parameters if parameters.count(p) > 1]
            raise AIFPLEvalError(f"Duplicate lambda parameters: {duplicates}")

        body = lambda_list.get(2)

        return AIFPLFunction(
            parameters=tuple(parameters),
            body=body,
            closure_environment=env,
            name="<lambda>"
        )

    def _evaluate_let_form(
        self,
        let_list: AIFPLList,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Evaluate (let ((var1 val1) (var2 val2) ...) body) form.

        Args:
            let_list: List representing let expression
            env: Current environment
            depth: Current recursion depth

        Returns:
            Result of evaluating the let body
        """
        if let_list.length() != 3:
            raise AIFPLEvalError(f"Let expression requires exactly 3 elements: (let ((bindings...)) body), got {let_list.length()}")

        # Parse binding list
        binding_expr = let_list.get(1)

        if not isinstance(binding_expr, AIFPLList):
            raise AIFPLEvalError(f"Let binding list must be a list, got {type(binding_expr).__name__}")

        bindings = []
        for binding in binding_expr.elements:
            if not isinstance(binding, AIFPLList) or binding.length() != 2:
                raise AIFPLEvalError("Let binding must be a list of 2 elements: (var value)")

            var_name_expr = binding.get(0)
            var_value_expr = binding.get(1)

            if not isinstance(var_name_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    f"Let binding variable must be a symbol, got {type(var_name_expr).__name__}"
                )

            bindings.append((var_name_expr.name, var_value_expr))

        # Check for duplicate binding names
        var_names = [name for name, _ in bindings]
        if len(var_names) != len(set(var_names)):
            duplicates = [name for name in var_names if var_names.count(name) > 1]
            raise AIFPLEvalError(f"Duplicate let binding variables: {duplicates}")

        body = let_list.get(2)

        # Analyze dependencies
        analyzer = AIFPLDependencyAnalyzer()
        binding_groups = analyzer.analyze_let_bindings(bindings)

        # Evaluate groups in order
        current_env = AIFPLEnvironment(bindings={}, parent=env, name="let")
        for group in binding_groups:
            if group.is_recursive:
                current_env = self._evaluate_recursive_binding_group(group, current_env, depth)

            else:
                current_env = self._evaluate_sequential_binding_group(group, current_env, depth)

        # Evaluate body in the final environment
        return self._evaluate_expression(body, current_env, depth)

    def _evaluate_sequential_binding_group(
        self,
        group: AIFPLBindingGroup,
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
        group: AIFPLBindingGroup,
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
        func_list: AIFPLList,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Evaluate function call with tail call optimization.

        Args:
            func_list: List representing function call
            env: Current environment
            depth: Current recursion depth

        Returns:
            Result of the function call
        """
        current_call = func_list
        current_env = env

        while True:
            if current_call.is_empty():
                raise AIFPLEvalError("Cannot call empty list")

            func_expr = current_call.first()
            arg_exprs = list(current_call.elements[1:])

            # Evaluate the function expression
            try:
                func_value = self._evaluate_expression(func_expr, current_env, depth)

            except AIFPLEvalError as e:
                if "Undefined variable" in str(e) and isinstance(func_expr, AIFPLSymbol):
                    raise AIFPLEvalError(f"Unknown function: '{func_expr.name}'") from e

                raise AIFPLEvalError(f"Error evaluating function expression: {e}") from e

            # Handle different types of functions
            if isinstance(func_value, AIFPLFunction):
                try:
                    arg_values = [self._evaluate_expression(arg, current_env, depth) for arg in arg_exprs]

                except AIFPLEvalError as e:
                    raise AIFPLEvalError(f"Error evaluating function arguments: {e}") from e

                result = self._call_lambda_function(func_value, arg_values, current_env, depth)

                # Check if result is a tail call
                if isinstance(result, AIFPLTailCall):
                    # Continue the loop with the tail call
                    current_call = AIFPLList((result.function,) + tuple(result.arguments))
                    current_env = result.environment
                    continue

                # Regular result, return it
                return result

            # Built-in function
            if isinstance(func_value, AIFPLBuiltinFunction) and isinstance(func_expr, AIFPLSymbol):
                function_name = func_expr.name

                # Handle special forms that require lazy evaluation
                if function_name in ['if', 'and', 'or']:
                    if function_name == 'if':
                        return self._apply_if_conditional(arg_exprs, current_env, depth)

                    if function_name == 'and':
                        return self._apply_and_short_circuit(arg_exprs, current_env, depth)

                    if function_name == 'or':
                        return self._apply_or_short_circuit(arg_exprs, current_env, depth)

                # Handle higher-order functions (map, filter, fold, etc.)
                if function_name in ['map', 'filter', 'fold', 'range', 'find', 'any?', 'all?']:
                    return self._apply_higher_order_function(function_name, arg_exprs, current_env, depth)

                # For regular functions, evaluate arguments first
                try:
                    evaluated_args = [self._evaluate_expression(arg, current_env, depth + 1) for arg in arg_exprs]

                except AIFPLEvalError as e:
                    raise AIFPLEvalError(f"Error evaluating arguments for '{function_name}': {e}") from e

                # Call the specific builtin function
                return self._call_builtin_function(function_name, evaluated_args, current_env, depth)

            raise AIFPLEvalError(f"Cannot call non-function value: {func_value.type_name()}")

    def _call_lambda_function(
        self,
        func: AIFPLFunction,
        arg_values: List[AIFPLValue],
        _env: AIFPLEnvironment,
        depth: int
    ) -> Union[AIFPLValue, AIFPLTailCall]:
        """
        Common logic for calling a lambda function with evaluated argument values.

        Args:
            func: Lambda function to call
            arg_values: Already-evaluated argument values
            env: Current environment (used for closure environment context)
            depth: Current recursion depth

        Returns:
            Function result or AIFPLTailCall for optimization
        """
        # Check arity
        if len(arg_values) != len(func.parameters):
            raise AIFPLEvalError(
                f"Function expects {len(func.parameters)} arguments, got {len(arg_values)}. "
                f"Parameters: {list(func.parameters)}"
            )

        # Create new environment for function execution
        func_env = AIFPLEnvironment(bindings={}, parent=func.closure_environment, name=f"{func.name}-call")

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
            return self._evaluate_expression_with_tail_detection(func.body, func_env, depth)

        finally:
            # Always pop the call frame and remove from call chain
            self.call_stack.pop()

            # Remove function from call chain
            if self.call_chain and self.call_chain[-1] is func:
                self.call_chain.pop()

    def _call_builtin_function(
        self,
        function_name: str,
        arg_values: List[AIFPLValue],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Dispatch to specific builtin function implementation.

        Args:
            function_name: Function name
            arg_values: Already-evaluated argument values
            env: Current environment
            depth: Current recursion depth

        Returns:
            Operation result
        """
        # Create comprehensive mapping of function names to method names
        method_name_map = {
            # Arithmetic operators
            '+': '_builtin_plus',
            '-': '_builtin_minus',
            '*': '_builtin_star',
            '/': '_builtin_slash',
            '//': '_builtin_slash_slash',
            '%': '_builtin_percent',
            '**': '_builtin_star_star',

            # Comparison operators
            '=': '_builtin_eq',
            '!=': '_builtin_bang_eq',
            '<': '_builtin_lt',
            '>': '_builtin_gt',
            '<=': '_builtin_lte',
            '>=': '_builtin_gte',

            # String conversion functions
            'string->number': '_builtin_string_to_number',
            'number->string': '_builtin_number_to_string',
            'string->list': '_builtin_string_to_list',
            'list->string': '_builtin_list_to_string',

            # String predicates
            'string-contains?': '_builtin_string_contains_p',
            'string-prefix?': '_builtin_string_prefix_p',
            'string-suffix?': '_builtin_string_suffix_p',
            'string=?': '_builtin_string_eq_p',

            # List predicates
            'null?': '_builtin_null_p',
            'list?': '_builtin_list_p',
            'member?': '_builtin_member_p',

            # Type predicates
            'number?': '_builtin_number_p',
            'integer?': '_builtin_integer_p',
            'float?': '_builtin_float_p',
            'complex?': '_builtin_complex_p',
            'string?': '_builtin_string_p',
            'boolean?': '_builtin_boolean_p',
            'function?': '_builtin_function_p',

            # Higher-order predicates
            'any?': '_builtin_any_p',
            'all?': '_builtin_all_p',
        }

        # Check for direct mapping first
        if function_name in method_name_map:
            method_name = method_name_map[function_name]

        else:
            # Convert function name to method name using standard rules
            method_name = f"_builtin_{function_name.replace('-', '_').replace('?', '_p').replace('!', '_bang')}"

        if hasattr(self, method_name):
            method = getattr(self, method_name)
            return method(arg_values, env, depth)

        raise AIFPLEvalError(f"Unknown function: '{function_name}'")

    def _call_function_with_evaluated_args(
        self,
        func_expr: AIFPLValue,
        arg_values: List[AIFPLValue],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Call a function with already-evaluated arguments.

        This is used by higher-order functions where arguments are already AIFPLValue objects.

        Args:
            func_expr: Function expression (unevaluated)
            arg_values: Already-evaluated argument values
            env: Current environment
            depth: Current recursion depth

        Returns:
            Function result
        """
        # Evaluate the function expression
        func_value = self._evaluate_expression(func_expr, env, depth)

        # Handle different types of functions
        if isinstance(func_value, AIFPLFunction):
            # User-defined function call with pre-evaluated arguments
            result = self._call_lambda_function(func_value, arg_values, env, depth)

            # For higher-order functions, we don't want tail call optimization
            if isinstance(result, AIFPLTailCall):
                # This shouldn't happen in higher-order contexts, but handle it gracefully
                raise AIFPLEvalError("Unexpected tail call in higher-order function context")

            return result

        # Built-in function with pre-evaluated arguments
        if isinstance(func_value, AIFPLBuiltinFunction):
            return self._call_builtin_function(func_value.name, arg_values, env, depth)

        raise AIFPLEvalError(f"Cannot call non-function value: {func_value.type_name()}")

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

    def _evaluate_expression_with_tail_detection(
        self,
        expr: AIFPLValue,
        env: AIFPLEnvironment,
        depth: int,
    ) -> Union[AIFPLValue, AIFPLTailCall]:
        """
        Evaluate an expression with tail call detection.

        Args:
            expr: Expression to evaluate
            env: Environment
            depth: Current depth

        Returns:
            Either a regular result or a AIFPLTailCall object for optimization
        """
        if depth > self.max_depth:
            stack_trace = self.call_stack.format_stack_trace()
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})\nCall stack:\n{stack_trace}")

        # If this isn't a list, evaluate normally
        if not isinstance(expr, AIFPLList):
            return self._evaluate_expression(expr, env, depth + 1)

        # If this list is empty, evaluate normally
        if expr.is_empty():
            return self._evaluate_expression(expr, env, depth + 1)

        first_elem = expr.first()
        if isinstance(first_elem, AIFPLSymbol):
            # Handle if expressions specially - branches are in tail position
            if first_elem.name == 'if':
                return self._evaluate_if_form(expr, env, depth + 1)

            # Handle lambda expressions - they are NOT tail calls, just return the function
            if first_elem.name == 'lambda':
                return self._evaluate_lambda_form(expr, env, depth + 1)

            # Handle let expressions - body is in tail position
            if first_elem.name == 'let':
                return self._evaluate_let_form(expr, env, depth + 1)

        # Check for tail calls
        func_value = self._evaluate_expression(first_elem, env, depth + 1)

        # If it's not a lambda function, evaluate normally
        if not isinstance(func_value, AIFPLFunction):
            return self._evaluate_function_call(expr, env, depth + 1)

        # Check for recursion (simple or mutual)
        if not self._is_recursive_call(func_value, self.call_chain):
            return self._evaluate_function_call(expr, env, depth + 1)

        # This is a recursive call (simple or mutual)!
        arg_exprs = list(expr.elements[1:])
        return AIFPLTailCall(
            function=first_elem,
            arguments=arg_exprs,
            environment=env
        )

    def _apply_if_conditional(
        self,
        args: List[AIFPLValue],
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

    def _apply_and_short_circuit(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLBoolean:
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

    def _apply_or_short_circuit(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLBoolean:
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

    def _apply_higher_order_function(
        self,
        function_name: str,
        args: List[AIFPLValue],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """Apply higher-order functions like map, filter, fold."""
        if function_name == 'map':
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
                # Call function with already-evaluated argument
                item_result = self._call_function_with_evaluated_args(func_expr, [item], env, depth + 1)
                result_elements.append(item_result)

            return AIFPLList(tuple(result_elements))

        if function_name == 'filter':
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
                # Call predicate with already-evaluated argument
                pred_result = self._call_function_with_evaluated_args(pred_expr, [item], env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"filter predicate must return boolean, got {pred_result.type_name()}")

                if pred_result.value:
                    result_elements.append(item)

            return AIFPLList(tuple(result_elements))

        if function_name == 'fold':
            if len(args) != 3:
                raise AIFPLEvalError(f"fold requires exactly 3 arguments (function, initial, list), got {len(args)}")

            func_expr, init_expr, list_expr = args

            # Evaluate the initial value and list
            accumulator = self._evaluate_expression(init_expr, env, depth + 1)
            list_value = self._evaluate_expression(list_expr, env, depth + 1)

            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"fold requires list as third argument, got {list_value.type_name()}")

            # Fold over the list
            for item in list_value.elements:
                # Call function with already-evaluated arguments
                accumulator = self._call_function_with_evaluated_args(func_expr, [accumulator, item], env, depth + 1)

            return accumulator

        if function_name == 'range':
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

                start_int = self._ensure_integer(start_val, "range")
                end_int = self._ensure_integer(end_val, "range")
                step_int = 1

            else:
                start_val, end_val, step_val = evaluated_args
                if not isinstance(start_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 1 must be numeric, got {start_val.type_name()}")

                if not isinstance(end_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 2 must be numeric, got {end_val.type_name()}")

                if not isinstance(step_val, AIFPLNumber):
                    raise AIFPLEvalError(f"range argument 3 must be numeric, got {step_val.type_name()}")

                start_int = self._ensure_integer(start_val, "range")
                end_int = self._ensure_integer(end_val, "range")
                step_int = self._ensure_integer(step_val, "range")

            if step_int == 0:
                raise AIFPLEvalError("range step cannot be zero")

            # Generate range
            range_values = list(range(start_int, end_int, step_int))
            elements = tuple(AIFPLNumber(val) for val in range_values)
            return AIFPLList(elements)

        if function_name == 'find':
            if len(args) != 2:
                raise AIFPLEvalError(f"find requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"find requires list as second argument, got {list_value.type_name()}")

            # Find first element matching predicate
            for item in list_value.elements:
                # Call predicate with already-evaluated argument
                pred_result = self._call_function_with_evaluated_args(pred_expr, [item], env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"find predicate must return boolean, got {pred_result.type_name()}")

                if pred_result.value:
                    return item

            return AIFPLBoolean(False)  # Return #f if not found

        if function_name == 'any?':
            if len(args) != 2:
                raise AIFPLEvalError(f"any? requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"any? requires list as second argument, got {list_value.type_name()}")

            # Check if any element matches predicate
            for item in list_value.elements:
                # Call predicate with already-evaluated argument
                pred_result = self._call_function_with_evaluated_args(pred_expr, [item], env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"any? predicate must return boolean, got {pred_result.type_name()}")

                if pred_result.value:
                    return AIFPLBoolean(True)

            return AIFPLBoolean(False)

        if function_name == 'all?':
            if len(args) != 2:
                raise AIFPLEvalError(f"all? requires exactly 2 arguments (predicate, list), got {len(args)}")

            pred_expr, list_expr = args

            # Evaluate the list
            list_value = self._evaluate_expression(list_expr, env, depth + 1)
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(f"all? requires list as second argument, got {list_value.type_name()}")

            # Check if all elements match predicate
            for item in list_value.elements:
                # Call predicate with already-evaluated argument
                pred_result = self._call_function_with_evaluated_args(pred_expr, [item], env, depth + 1)

                if not isinstance(pred_result, AIFPLBoolean):
                    raise AIFPLEvalError(f"all? predicate must return boolean, got {pred_result.type_name()}")

                if not pred_result.value:
                    return AIFPLBoolean(False)

            return AIFPLBoolean(True)

        raise AIFPLEvalError(f"Higher-order function '{function_name}' not yet implemented")

    def _builtin_plus(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement + operation."""
        if not args:
            return AIFPLNumber(0)

        # Ensure all args are numbers
        num_args = [self._ensure_number(arg, "+") for arg in args]
        result = sum(arg.value for arg in num_args)
        return AIFPLNumber(result)

    def _builtin_minus(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement - operation."""
        if len(args) == 0:
            raise AIFPLEvalError("Function '-' requires at least 1 argument, got 0")

        num_args = [self._ensure_number(arg, "-") for arg in args]

        if len(args) == 1:
            return AIFPLNumber(-num_args[0].value)

        result = num_args[0].value
        for arg in num_args[1:]:
            result -= arg.value

        return AIFPLNumber(result)

    def _builtin_star(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement * operation."""
        if not args:
            return AIFPLNumber(1)

        num_args = [self._ensure_number(arg, "*") for arg in args]
        result = num_args[0].value
        for arg in num_args[1:]:
            result *= arg.value

        return AIFPLNumber(result)

    def _builtin_slash(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement / operation."""
        if len(args) < 2:
            raise AIFPLEvalError("Function '/' requires at least 2 arguments, got " + str(len(args)))

        num_args = [self._ensure_number(arg, "/") for arg in args]

        # Check for division by zero
        for i, arg in enumerate(num_args[1:], 1):
            if arg.value == 0:
                raise AIFPLEvalError(f"Division by zero at argument {i+1}")

        result = num_args[0].value
        for arg in num_args[1:]:
            result /= arg.value

        return AIFPLNumber(result)

    def _builtin_slash_slash(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement // (floor division) operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Floor division takes exactly 2 arguments, got {len(args)}")

        left_val = self._ensure_real_number(args[0], "//")
        right_val = self._ensure_real_number(args[1], "//")

        if right_val == 0:
            raise AIFPLEvalError("Division by zero")

        return AIFPLNumber(left_val // right_val)

    def _builtin_percent(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement % (modulo) operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Modulo takes exactly 2 arguments, got {len(args)}")

        left_val = self._ensure_real_number(args[0], "%")
        right_val = self._ensure_real_number(args[1], "%")

        if right_val == 0:
            raise AIFPLEvalError("Modulo by zero")

        return AIFPLNumber(left_val % right_val)

    def _builtin_star_star(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement ** (exponentiation) operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"Power takes exactly 2 arguments, got {len(args)}")

        base = self._ensure_number(args[0], "**")
        exponent = self._ensure_number(args[1], "**")
        return AIFPLNumber(base.value ** exponent.value)

    def _builtin_eq(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement = (equality) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '=' requires at least 2 arguments, got {len(args)}")

        # All values must be equal
        first = args[0]
        return AIFPLBoolean(all(first == arg for arg in args[1:]))

    def _builtin_bang_eq(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement != (inequality) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '!=' requires at least 2 arguments, got {len(args)}")

        # Any values not equal
        for i, arg_i in enumerate(args):
            for j in range(i + 1, len(args)):
                if arg_i != args[j]:
                    return AIFPLBoolean(True)

        return AIFPLBoolean(False)

    def _builtin_lt(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement < (less than) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '<' requires at least 2 arguments, got {len(args)}")

        # Ensure all arguments are numeric and real (not complex)
        for i, arg in enumerate(args):
            if not isinstance(arg, AIFPLNumber):
                raise AIFPLEvalError(f"Function '<' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            if isinstance(arg.value, complex):
                raise AIFPLEvalError("Function '<' does not support complex numbers")

        # Check comparison chain
        for i in range(len(args) - 1):
            left_arg, right_arg = args[i], args[i + 1]
            assert isinstance(left_arg, AIFPLNumber) and isinstance(right_arg, AIFPLNumber)
            left_val = left_arg.value
            right_val = right_arg.value
            assert not isinstance(left_val, complex) and not isinstance(right_val, complex)

            if not left_val < right_val:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_gt(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement > (greater than) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '>' requires at least 2 arguments, got {len(args)}")

        # Ensure all arguments are numeric and real (not complex)
        for i, arg in enumerate(args):
            if not isinstance(arg, AIFPLNumber):
                raise AIFPLEvalError(f"Function '>' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            if isinstance(arg.value, complex):
                raise AIFPLEvalError("Function '>' does not support complex numbers")

        # Check comparison chain
        for i in range(len(args) - 1):
            left_arg, right_arg = args[i], args[i + 1]
            assert isinstance(left_arg, AIFPLNumber) and isinstance(right_arg, AIFPLNumber)
            left_val = left_arg.value
            right_val = right_arg.value
            assert not isinstance(left_val, complex) and not isinstance(right_val, complex)

            if not left_val > right_val:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_lte(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement <= (less than or equal) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '<=' requires at least 2 arguments, got {len(args)}")

        # Ensure all arguments are numeric and real (not complex)
        for i, arg in enumerate(args):
            if not isinstance(arg, AIFPLNumber):
                raise AIFPLEvalError(f"Function '<=' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            if isinstance(arg.value, complex):
                raise AIFPLEvalError("Function '<=' does not support complex numbers")

        # Check comparison chain
        for i in range(len(args) - 1):
            left_arg, right_arg = args[i], args[i + 1]
            assert isinstance(left_arg, AIFPLNumber) and isinstance(right_arg, AIFPLNumber)
            left_val = left_arg.value
            right_val = right_arg.value
            assert not isinstance(left_val, complex) and not isinstance(right_val, complex)

            if not left_val <= right_val:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_gte(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement >= (greater than or equal) operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"Function '>=' requires at least 2 arguments, got {len(args)}")

        # Ensure all arguments are numeric and real (not complex)
        for i, arg in enumerate(args):
            if not isinstance(arg, AIFPLNumber):
                raise AIFPLEvalError(f"Function '>=' requires numeric arguments, argument {i+1} is {arg.type_name()}")

            if isinstance(arg.value, complex):
                raise AIFPLEvalError("Function '>=' does not support complex numbers")

        # Check comparison chain
        for i in range(len(args) - 1):
            left_arg, right_arg = args[i], args[i + 1]
            assert isinstance(left_arg, AIFPLNumber) and isinstance(right_arg, AIFPLNumber)
            left_val = left_arg.value
            right_val = right_arg.value
            assert not isinstance(left_val, complex) and not isinstance(right_val, complex)

            if not left_val >= right_val:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _builtin_not(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement not operation."""
        if len(args) != 1:
            raise AIFPLEvalError(f"not takes exactly 1 argument, got {len(args)}")

        bool_val = self._ensure_boolean(args[0], "not")
        return AIFPLBoolean(not bool_val.value)

    def _builtin_bit_or(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement bit-or operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"bit-or requires at least 2 arguments, got {len(args)}")

        int_args = [self._ensure_integer(arg, "bit-or") for arg in args]
        result = int_args[0]
        for arg in int_args[1:]:
            result |= arg

        return AIFPLNumber(result)

    def _builtin_bit_and(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement bit-and operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"bit-and requires at least 2 arguments, got {len(args)}")

        int_args = [self._ensure_integer(arg, "bit-and") for arg in args]
        result = int_args[0]
        for arg in int_args[1:]:
            result &= arg

        return AIFPLNumber(result)

    def _builtin_bit_xor(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement bit-xor operation."""
        if len(args) < 2:
            raise AIFPLEvalError(f"bit-xor requires at least 2 arguments, got {len(args)}")

        int_args = [self._ensure_integer(arg, "bit-xor") for arg in args]
        result = int_args[0]
        for arg in int_args[1:]:
            result ^= arg

        return AIFPLNumber(result)

    def _builtin_bit_not(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement bit-not operation."""
        if len(args) != 1:
            raise AIFPLEvalError(f"bit-not takes exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "bit-not")
        return AIFPLNumber(~int_val)

    def _builtin_bit_shift_left(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement bit-shift-left operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"bit-shift-left takes exactly 2 arguments, got {len(args)}")

        value = self._ensure_integer(args[0], "bit-shift-left")
        shift = self._ensure_integer(args[1], "bit-shift-left")
        return AIFPLNumber(value << shift)

    def _builtin_bit_shift_right(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement bit-shift-right operation."""
        if len(args) != 2:
            raise AIFPLEvalError(f"bit-shift-right takes exactly 2 arguments, got {len(args)}")

        value = self._ensure_integer(args[0], "bit-shift-right")
        shift = self._ensure_integer(args[1], "bit-shift-right")
        return AIFPLNumber(value >> shift)

    def _builtin_sin(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement sin function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"sin takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "sin")
        val = num_val.value

        if isinstance(val, complex):
            return AIFPLNumber(cmath.sin(val))

        return AIFPLNumber(math.sin(val))

    def _builtin_cos(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement cos function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"cos takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "cos")
        val = num_val.value

        if isinstance(val, complex):
            return AIFPLNumber(cmath.cos(val))

        return AIFPLNumber(math.cos(val))

    def _builtin_tan(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement tan function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"tan takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "tan")
        val = num_val.value

        if isinstance(val, complex):
            return AIFPLNumber(cmath.tan(val))

        return AIFPLNumber(math.tan(val))

    def _builtin_log(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement log (natural logarithm) function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"log takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "log")
        val = num_val.value

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return AIFPLNumber(cmath.log(val))

        return AIFPLNumber(math.log(val))

    def _builtin_log10(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement log10 function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"log10 takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "log10")
        val = num_val.value

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return AIFPLNumber(cmath.log10(val))

        return AIFPLNumber(math.log10(val))

    def _builtin_exp(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement exp function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"exp takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "exp")
        val = num_val.value

        if isinstance(val, complex):
            return AIFPLNumber(cmath.exp(val))

        return AIFPLNumber(math.exp(val))

    def _builtin_sqrt(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement sqrt function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"sqrt takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "sqrt")
        val = num_val.value

        if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
            return AIFPLNumber(cmath.sqrt(val))

        return AIFPLNumber(math.sqrt(val))

    def _builtin_abs(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement abs function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"abs takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "abs")
        return AIFPLNumber(abs(num_val.value))

    def _builtin_round(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement round function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"round takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "round")

        # Extract real part if complex
        if isinstance(num_val.value, complex):
            if abs(num_val.value.imag) >= self.floating_point_tolerance:
                raise AIFPLEvalError("Function 'round' does not support complex numbers")

            val = num_val.value.real
        else:
            val = num_val.value

        return AIFPLNumber(round(val))

    def _builtin_floor(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement floor function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"floor takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "floor")

        # Extract real part if complex
        if isinstance(num_val.value, complex):
            if abs(num_val.value.imag) >= self.floating_point_tolerance:
                raise AIFPLEvalError("Function 'floor' does not support complex numbers")

            val = num_val.value.real
        else:
            val = num_val.value

        return AIFPLNumber(math.floor(val))

    def _builtin_ceil(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement ceil function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"ceil takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "ceil")

        # Extract real part if complex
        if isinstance(num_val.value, complex):
            if abs(num_val.value.imag) >= self.floating_point_tolerance:
                raise AIFPLEvalError("Function 'ceil' does not support complex numbers")

            val = num_val.value.real
        else:
            val = num_val.value

        return AIFPLNumber(math.ceil(val))

    def _builtin_min(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement min function."""
        if not args:
            raise AIFPLEvalError("min requires at least 1 argument, got 0")

        # Use type narrowing to handle only real numbers for min/max
        real_values = []
        for arg in args:
            real_val = self._ensure_real_number(arg, "min")
            real_values.append(real_val)

        return AIFPLNumber(min(real_values))

    def _builtin_max(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement max function."""
        if not args:
            raise AIFPLEvalError("max requires at least 1 argument, got 0")

        # Use type narrowing to handle only real numbers for min/max
        real_values = []
        for arg in args:
            real_val = self._ensure_real_number(arg, "max")
            real_values.append(real_val)

        return AIFPLNumber(max(real_values))

    def _builtin_pow(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement pow function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"pow takes exactly 2 arguments, got {len(args)}")

        base = self._ensure_number(args[0], "pow")
        exponent = self._ensure_number(args[1], "pow")
        return AIFPLNumber(base.value ** exponent.value)

    def _builtin_bin(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement bin function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"bin takes exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "bin")
        return AIFPLString(bin(int_val))

    def _builtin_hex(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement hex function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"hex takes exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "hex")
        return AIFPLString(hex(int_val))

    def _builtin_oct(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement oct function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"oct takes exactly 1 argument, got {len(args)}")

        int_val = self._ensure_integer(args[0], "oct")
        return AIFPLString(oct(int_val))

    def _builtin_real(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement real function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"real takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "real")
        val = num_val.value

        if isinstance(val, complex):
            real_part = val.real
            # Convert to int if it's a whole number
            if isinstance(real_part, float) and real_part.is_integer():
                return AIFPLNumber(int(real_part))

            return AIFPLNumber(real_part)

        # For real numbers, return as-is
        return num_val

    def _builtin_imag(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement imag function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"imag takes exactly 1 argument, got {len(args)}")

        num_val = self._ensure_number(args[0], "imag")
        val = num_val.value

        if isinstance(val, complex):
            imag_part = val.imag
            # Convert to int if it's a whole number
            if isinstance(imag_part, float) and imag_part.is_integer():
                return AIFPLNumber(int(imag_part))

            return AIFPLNumber(imag_part)

        # For real numbers, imaginary part is 0
        return AIFPLNumber(0)

    def _builtin_complex(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement complex function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"complex takes exactly 2 arguments, got {len(args)}")

        real_val = self._ensure_number(args[0], "complex")
        imag_val = self._ensure_number(args[1], "complex")

        if not isinstance(real_val.value, (int, float)) or not isinstance(imag_val.value, (int, float)):
            raise AIFPLEvalError("complex arguments must be real numbers")

        real_part, imag_part = real_val.value, imag_val.value
        return AIFPLNumber(complex(real_part, imag_part))

    def _builtin_string_append(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-append function."""
        if not args:
            return AIFPLString("")

        # Ensure all arguments are strings
        string_args = [self._ensure_string(arg, "string-append") for arg in args]
        result = ''.join(arg.value for arg in string_args)
        return AIFPLString(result)

    def _builtin_string_length(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-length function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-length takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-length")
        return AIFPLNumber(len(string_arg.value))

    def _builtin_substring(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement substring function."""
        if len(args) != 3:
            raise AIFPLEvalError(f"substring takes exactly 3 arguments, got {len(args)}")

        string_arg = self._ensure_string(args[0], "substring")
        start_idx = self._ensure_integer(args[1], "substring")
        end_idx = self._ensure_integer(args[2], "substring")

        string_len = len(string_arg.value)
        if start_idx < 0:
            raise AIFPLEvalError(f"substring start index cannot be negative: {start_idx}")

        if end_idx < 0:
            raise AIFPLEvalError(f"substring end index cannot be negative: {end_idx}")

        if start_idx > string_len:
            raise AIFPLEvalError(f"substring start index out of range: {start_idx} (string length: {string_len})")

        if end_idx > string_len:
            raise AIFPLEvalError(f"substring end index out of range: {end_idx} (string length: {string_len})")

        if start_idx > end_idx:
            raise AIFPLEvalError(f"substring start index ({start_idx}) cannot be greater than end index ({end_idx})")

        return AIFPLString(string_arg.value[start_idx:end_idx])

    def _builtin_string_upcase(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-upcase function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-upcase takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-upcase")
        return AIFPLString(string_arg.value.upper())

    def _builtin_string_downcase(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-downcase function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-downcase takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-downcase")
        return AIFPLString(string_arg.value.lower())

    def _builtin_string_ref(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-ref function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-ref takes exactly 2 arguments, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-ref")
        index = self._ensure_integer(args[1], "string-ref")

        # Check for negative index (not allowed in AIFPL)
        if index < 0:
            raise AIFPLEvalError(f"string-ref index out of range: {index}")

        string_len = len(string_arg.value)
        if index >= string_len:
            raise AIFPLEvalError(f"string-ref index out of range: {index}")

        return AIFPLString(string_arg.value[index])

    def _builtin_string_to_number(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string->number function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string->number takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string->number")

        try:
            # Try to parse as integer first
            if '.' not in string_arg.value and 'e' not in string_arg.value.lower() and 'j' not in string_arg.value.lower():
                return AIFPLNumber(int(string_arg.value))

            # Try complex number
            if 'j' in string_arg.value.lower():
                return AIFPLNumber(complex(string_arg.value))

            # Otherwise float
            return AIFPLNumber(float(string_arg.value))

        except ValueError as e:
            raise AIFPLEvalError(f"Cannot convert string to number: '{string_arg.value}'") from e

    def _builtin_number_to_string(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement number->string function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"number->string takes exactly 1 argument, got {len(args)}")

        num_arg = self._ensure_number(args[0], "number->string")
        return AIFPLString(str(num_arg.value))

    def _builtin_string_trim(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-trim function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string-trim takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string-trim")
        return AIFPLString(string_arg.value.strip())

    def _builtin_string_replace(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-replace function."""
        if len(args) != 3:
            raise AIFPLEvalError(f"string-replace takes exactly 3 arguments, got {len(args)}")

        string_arg, old_str, new_str = [self._ensure_string(arg, "string-replace") for arg in args]
        result = string_arg.value.replace(old_str.value, new_str.value)
        return AIFPLString(result)

    def _builtin_string_contains_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-contains? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-contains? takes exactly 2 arguments, got {len(args)}")

        string_arg, substring = [self._ensure_string(arg, "string-contains?") for arg in args]
        return AIFPLBoolean(substring.value in string_arg.value)

    def _builtin_string_prefix_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-prefix? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-prefix? takes exactly 2 arguments, got {len(args)}")

        string_arg, prefix = [self._ensure_string(arg, "string-prefix?") for arg in args]
        return AIFPLBoolean(string_arg.value.startswith(prefix.value))

    def _builtin_string_suffix_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-suffix? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-suffix? takes exactly 2 arguments, got {len(args)}")

        string_arg, suffix = [self._ensure_string(arg, "string-suffix?") for arg in args]
        return AIFPLBoolean(string_arg.value.endswith(suffix.value))

    def _builtin_string_eq_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string=? function."""
        if len(args) < 2:
            raise AIFPLEvalError(f"string=? requires at least 2 arguments, got {len(args)}")

        string_args = [self._ensure_string(arg, "string=?") for arg in args]
        first_val = string_args[0].value
        return AIFPLBoolean(all(arg.value == first_val for arg in string_args[1:]))

    def _builtin_list(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement list constructor."""
        return AIFPLList(tuple(args))

    def _builtin_cons(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement cons function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"cons takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "cons")
        return list_val.cons(item)

    def _builtin_append(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement append function."""
        if len(args) < 2:
            raise AIFPLEvalError(f"append requires at least 2 arguments, got {len(args)}")

        # Validate all arguments are lists
        list_vals = [self._ensure_list(arg, "append") for arg in args]

        # Concatenate all lists
        result = list_vals[0]
        for list_val in list_vals[1:]:
            result = result.append_list(list_val)

        return result

    def _builtin_reverse(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement reverse function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"reverse takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "reverse")
        return list_val.reverse()

    def _builtin_first(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement first function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"first takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "first")
        try:
            return list_val.first()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_rest(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement rest function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"rest takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "rest")
        try:
            return list_val.rest()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_last(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement last function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"last takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "last")
        try:
            return list_val.last()

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

    def _builtin_list_ref(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement list-ref function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"list-ref takes exactly 2 arguments, got {len(args)}")

        list_val = self._ensure_list(args[0], "list-ref")
        index = self._ensure_integer(args[1], "list-ref")

        # Check for negative index (not allowed in AIFPL)
        if index < 0:
            raise AIFPLEvalError(f"list-ref index out of range: {index}")

        try:
            return list_val.get(index)

        except IndexError as e:
            raise AIFPLEvalError(f"list-ref index out of range: {index}") from e

    def _builtin_length(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement length function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"length takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "length")
        return AIFPLNumber(list_val.length())

    def _builtin_null_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement null? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"null? takes exactly 1 argument, got {len(args)}")

        list_val = self._ensure_list(args[0], "null?")
        return AIFPLBoolean(list_val.is_empty())

    def _builtin_list_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement list? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"list? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLList))

    def _builtin_member_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement member? function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"member? takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "member?")
        return AIFPLBoolean(list_val.contains(item))

    def _builtin_remove(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement remove function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"remove takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "remove")
        return list_val.remove_all(item)

    def _builtin_position(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement position function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"position takes exactly 2 arguments, got {len(args)}")

        item, list_arg = args
        list_val = self._ensure_list(list_arg, "position")

        pos = list_val.position(item)
        if pos is not None:
            return AIFPLNumber(pos)

        return AIFPLBoolean(False)

    def _builtin_take(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement take function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"take takes exactly 2 arguments, got {len(args)}")

        n = self._ensure_integer(args[0], "take")
        list_val = self._ensure_list(args[1], "take")
        if n < 0:
            raise AIFPLEvalError(f"take count cannot be negative: {n}")

        return list_val.take(n)

    def _builtin_drop(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement drop function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"drop takes exactly 2 arguments, got {len(args)}")

        n = self._ensure_integer(args[0], "drop")
        list_val = self._ensure_list(args[1], "drop")
        if n < 0:
            raise AIFPLEvalError(f"drop count cannot be negative: {n}")

        return list_val.drop(n)

    def _builtin_string_to_list(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string->list function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string->list takes exactly 1 argument, got {len(args)}")

        string_arg = self._ensure_string(args[0], "string->list")
        elements = tuple(AIFPLString(char) for char in string_arg.value)
        return AIFPLList(elements)

    def _builtin_list_to_string(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement list->string function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"list->string takes exactly 1 argument, got {len(args)}")

        list_arg = self._ensure_list(args[0], "list->string")

        try:
            return AIFPLString(''.join(str(elem.to_python()) for elem in list_arg.elements))

        except Exception as e:
            raise AIFPLEvalError(f"Cannot convert list to string: {e}") from e

    def _builtin_string_split(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-split function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-split takes exactly 2 arguments, got {len(args)}")

        string_arg, delimiter = [self._ensure_string(arg, "string-split") for arg in args]

        # Handle empty separator case - split into individual characters
        if delimiter.value == "":
            return self._builtin_string_to_list([string_arg], env, depth)

        parts = string_arg.value.split(delimiter.value)
        elements = tuple(AIFPLString(part) for part in parts)
        return AIFPLList(elements)

    def _builtin_string_join(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string-join function."""
        if len(args) != 2:
            raise AIFPLEvalError(f"string-join takes exactly 2 arguments, got {len(args)}")

        list_arg = self._ensure_list(args[0], "string-join")
        separator = self._ensure_string(args[1], "string-join")

        # Ensure all list elements are strings
        str_items = []
        for item in list_arg.elements:
            if not isinstance(item, AIFPLString):
                raise AIFPLEvalError(f"string-join requires list of strings, found {item.type_name()}")

            str_items.append(item.value)

        return AIFPLString(separator.value.join(str_items))

    def _builtin_number_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement number? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"number? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber))

    def _builtin_integer_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement integer? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"integer? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_integer())

    def _builtin_float_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement float? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"float? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_float())

    def _builtin_complex_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement complex? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"complex? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_complex())

    def _builtin_string_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement string? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"string? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLString))

    def _builtin_boolean_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement boolean? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"boolean? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], AIFPLBoolean))

    def _builtin_function_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement function? function."""
        if len(args) != 1:
            raise AIFPLEvalError(f"function? takes exactly 1 argument, got {len(args)}")

        return AIFPLBoolean(isinstance(args[0], (AIFPLFunction, AIFPLBuiltinFunction)))

    def _builtin_any_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement any? function - delegated to higher-order function handler."""
        return self._apply_higher_order_function('any?', args, env, depth)

    def _builtin_all_p(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Implement all? function - delegated to higher-order function handler."""
        return self._apply_higher_order_function('all?', args, env, depth)

    def _ensure_number(self, value: AIFPLValue, function_name: str) -> AIFPLNumber:
        """Ensure value is a number, raise error if not."""
        if not isinstance(value, AIFPLNumber):
            raise AIFPLEvalError(f"Function '{function_name}' requires numeric arguments, got {value.type_name()}")

        return value

    def _ensure_string(self, value: AIFPLValue, function_name: str) -> AIFPLString:
        """Ensure value is a string, raise error if not."""
        if not isinstance(value, AIFPLString):
            raise AIFPLEvalError(f"Function '{function_name}' requires string arguments, got {value.type_name()}")

        return value

    def _ensure_boolean(self, value: AIFPLValue, function_name: str) -> AIFPLBoolean:
        """Ensure value is a boolean, raise error if not."""
        if not isinstance(value, AIFPLBoolean):
            raise AIFPLEvalError(f"Function '{function_name}' requires boolean arguments, got {value.type_name()}")

        return value

    def _ensure_list(self, value: AIFPLValue, function_name: str) -> AIFPLList:
        """Ensure value is a list, raise error if not."""
        if not isinstance(value, AIFPLList):
            raise AIFPLEvalError(f"Function '{function_name}' requires list arguments, got {value.type_name()}")

        return value

    def _ensure_integer(self, value: AIFPLValue, function_name: str) -> int:
        """Ensure value is an integer, raise error if not."""
        if not isinstance(value, AIFPLNumber) or not value.is_integer():
            raise AIFPLEvalError(f"Function '{function_name}' requires integer arguments, got {value.type_name()}")

        # Type narrowing: we know value.value is int here
        assert isinstance(value.value, int), "is_integer() should guarantee int type"
        return value.value

    def _ensure_real_number(self, value: AIFPLValue, function_name: str) -> Union[int, float]:
        """Ensure value is a real number (int or float), raise error if complex."""
        if not isinstance(value, AIFPLNumber):
            raise AIFPLEvalError(f"Function '{function_name}' requires numeric arguments, got {value.type_name()}")

        if isinstance(value.value, complex):
            raise AIFPLEvalError(f"Function '{function_name}' does not support complex numbers")

        # Type narrowing: we know value.value is int or float here
        return value.value

    def simplify_result(self, result: AIFPLValue) -> AIFPLValue:
        """Simplify complex results to real numbers when imaginary part is negligible."""
        if isinstance(result, AIFPLNumber) and isinstance(result.value, complex):
            # If imaginary part is effectively zero, return just the real part
            if abs(result.value.imag) < self.floating_point_tolerance:
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

        if isinstance(result, AIFPLBuiltinFunction):
            # Format builtin functions
            return f"<builtin {result.name}>"

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

    def _is_close_to_nice_number(self, value: float) -> float | None:
        """Check if a float is very close to a 'nice' number and return the nice number if so."""
        # Check if it's close to common fractions with small denominators
        for denominator in range(1, 11):  # Check denominators 1-10
            for numerator in range(-50, 51):  # Check reasonable range
                nice_value = numerator / denominator
                if abs(value - nice_value) < self.floating_point_tolerance:
                    return nice_value

        return None
