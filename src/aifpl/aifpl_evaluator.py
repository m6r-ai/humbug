"""Evaluator for AIFPL Abstract Syntax Trees using pure list representation."""

import math
from dataclasses import dataclass
from typing import List, Union

from aifpl.aifpl_call_stack import AIFPLCallStack
from aifpl.aifpl_collections import AIFPLCollectionsFunctions
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_math import AIFPLMathFunctions
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

        # Create function modules
        self.math_functions = AIFPLMathFunctions(floating_point_tolerance)
        self.collections_functions = AIFPLCollectionsFunctions()

        # Create built-in functions with their native implementations
        self._builtin_functions = self._create_builtin_functions()

    def _create_builtin_functions(self) -> dict[str, AIFPLBuiltinFunction]:
        """Create all built-in functions with their native implementations."""
        builtins = {}

        # Add mathematical functions
        for name, impl in self.math_functions.get_functions().items():
            builtins[name] = AIFPLBuiltinFunction(name, impl)

        # Add collections functions
        for name, impl in self.collections_functions.get_functions().items():
            builtins[name] = AIFPLBuiltinFunction(name, impl)

        # Add higher-order functions (defined in this class)
        builtins['if'] = AIFPLBuiltinFunction('if', self._builtin_if_special)
        builtins['and'] = AIFPLBuiltinFunction('and', self._builtin_and_special)
        builtins['or'] = AIFPLBuiltinFunction('or', self._builtin_or_special)
        builtins['map'] = AIFPLBuiltinFunction('map', self._builtin_map_special)
        builtins['filter'] = AIFPLBuiltinFunction('filter', self._builtin_filter_special)
        builtins['fold'] = AIFPLBuiltinFunction('fold', self._builtin_fold_special)
        builtins['range'] = AIFPLBuiltinFunction('range', self._builtin_range_special)
        builtins['find'] = AIFPLBuiltinFunction('find', self._builtin_find_special)
        builtins['any?'] = AIFPLBuiltinFunction('any?', self._builtin_any_p_special)
        builtins['all?'] = AIFPLBuiltinFunction('all?', self._builtin_all_p_special)

        return builtins

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

            # Add built-in functions to global environment
            for name, builtin_func in self._builtin_functions.items():
                env = env.define(name, builtin_func)

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
                if first_elem.name == "quote":
                    return self._evaluate_quote_form(expr, env, depth + 1)

                if first_elem.name == "lambda":
                    return self._evaluate_lambda_form(expr, env, depth + 1)

                if first_elem.name == "let":
                    return self._evaluate_let_form(expr, env, depth + 1)

                # Regular function call (including built-ins and user functions)
                return self._evaluate_function_call(expr, env, depth + 1)

            # First element is not a symbol - evaluate as function call anyway
            return self._evaluate_function_call(expr, env, depth + 1)

        raise AIFPLEvalError(f"Invalid expression type: {type(expr).__name__}")

    def _evaluate_quote_form(
        self,
        quote_list: AIFPLList,
        _env: AIFPLEnvironment,
        _depth: int
    ) -> AIFPLValue:
        """
        Evaluate (quote expr) form - returns expr without evaluation.

        Args:
            quote_list: List representing quote expression
            _env: Current environment (unused for quote)
            _depth: Current recursion depth (unused for quote)

        Returns:
            The quoted expression unevaluated
        """
        if quote_list.length() != 2:
            raise AIFPLEvalError(f"quote requires exactly 1 argument, got {quote_list.length() - 1}")

        # Return the quoted expression without evaluation
        return quote_list.get(1)

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

            # We can only call functions!
            if not isinstance(func_value, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(f"Cannot call non-function value: {func_value.type_name()}")

            # Check if this is a special form that needs unevaluated arguments
            if isinstance(func_value, AIFPLBuiltinFunction) and self._is_special_form(func_value.name):
                # Special forms get unevaluated arguments
                return func_value.native_impl(arg_exprs, current_env, depth)

            # Regular functions get evaluated arguments
            try:
                arg_values = [self._evaluate_expression(arg, current_env, depth) for arg in arg_exprs]

            except AIFPLEvalError as e:
                raise AIFPLEvalError(f"Error evaluating function arguments: {e}") from e

            result = self._call_function(func_value, arg_values, current_env, depth)

            # Check if result is a tail call
            if isinstance(result, AIFPLTailCall):
                # Continue the loop with the tail call
                current_call = AIFPLList((result.function,) + tuple(result.arguments))
                current_env = result.environment
                continue

            # Regular result, return it
            return result

    def _is_special_form(self, function_name: str) -> bool:
        """Check if a function name is a special form that needs unevaluated arguments."""
        return function_name in ['if', 'and', 'or', 'map', 'filter', 'fold', 'range', 'find', 'any?', 'all?']

    def _call_function(
        self,
        func: Union[AIFPLFunction, AIFPLBuiltinFunction],
        arg_values: List[AIFPLValue],
        env: AIFPLEnvironment,
        depth: int
    ) -> Union[AIFPLValue, AIFPLTailCall]:
        """
        Unified function calling mechanism for both user-defined and built-in functions.

        Args:
            func: Function to call (either AIFPLFunction or AIFPLBuiltinFunction)
            arg_values: Already-evaluated argument values
            env: Current environment
            depth: Current recursion depth

        Returns:
            Function result or AIFPLTailCall for optimization
        """
        if isinstance(func, AIFPLFunction):
            return self._call_lambda_function(func, arg_values, env, depth)

        if isinstance(func, AIFPLBuiltinFunction):
            return self._call_builtin_function(func, arg_values, env, depth)

        raise AIFPLEvalError(f"Cannot call non-function value: {func.type_name()}")

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
        func: AIFPLBuiltinFunction,
        arg_values: List[AIFPLValue],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Call a built-in function with its native implementation.

        Args:
            func: Built-in function to call
            arg_values: Already-evaluated argument values
            env: Current environment
            depth: Current recursion depth

        Returns:
            Function result
        """
        try:
            return func.native_impl(arg_values, env, depth)

        except AIFPLEvalError:
            # Re-raise AIFPL errors as-is
            raise

        except Exception as e:
            # Wrap other exceptions with context
            raise AIFPLEvalError(f"Error in built-in function '{func.name}': {e}") from e

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
            # Handle quote expressions - they are NOT tail calls, just return the quoted value
            if first_elem.name == 'quote':
                return self._evaluate_quote_form(expr, env, depth + 1)

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

        # We can only call functions!
        if not isinstance(func_value, (AIFPLFunction, AIFPLBuiltinFunction)):
            raise AIFPLEvalError(f"Cannot call non-function value: {func_value.type_name()}")

        result = self._call_function(func_value, arg_values, env, depth)

        # For higher-order functions, we don't want tail call optimization
        if isinstance(result, AIFPLTailCall):
            # This shouldn't happen in higher-order contexts, but handle it gracefully
            raise AIFPLEvalError("Unexpected tail call in higher-order function context")

        return result

    # Higher-order functions and special forms
    def _builtin_if_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Handle if conditional with lazy evaluation of branches."""
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

    def _builtin_and_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLBoolean:
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

    def _builtin_or_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLBoolean:
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

    def _builtin_map_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Apply map function with unevaluated function argument."""
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

    def _builtin_filter_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Apply filter function with unevaluated predicate argument."""
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

    def _builtin_fold_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Apply fold function with unevaluated function argument."""
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

    def _builtin_range_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Apply range function with evaluated arguments."""
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

    def _builtin_find_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Apply find function with unevaluated predicate argument."""
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

    def _builtin_any_p_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Apply any? function with unevaluated predicate argument."""
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

    def _builtin_all_p_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLValue:
        """Apply all? function with unevaluated predicate argument."""
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

    # Helper method for higher-order functions
    def _ensure_integer(self, value: AIFPLValue, function_name: str) -> int:
        """Ensure value is an integer, raise error if not."""
        if not isinstance(value, AIFPLNumber) or not value.is_integer():
            raise AIFPLEvalError(f"Function '{function_name}' requires integer arguments, got {value.type_name()}")

        # Type narrowing: we know value.value is int here
        assert isinstance(value.value, int), "is_integer() should guarantee int type"
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
