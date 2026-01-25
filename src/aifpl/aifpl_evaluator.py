"""Evaluator for AIFPL Abstract Syntax Trees with detailed error messages."""

import math
from typing import List, Dict, cast

from aifpl.aifpl_call_stack import AIFPLCallStack
from aifpl.aifpl_builtins import AIFPLBuiltinRegistry
from aifpl.aifpl_error import AIFPLEvalError, ErrorMessageBuilder
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_pattern_matcher import AIFPLPatternMatcher
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol,
    AIFPLList, AIFPLAList, AIFPLRecursivePlaceholder, AIFPLFunction, AIFPLTailCall
)
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer, AIFPLBindingGroup


class AIFPLEvaluator:
    """Evaluates AIFPL Abstract Syntax Trees using pure list representation with detailed error messages."""

    # Mathematical constants
    CONSTANTS = {
        'pi': AIFPLNumber(math.pi),
        'e': AIFPLNumber(math.e),
        'j': AIFPLNumber(1j),
        'true': AIFPLBoolean(True),
        'false': AIFPLBoolean(False),
    }

    def __init__(self, max_depth: int = 1000, floating_point_tolerance: float = 1e-10):
        """
        Initialize evaluator.

        Args:
            max_depth: Maximum recursion depth
            floating_point_tolerance: Tolerance for floating point comparisons and simplifications
        """
        self.max_depth = max_depth
        self.floating_point_tolerance = floating_point_tolerance
        self.call_stack = AIFPLCallStack()
        self.current_expression = ""  # Store original expression for context
        self.message_builder = ErrorMessageBuilder()

        # Add call chain tracking for mutual recursion detection
        self.call_chain_list: List[AIFPLFunction] = []
        self.call_chain_set: set = set()  # For O(1) membership check

        # Create builtin registry
        self.builtin_registry = AIFPLBuiltinRegistry(floating_point_tolerance)

        # Create pattern matcher
        self.pattern_matcher = AIFPLPatternMatcher(self.format_result)

        # Create built-in functions with their native implementations
        self._builtin_functions = self._create_builtin_functions()

    def set_expression_context(self, expression: str) -> None:
        """Set the current expression for error context."""
        self.current_expression = expression

    def _create_builtin_functions(self) -> dict[str, AIFPLFunction]:
        """Create all built-in functions with their native implementations."""
        # Get all regular builtins from the registry
        builtins = self.builtin_registry.create_builtin_function_objects()

        # Add special forms (these require special evaluation semantics)
        builtins['and'] = AIFPLFunction(parameters=('args',), native_impl=self._builtin_and_special, name='and', is_variadic=True)
        builtins['or'] = AIFPLFunction(parameters=('args',), native_impl=self._builtin_or_special, name='or', is_variadic=True)

        return builtins

    # Helper methods for common type checking patterns
    def _is_symbol_with_name(self, value: AIFPLValue, name: str) -> bool:
        """Check if value is a symbol with the given name."""
        return isinstance(value, AIFPLSymbol) and value.name == name

    def evaluate(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Recursively evaluate AST.

        Args:
            expr: Expression to evaluate

        Returns:
            Evaluation result as AIFPLValue

        Raises:
            AIFPLEvalError: If evaluation fails
        """
        env = AIFPLEnvironment()  # Global environment, no function reference

        # Add constants and built-in functions to global environment (batch for efficiency)
        global_bindings = {**self.CONSTANTS, **self._builtin_functions}
        env = env.define_many(global_bindings)

        # All code paths in the evaluator raise AIFPLEvalError, so the generic exception
        # wrapper is unreachable. We only need to re-raise AIFPLEvalError.
        return self._evaluate_expression(expr, env, 0)

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
            raise AIFPLEvalError(
                message=f"Expression too deeply nested (max depth: {self.max_depth})",
                context=f"Call stack:\n{stack_trace}",
                suggestion="Reduce nesting depth or increase max_depth limit"
            )

        # Check most common self-evaluating types first (numbers are very common)
        if expr.is_self_evaluating():
            return expr

        # Symbol lookup
        if isinstance(expr, AIFPLSymbol):
            try:
                return env.lookup(expr.name)

            except AIFPLEvalError as e:
                # Add more context to symbol lookup errors
                available_vars = env.get_available_bindings()

                raise AIFPLEvalError(
                    message=f"Undefined variable: '{expr.name}'",
                    context=f"Available variables: {', '.join(sorted(available_vars)[:10])}"
                        "{'...' if len(available_vars) > 10 else ''}",
                    suggestion=f"Check spelling or define '{expr.name}' in a let binding",
                    example=f"(let (({expr.name} some-value)) ...)"
                ) from e

        # List evaluation - check for special forms FIRST before any symbol evaluation
        if not isinstance(expr, AIFPLList):
            raise AIFPLEvalError(f"Unexpected expression type: {type(expr)}")

        # Empty list evaluates to itself
        if expr.is_empty():
            return expr

        # Non-empty list - check first element for special forms
        first_elem = expr.first()
        if isinstance(first_elem, AIFPLSymbol):
            # Handle special forms BEFORE attempting any symbol lookup
            if self._is_symbol_with_name(first_elem, "quote"):
                return self._evaluate_quote_form(expr, env, depth + 1)

            if self._is_symbol_with_name(first_elem, 'if'):
                return self._evaluate_if_form(expr, env, depth + 1, False)

            if self._is_symbol_with_name(first_elem, "lambda"):
                return self._evaluate_lambda_form(expr, env, depth + 1)

            if self._is_symbol_with_name(first_elem, "let"):
                return self._evaluate_let_form(expr, env, depth + 1, False)

            if self._is_symbol_with_name(first_elem, "match"):
                return self.pattern_matcher.evaluate_match_form(expr, env, depth + 1, self._evaluate_expression)

        # Evaluate as function call
        return self._evaluate_function_call(expr, env, depth + 1)

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
            raise AIFPLEvalError(
                message="Quote expression has wrong number of arguments",
                received=f"Got {quote_list.length() - 1} arguments: {self.format_result(quote_list)}",
                expected="Exactly 1 argument",
                example="(quote expr) or 'expr",
                suggestion="Quote requires exactly one expression to quote"
            )

        # Return the quoted expression without evaluation
        return quote_list.get(1)

    def _evaluate_lambda_form(
        self,
        lambda_list: AIFPLList,
        env: AIFPLEnvironment,
        _depth: int
    ) -> AIFPLFunction:
        """
        Evaluate (lambda (param1 param2 ...) body) form with enhanced error messages.

        Args:
            lambda_list: List representing lambda expression
            env: Current environment
            _depth: Current recursion depth

        Returns:
            AIFPLFunction object
        """
        if lambda_list.length() != 3:
            raise AIFPLEvalError(
                message="Lambda expression structure is incorrect",
                received=f"Got {lambda_list.length()} elements: {self.format_result(lambda_list)}",
                expected="Exactly 3 elements: (lambda (params...) body)",
                example="(lambda (x y) (+ x y))",
                suggestion="Lambda needs parameter list and body: (lambda (param1 param2 ...) body-expression)"
            )

        # Extract parameter list
        param_expr = lambda_list.get(1)

        if not isinstance(param_expr, AIFPLList):
            raise AIFPLEvalError(
                message="Lambda parameters must be a list",
                received=f"Parameter list: {self.format_result(param_expr)} ({param_expr.type_name()})",
                expected="List of symbols: (param1 param2 ...)",
                example="(lambda (x y z) (+ x y z))",
                suggestion="Parameters should be unquoted variable names"
            )

        # Extract parameters and ensure they're all symbols
        raw_parameters: List[AIFPLValue] = []
        raw_parameters = list(param_expr.elements)

        # Validate parameters are all symbols and convert them
        parameters: List[str] = []
        for i, param in enumerate(raw_parameters):
            if not isinstance(param, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Lambda parameter {i+1} must be a symbol",
                    received=f"Parameter {i+1}: {self.format_result(param)} ({param.type_name()})",
                    expected="Unquoted symbol (variable name)",
                    example="Correct: (lambda (x y) (+ x y))\nIncorrect: (lambda (\"x\" 1) ...)",
                    suggestion="Use unquoted names: x, not \"x\" or 1"
                )

            parameters.append(param.name)

        # Check for duplicate parameters
        if len(parameters) != len(set(parameters)):
            duplicates = [p for p in parameters if parameters.count(p) > 1]
            raise AIFPLEvalError(
                message="Lambda parameters must be unique",
                received=f"Duplicate parameters: {duplicates}",
                expected="All parameter names should be different",
                example="Correct: (lambda (x y z) ...)\nIncorrect: (lambda (x y x) ...)",
                suggestion="Use different names for each parameter"
            )

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
        depth: int,
        in_tail_position: bool = False
    ) -> AIFPLValue:
        """
        Evaluate (let ((var1 val1) (var2 val2) ...) body) form with enhanced error messages.

        Args:
            let_list: List representing let expression
            env: Current environment
            depth: Current recursion depth
            in_tail_position: Whether this let is in tail position (for TCO)

        Returns:
            Result of evaluating the let body
        """
        if let_list.length() != 3:
            raise AIFPLEvalError(
                message="Let expression structure is incorrect",
                received=f"Got {let_list.length()} elements: {self.format_result(let_list)}",
                expected="Exactly 3 elements: (let ((bindings...)) body)",
                example="(let ((x 5) (y 10)) (+ x y))",
                suggestion="Let needs binding list and body: (let ((var1 val1) (var2 val2) ...) body)"
            )

        # Parse binding list
        binding_expr = let_list.get(1)

        if not isinstance(binding_expr, AIFPLList):
            raise AIFPLEvalError(
                message="Let binding list must be a list",
                received=f"Binding list: {self.format_result(binding_expr)} ({binding_expr.type_name()})",
                expected="List of bindings: ((var1 val1) (var2 val2) ...)",
                example="(let ((x 5) (y (* x 2))) (+ x y))",
                suggestion="Wrap bindings in parentheses: ((var val) (var val) ...)"
            )

        bindings = []
        for i, binding in enumerate(binding_expr.elements):
            if not isinstance(binding, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} must be a list",
                    received=f"Binding {i+1}: {self.format_result(binding)} ({binding.type_name()})",
                    expected="List with variable and value: (var val)",
                    example="Correct: (x 5)\nIncorrect: x or \"x\"",
                    suggestion="Wrap each binding in parentheses: (variable value)"
                )

            if binding.length() != 2:
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} has wrong number of elements",
                    received=f"Binding {i+1}: {self.format_result(binding)} (has {binding.length()} elements)",
                    expected="Each binding needs exactly 2 elements: (variable value)",
                    example="Correct: (x 5)\nIncorrect: (x) or (x 5 6)",
                    suggestion="Each binding: (variable-name value-expression)"
                )

            var_name_expr = binding.get(0)
            var_value_expr = binding.get(1)

            if not isinstance(var_name_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} variable must be a symbol",
                    received=f"Variable: {self.format_result(var_name_expr)} ({var_name_expr.type_name()})",
                    expected="Unquoted symbol (variable name)",
                    example="Correct: (x 5)\nIncorrect: (\"x\" 5) or (1 5)",
                    suggestion="Use unquoted variable names: x, not \"x\""
                )

            bindings.append((var_name_expr.name, var_value_expr))

        # Check for duplicate binding names
        var_names = [name for name, _ in bindings]
        if len(var_names) != len(set(var_names)):
            duplicates = [name for name in var_names if var_names.count(name) > 1]
            raise AIFPLEvalError(
                message="Let binding variables must be unique",
                received=f"Duplicate variables: {duplicates}",
                expected="All variable names should be different",
                example="Correct: (let ((x 1) (y 2)) ...)\nIncorrect: (let ((x 1) (x 2)) ...)",
                suggestion="Use different names for each variable"
            )

        body = let_list.get(2)

        # Analyze dependencies
        analyzer = AIFPLDependencyAnalyzer()
        binding_groups = analyzer.analyze_let_bindings(bindings)

        # Evaluate groups in order
        current_env = AIFPLEnvironment(bindings={}, parent=env)  # Let environment, no function reference
        for group in binding_groups:
            if group.is_recursive:
                current_env = self._evaluate_recursive_binding_group(group, current_env, depth)

            else:
                current_env = self._evaluate_sequential_binding_group(group, current_env, depth)

        # Evaluate body in the final environment
        # Use tail detection if we're in tail position
        if in_tail_position:
            return self._evaluate_expression_with_tail_detection(body, current_env, depth)

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
                raise AIFPLEvalError(
                    message=f"Error evaluating let binding '{name}'",
                    context=str(e),
                    received=f"Binding: ({name} {self.format_result(expr)})",
                    suggestion=f"Check the expression for variable '{name}'"
                ) from e

        return current_env

    def _evaluate_recursive_binding_group(
        self,
        group: AIFPLBindingGroup,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLEnvironment:
        """Evaluate a recursive binding group using recursive placeholders."""
        # Step 1: Create environment with recursive placeholders
        placeholders: Dict[str, AIFPLRecursivePlaceholder] = {}
        for name, _ in group.bindings:
            placeholder = AIFPLRecursivePlaceholder(name)
            placeholders[name] = placeholder

        recursive_env = env.define_many(cast(Dict[str, AIFPLValue], placeholders))

        # Step 2: Evaluate all binding expressions in the recursive environment
        resolved_values = {}
        for name, expr in group.bindings:
            try:
                value = self._evaluate_expression(expr, recursive_env, depth + 1)
                resolved_values[name] = value

            except AIFPLEvalError as e:
                raise AIFPLEvalError(
                    message=f"Error evaluating recursive let binding '{name}'",
                    context=str(e),
                    received=f"Recursive binding: ({name} {self.format_result(expr)})",
                    suggestion=f"Check the recursive expression for variable '{name}'"
                ) from e

        # Step 3: Update placeholders with resolved values
        for name, placeholder in placeholders.items():
            placeholder.resolve(resolved_values[name])

        # Step 4: Create final environment with resolved values (batch for efficiency)
        final_env = env.define_many(resolved_values)

        return final_env

    def _evaluate_function_call(
        self,
        func_list: AIFPLList,
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Evaluate function call with tail call optimization and enhanced error messages.

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
            func_expr = current_call.first()
            arg_exprs = list(current_call.elements[1:])

            # Evaluate the function expression
            try:
                func_value = self._evaluate_expression(func_expr, current_env, depth)

            except AIFPLEvalError as e:
                if "Undefined variable" in str(e) and isinstance(func_expr, AIFPLSymbol):
                    # Enhanced unknown function error
                    available_functions = list(self._builtin_functions.keys())
                    similar = self.message_builder.suggest_similar_functions(
                        func_expr.name, available_functions
                    )

                    suggestion_text = ""
                    if similar:
                        suggestion_text = f"Did you mean: {', '.join(similar)}?"

                    else:
                        # Show some common functions
                        common_funcs = ['+', '-', '*', '/', '=', '<', '>', 'list', 'map', 'filter', 'let', 'lambda']
                        available_common = [f for f in common_funcs if f in available_functions]
                        suggestion_text = f"Common functions: {', '.join(available_common[:8])}"

                    example = self.message_builder.create_function_example(func_expr.name)

                    raise AIFPLEvalError(
                        message=f"Unknown function: '{func_expr.name}'",
                        context=f"Available functions include: {', '.join(sorted(available_functions)[:10])}...",
                        suggestion=suggestion_text,
                        example=example
                    ) from e

                raise AIFPLEvalError(
                    message="Error evaluating function expression",
                    received=f"Function expression: {self.format_result(func_expr)}",
                    context=str(e),
                    suggestion="Check that the function name is spelled correctly"
                ) from e

            # We can only call functions!
            if not isinstance(func_value, AIFPLFunction):
                func_name = func_expr.name if isinstance(func_expr, AIFPLSymbol) else str(func_expr)
                raise AIFPLEvalError(
                    message="Cannot call non-function value",
                    received=f"Trying to call: {self.format_result(func_value)} ({func_value.type_name()})",
                    expected="Function (builtin or lambda)",
                    example="(+ 1 2) calls function +\n(42 1 2) tries to call number 42",
                    suggestion=f"'{func_name}' is not a function - check spelling or define it first"
                )

            # Check if this is a special form that needs unevaluated arguments
            if func_value.is_native and self._is_special_form(func_value.name):
                # Special forms get unevaluated arguments
                return func_value.native_impl(arg_exprs, current_env, depth)

            # Regular functions get evaluated arguments
            try:
                arg_values = [self._evaluate_expression(arg, current_env, depth) for arg in arg_exprs]

            except AIFPLEvalError as e:
                raise AIFPLEvalError(
                    message="Error evaluating function arguments",
                    context=str(e),
                    received=f"Arguments: {[self.format_result(arg) for arg in arg_exprs]}",
                    suggestion="Check each argument for syntax errors"
                ) from e

            if func_value.is_native:
                result = self._call_builtin_function(cast(AIFPLFunction, func_value), arg_values, env, depth)
            else:
                result = self._call_lambda_function(cast(AIFPLFunction, func_value), arg_values, env, depth)

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
        # Only and/or need special forms for short-circuit evaluation
        return function_name in ['and', 'or']

    def _call_lambda_function(
        self,
        func: AIFPLFunction,
        arg_values: List[AIFPLValue],
        _env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Common logic for calling a lambda function with evaluated argument values and enhanced error messages.

        Args:
            func: Lambda function to call
            arg_values: Already-evaluated argument values
            env: Current environment (used for closure environment context)
            depth: Current recursion depth

        Returns:
            Function result or AIFPLTailCall for optimization
        """
        # Check arity with detailed error
        if len(arg_values) != len(func.parameters):
            param_list = ", ".join(func.parameters) if func.parameters else "(no parameters)"
            arg_list = ", ".join(self.format_result(arg) for arg in arg_values) if arg_values else "(no arguments)"

            raise AIFPLEvalError(
                message=f"Function '{func.name}' expects {len(func.parameters)} arguments, got {len(arg_values)}",
                received=f"Arguments provided: {arg_list}",
                expected=f"Parameters expected: {param_list}",
                example=(f"({func.name} {' '.join(['arg' + str(i+1) for i in range(len(func.parameters))])})"
                    if func.parameters else f"({func.name})"),
                suggestion=f"Provide exactly {len(func.parameters)} argument{'s' if len(func.parameters) != 1 else ''}"
            )

        # Bind parameters to arguments (build dict once)
        param_bindings = {}
        for param, arg_value in zip(func.parameters, arg_values):
            param_bindings[param] = arg_value

        # Create new environment for function execution with all bindings at once
        func_env = AIFPLEnvironment(
            bindings=param_bindings,
            parent=func.closure_environment,
            function=func
        )

        # Ensure this is an AST-based function (not bytecode)
        if func.body is None:
            raise AIFPLEvalError(
                "Cannot evaluate bytecode function in AST evaluator - use VM instead"
            )

        # Add call frame to stack for error reporting
        self.call_stack.push(
            function_name=func.name or "<lambda>",
            arguments=param_bindings,
            expression=func.body
        )

        # Track function in call chain for mutual recursion detection
        self.call_chain_list.append(func)
        self.call_chain_set.add(id(func))

        try:
            # Enable tail call optimization with mutual recursion support
            return self._evaluate_expression_with_tail_detection(func.body, func_env, depth)

        finally:
            # Remove function from call chain and pop call stack
            self.call_chain_list.pop()
            self.call_chain_set.discard(id(func))
            self.call_stack.pop()

    def _call_builtin_function(
        self,
        func: AIFPLFunction,
        arg_values: List[AIFPLValue],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """
        Call a built-in function with its native implementation.

        Special forms (and, or) receive env and depth for evaluation.
        Regular builtins only receive args.

        Args:
            func: Built-in function to call
            arg_values: Already-evaluated argument values
            env: Current environment
            depth: Current recursion depth

        Returns:
            Function result
        """
        try:
            # Special forms need env and depth for evaluation, regular builtins don't
            if self._is_special_form(func.name):
                return func.native_impl(arg_values, env, depth)

            # Higher-order functions need special handling for lambda arguments
            if func.name in ['map', 'filter', 'fold', 'find', 'any?', 'all?']:
                # Temporarily override the collections module's _call_function method
                # to use our evaluator's lambda calling capability
                old_call = self.builtin_registry.collections_functions._call_function
                self.builtin_registry.collections_functions._call_function = lambda f, args, ctx: self._call_function_for_higher_order(f, args, env, depth)
                try:
                    return func.native_impl(arg_values)

                finally:
                    self.builtin_registry.collections_functions._call_function = old_call

            return func.native_impl(arg_values)

        except AIFPLEvalError:
            # Re-raise AIFPL errors as-is
            raise

        except Exception as e:
            # Wrap other exceptions with context
            raise AIFPLEvalError(
                message=f"Error in built-in function '{func.name}'",
                context=str(e),
                suggestion="This is an internal error - please report this issue"
            ) from e

    def _call_function_for_higher_order(
        self,
        func: AIFPLValue,
        args: List[AIFPLValue],
        env: AIFPLEnvironment,
        depth: int
    ) -> AIFPLValue:
        """Helper to call functions from higher-order functions (map, filter, etc.)."""
        if not isinstance(func, AIFPLFunction):
            raise AIFPLEvalError(
                message="Cannot call non-function value",
                received=f"Got: {func.type_name()}",
                expected="Function (lambda or builtin)"
            )
        
        if func.is_native:
            return self._call_builtin_function(func, args, env, depth)
        else:
            return self._call_lambda_function(func, args, env, depth)

    def _evaluate_expression_with_tail_detection(
        self,
        expr: AIFPLValue,
        env: AIFPLEnvironment,
        depth: int,
    ) -> AIFPLValue:
        """
        Evaluate an expression with tail call detection.

        Args:
            expr: Expression to evaluate
            env: Environment
            depth: Current depth

        Returns:
            Either a regular result or a AIFPLTailCall object for optimization
        """

        # Check most common self-evaluating types first (numbers are very common)
        if expr.is_self_evaluating():
            return expr

         # Symbol lookup
        if isinstance(expr, AIFPLSymbol):
            try:
                return env.lookup(expr.name)

            except AIFPLEvalError as e:
                # Add more context to symbol lookup errors
                available_vars = env.get_available_bindings()

                raise AIFPLEvalError(
                    message=f"Undefined variable: '{expr.name}'",
                    context=f"Available variables: {', '.join(sorted(available_vars)[:10])}"
                        "{'...' if len(available_vars) > 10 else ''}",
                    suggestion=f"Check spelling or define '{expr.name}' in a let binding",
                    example=f"(let (({expr.name} some-value)) ...)"
                ) from e

        # If this isn't a list, evaluate normally
        if not isinstance(expr, AIFPLList):
            raise AIFPLEvalError(f"Unexpected expression type: {type(expr)}")

        # Empty list evaluates to itself
        if expr.is_empty():
            return expr

        first_elem = expr.first()
        if isinstance(first_elem, AIFPLSymbol):
            # Handle special forms BEFORE attempting any symbol lookup
            if self._is_symbol_with_name(first_elem, 'quote'):
                return self._evaluate_quote_form(expr, env, depth + 1)

            if self._is_symbol_with_name(first_elem, 'if'):
                return self._evaluate_if_form(expr, env, depth + 1, True)

            if self._is_symbol_with_name(first_elem, 'lambda'):
                return self._evaluate_lambda_form(expr, env, depth + 1)

            if self._is_symbol_with_name(first_elem, 'let'):
                return self._evaluate_let_form(expr, env, depth + 1, True)

            if self._is_symbol_with_name(first_elem, "match"):
                return self.pattern_matcher.evaluate_match_form(expr, env, depth + 1, self._evaluate_expression)

        # Check for tail calls
        func_value = self._evaluate_expression(first_elem, env, depth + 1)

        # If it's not a lambda function, evaluate normally
        if not isinstance(func_value, AIFPLFunction):
            return self._evaluate_function_call(expr, env, depth + 1)

        # Check for recursion (simple or mutual)
        if id(func_value) not in self.call_chain_set:
            return self._evaluate_function_call(expr, env, depth + 1)

        # This is a recursive call (simple or mutual)!
        arg_exprs = list(expr.elements[1:])
        return AIFPLTailCall(
            function=first_elem,
            arguments=arg_exprs,
            environment=env
        )

    def _evaluate_if_form(
        self,
        if_list: AIFPLList,
        env: AIFPLEnvironment,
        depth: int,
        in_tail_position: bool = False
    ) -> AIFPLValue:
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
            raise AIFPLEvalError(
                message="If expression has wrong number of arguments",
                received=f"Got {if_list.length() - 1} arguments: {self.format_result(if_list)}",
                expected="Exactly 3 arguments: (if condition then else)",
                example="(if (> x 0) \"positive\" \"negative\")",
                suggestion="If needs condition, then-branch, and else-branch"
            )

        condition_expr = if_list.get(1)
        then_expr = if_list.get(2)
        else_expr = if_list.get(3)

        # Evaluate condition (not in tail position)
        condition = self._evaluate_expression(condition_expr, env, depth + 1)

        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError(
                message="If condition must be boolean",
                received=f"Condition: {self.format_result(condition)} ({condition.type_name()})",
                expected="Boolean value (#t or #f)",
                example="(if (> x 0) \"positive\" \"negative\")",
                suggestion="Use comparison operators like =, <, >, or boolean functions like and, or"
            )

        # Evaluate chosen branch (in tail position)
        if not in_tail_position:
            if condition.value:
                return self._evaluate_expression(then_expr, env, depth + 1)

            return self._evaluate_expression(else_expr, env, depth + 1)

        if condition.value:
            return self._evaluate_expression_with_tail_detection(then_expr, env, depth + 1)

        return self._evaluate_expression_with_tail_detection(else_expr, env, depth + 1)

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
        if isinstance(func_value, AIFPLFunction):
            result = self._call_lambda_function(cast(AIFPLFunction, func_value), arg_values, env, depth)
            assert not isinstance(result, AIFPLTailCall), (
                "Tail calls should not propagate out of higher-order function calls"
            )
            return result

        raise AIFPLEvalError(
            message="Cannot call non-function value in higher-order context",
            received=f"Trying to call: {self.format_result(func_value)} ({func_value.type_name()})",
            expected="Function (builtin or lambda)",
            example="(map (lambda (x) (* x 2)) (list 1 2 3))",
            suggestion="Provide a function as the first argument to higher-order functions"
        )

    def _builtin_and_special(self, args: List[AIFPLValue], env: AIFPLEnvironment, depth: int) -> AIFPLBoolean:
        """Handle AND with short-circuit evaluation."""
        # Empty AND returns True (identity)
        if not args:
            return AIFPLBoolean(True)

        # Evaluate arguments one by one, short-circuiting on first False
        for i, arg in enumerate(args):
            result = self._evaluate_expression(arg, env, depth + 1)

            # Validate that result is boolean
            if not isinstance(result, AIFPLBoolean):
                raise AIFPLEvalError(
                    message=f"And operator argument {i+1} must be boolean",
                    received=f"Argument {i+1}: {self.format_result(result)} ({result.type_name()})",
                    expected="Boolean value (#t or #f)",
                    example="(and (> x 0) (< x 10))",
                    suggestion="Use comparison or boolean operators to create boolean values"
                )

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
        for i, arg in enumerate(args):
            result = self._evaluate_expression(arg, env, depth + 1)

            # Validate that result is boolean
            if not isinstance(result, AIFPLBoolean):
                raise AIFPLEvalError(
                    message=f"Or operator argument {i+1} must be boolean",
                    received=f"Argument {i+1}: {self.format_result(result)} ({result.type_name()})",
                    expected="Boolean value (#t or #f)",
                    example="(or (= x 0) (> x 10))",
                    suggestion="Use comparison or boolean operators to create boolean values"
                )

            # Short-circuit: if any argument is True, return True immediately
            if result.value:
                return AIFPLBoolean(True)

        # All arguments were False
        return AIFPLBoolean(False)

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

        if isinstance(result, AIFPLAList):
            # Format alist in LISP notation: (alist (key1 val1) (key2 val2) ...)
            if result.is_empty():
                return "(alist)"

            formatted_pairs = []
            for key, value in result.pairs:
                formatted_key = self.format_result(key)
                formatted_value = self.format_result(value)
                formatted_pairs.append(f"({formatted_key} {formatted_value})")

            pairs_str = ' '.join(formatted_pairs)
            return f"(alist {pairs_str})"

        if isinstance(result, AIFPLFunction):
            # Use the describe method which handles both native and user-defined
            if result.is_native:
                return f"<builtin {result.name}>"

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

    def _is_close_to_nice_number(self, value: float) -> float | None:
        """Check if a float is very close to a 'nice' number and return the nice number if so."""
        # Check if it's close to common fractions with small denominators
        for denominator in range(1, 11):  # Check denominators 1-10
            for numerator in range(-50, 51):  # Check reasonable range
                nice_value = numerator / denominator
                if abs(value - nice_value) < self.floating_point_tolerance:
                    return nice_value

        return None
