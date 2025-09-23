import ast
import asyncio
import cmath
import logging
import math
import operator
from typing import Any, Callable, cast

from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationCallback, AIToolTimeoutError, AIToolResult, AIToolCall
)


class SafeMathEvaluator:
    """Safe mathematical expression evaluator using AST parsing."""

    # Allowed unary operators
    UNARY_OPERATORS = {
        ast.UAdd: operator.pos,
        ast.USub: operator.neg,
        ast.Invert: operator.invert,  # ~ (bitwise NOT)
    }

    # Allowed binary operators
    BINARY_OPERATORS = {
        ast.Add: operator.add,
        ast.Sub: operator.sub,
        ast.Mult: operator.mul,
        ast.Div: operator.truediv,
        ast.FloorDiv: operator.floordiv,
        ast.Mod: operator.mod,
        ast.Pow: operator.pow,
        ast.BitOr: operator.or_,
        ast.BitXor: operator.xor,
        ast.BitAnd: operator.and_,
        ast.LShift: operator.lshift,
        ast.RShift: operator.rshift,
    }

    # Allowed mathematical functions (updated to use cmath where appropriate)
    ALLOWED_FUNCTIONS = {
        'abs': abs,
        'round': round,  # Used for real numbers only
        'min': min,
        'max': max,
        'pow': pow,
        'sqrt': cmath.sqrt,
        'sin': cmath.sin,
        'cos': cmath.cos,
        'tan': cmath.tan,
        'log': cmath.log,
        'log10': cmath.log10,
        'exp': cmath.exp,
        'floor': math.floor,  # Used for real numbers only
        'ceil': math.ceil,    # Used for real numbers only
        'bin': lambda x: bin(int(x)),
        'hex': lambda x: hex(int(x)),
        'oct': lambda x: oct(int(x)),
    }

    # Maximum recursion depth to prevent stack overflow
    MAX_DEPTH = 100

    # Tolerance for considering imaginary part as zero
    IMAGINARY_TOLERANCE = 1e-10

    def __init__(self) -> None:
        """Initialize the safe math evaluator."""
        self._depth = 0

    def evaluate(self, expression: str) -> int | float | complex | str:
        """
        Safely evaluate a mathematical expression.

        Args:
            expression: Mathematical expression to evaluate

        Returns:
            Result of the mathematical expression (simplified to real if imaginary part is negligible)

        Raises:
            ValueError: If expression is invalid or contains unsafe operations
            ZeroDivisionError: If division by zero occurs
            OverflowError: If result is too large to represent
        """
        if not expression.strip():
            raise ValueError("Expression cannot be empty")

        try:
            # Parse the expression into an AST
            tree = ast.parse(expression, mode='eval')

            # Reset depth counter for each evaluation
            self._depth = 0

            # Evaluate the AST safely
            result = self._eval_node(tree.body)

            # Check for overflow/invalid values (only for numeric results)
            if isinstance(result, complex):
                if (math.isinf(result.real) or math.isnan(result.real) or
                    math.isinf(result.imag) or math.isnan(result.imag)):
                    raise OverflowError("Result is too large or undefined")

            elif isinstance(result, float) and (math.isinf(result) or math.isnan(result)):
                raise OverflowError("Result is too large or undefined")

            # Simplify the result (but preserve strings from base conversion functions)
            if isinstance(result, str):
                return result

            return self._simplify_result(result)

        except SyntaxError as e:
            raise ValueError(f"Invalid mathematical expression: {e}") from e

    def _simplify_result(self, result: int | float | complex) -> int | float | complex:
        """
        Simplify complex results to real numbers when imaginary part is negligible.

        Args:
            result: The calculation result

        Returns:
            Simplified result (real number if imaginary part is effectively zero)
        """
        if isinstance(result, complex):
            # If imaginary part is effectively zero, return just the real part
            if abs(result.imag) < self.IMAGINARY_TOLERANCE:
                real_part = result.real
                # Convert to int if it's a whole number
                if isinstance(real_part, float) and real_part.is_integer():
                    return int(real_part)

                return real_part

        # For real numbers, convert float to int if it's a whole number
        if isinstance(result, float) and result.is_integer():
            return int(result)

        return result

    def _eval_node(self, node: ast.AST) -> int | float | complex | str:
        """
        Recursively evaluate an AST node.

        Args:
            node: AST node to evaluate

        Returns:
            Evaluated result

        Raises:
            ValueError: If node type is not allowed
        """
        # Prevent stack overflow from deeply nested expressions
        self._depth += 1
        if self._depth > self.MAX_DEPTH:
            raise ValueError("Expression is too deeply nested")

        try:
            # Handle different node types
            if isinstance(node, ast.Constant):
                return self._eval_constant(node)

            if isinstance(node, ast.BinOp):
                return self._eval_binop(node)

            if isinstance(node, ast.UnaryOp):
                return self._eval_unaryop(node)

            if isinstance(node, ast.Call):
                return self._eval_call(node)

            if isinstance(node, ast.Name):
                return self._eval_name(node)

            # If we get here, it's an unsupported node type
            raise ValueError(f"Unsupported operation: {type(node).__name__}")

        finally:
            self._depth -= 1

    def _eval_constant(self, node: ast.Constant) -> int | float | complex:
        """Evaluate a constant node."""
        # Bools are implemented as integers in Python, but we don't support them here
        if isinstance(node.value, bool):
            raise ValueError(f"Unsupported constant type: {type(node.value).__name__}")

        if not isinstance(node.value, (int, float, complex)):
            raise ValueError(f"Unsupported constant type: {type(node.value).__name__}")

        return node.value

    def _convert_to_integer_for_bitwise(self, value: int | float | complex, operation: str) -> int:
        """
        Convert a numeric value to integer for bitwise operations.

        Args:
            value: Numeric value to convert
            operation: Name of the bitwise operation (for error messages)

        Returns:
            Integer value

        Raises:
            ValueError: If value cannot be converted to integer for bitwise operations
        """
        if isinstance(value, complex):
            # Only allow complex numbers with zero imaginary part
            if abs(value.imag) >= self.IMAGINARY_TOLERANCE:
                raise ValueError(f"Bitwise operation '{operation}' does not support complex numbers")

            value = value.real

        if isinstance(value, float):
            # Only allow whole number floats
            if not value.is_integer():
                raise ValueError(f"Bitwise operation '{operation}' requires integer values, got float: {value}")

            value = int(value)

        return value

    def _eval_binop(self, node: ast.BinOp) -> int | float | complex:
        """Evaluate a binary operation node."""
        if type(node.op) not in self.BINARY_OPERATORS:
            raise ValueError(f"Unsupported binary operator: {type(node.op).__name__}")

        left = self._eval_node(node.left)
        right = self._eval_node(node.right)

        # Binary operations should only work on numbers, not strings
        if isinstance(left, str) or isinstance(right, str):
            raise ValueError("Binary operations cannot be performed on string results from base conversion functions")

        # Handle bitwise operations specially - they require integers
        if type(node.op) in (ast.BitOr, ast.BitXor, ast.BitAnd, ast.LShift, ast.RShift):
            operation_name = {
                ast.BitOr: '|',
                ast.BitXor: '^',
                ast.BitAnd: '&',
                ast.LShift: '<<',
                ast.RShift: '>>'
            }[type(node.op)]

            left = self._convert_to_integer_for_bitwise(left, operation_name)
            right = self._convert_to_integer_for_bitwise(right, operation_name)

            # Additional validation for shift operations
            if type(node.op) in (ast.LShift, ast.RShift):
                if right < 0:
                    raise ValueError(f"Shift count cannot be negative: {right}")

                # Prevent excessive shifts that could cause memory issues
                if right > 64:
                    raise ValueError(f"Shift count too large (max 64): {right}")

        op_func = self.BINARY_OPERATORS[type(node.op)]
        return op_func(left, right)

    def _eval_unaryop(self, node: ast.UnaryOp) -> int | float | complex:
        """Evaluate a unary operation node."""
        if type(node.op) not in self.UNARY_OPERATORS:
            raise ValueError(f"Unsupported unary operator: {type(node.op).__name__}")

        operand = self._eval_node(node.operand)

        # Unary operations should only work on numbers, not strings
        if isinstance(operand, str):
            raise ValueError("Unary operations cannot be performed on string results from base conversion functions")

        # Handle bitwise NOT specially - it requires integers
        if isinstance(node.op, ast.Invert):
            operand = self._convert_to_integer_for_bitwise(operand, '~')

        op_func = cast(Callable[[Any], int | float | complex], self.UNARY_OPERATORS[type(node.op)])
        return op_func(operand)

    def _eval_call(self, node: ast.Call) -> int | float | complex | str:
        """Evaluate a function call node."""
        if not isinstance(node.func, ast.Name):
            raise ValueError("Only simple function calls are allowed")

        func_name = node.func.id
        if func_name not in self.ALLOWED_FUNCTIONS:
            raise ValueError(f"Function '{func_name}' is not allowed")

        # Evaluate arguments
        args = [self._eval_node(arg) for arg in node.args]

        # Function arguments should only be numbers, not strings
        for i, arg in enumerate(args):
            if isinstance(arg, str):
                raise ValueError(f"Function '{func_name}' argument {i+1} cannot be a string result from base conversion")

        # Check for keyword arguments (not allowed for simplicity)
        if node.keywords:
            raise ValueError("Keyword arguments are not allowed in function calls")

        func = cast(Callable[..., int | float | complex | str], self.ALLOWED_FUNCTIONS[func_name])

        try:
            # Special handling for functions that don't support complex numbers
            if func_name in ('floor', 'ceil', 'round'):
                # These functions only work with real numbers
                for arg in args:
                    if isinstance(arg, complex) and abs(arg.imag) >= self.IMAGINARY_TOLERANCE:
                        raise ValueError(f"Function '{func_name}' does not support complex numbers")

                # Convert complex numbers with zero imaginary part to real
                args = [arg.real if isinstance(arg, complex) else arg for arg in args]

            # Special handling for base conversion functions - they need integers
            elif func_name in ('bin', 'hex', 'oct'):
                if len(args) != 1:
                    raise ValueError(f"Function '{func_name}' takes exactly 1 argument ({len(args)} given)")

                arg = args[0]
                if isinstance(arg, complex):
                    if abs(arg.imag) >= self.IMAGINARY_TOLERANCE:
                        raise ValueError(f"Function '{func_name}' does not support complex numbers")

                    arg = arg.real

                if isinstance(arg, float):
                    if not arg.is_integer():
                        raise ValueError(f"Function '{func_name}' requires integer values, got float: {arg}")

                    arg = int(arg)

                args = [arg]

            result = func(*args)
            assert isinstance(result, (int, float, complex, str)), f"Function '{func_name}' must return a number or string"
            return result

        except (TypeError, ValueError) as e:
            raise ValueError(f"Error calling function '{func_name}': {e}") from e

        except OverflowError as e:
            raise OverflowError(f"Function '{func_name}' caused overflow: {e}") from e

    def _eval_name(self, node: ast.Name) -> int | float | complex:
        """Evaluate a name node (variables/constants)."""
        # Mathematical constants
        if node.id == 'pi':
            return math.pi

        if node.id == 'e':
            return math.e

        # Complex number unit
        if node.id == 'j':
            return 1j

        raise ValueError(f"Undefined variable or constant: '{node.id}'")


class CalculatorAITool(AITool):
    """Tool that performs mathematical calculations using safe AST evaluation."""

    def __init__(self) -> None:
        """Initialize the calculator tool."""
        self._evaluator = SafeMathEvaluator()
        self._logger = logging.getLogger("CalculatorAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return AIToolDefinition(
            name="calculator",
            description=(
                "The calculator tool lets you (the AI) evaluate mathematical expressions. "
                "It supports the following:\n"
                "- Arithmetic operators: + - * / // % ** (add, subtract, multiply, divide, floor divide, modulo, power)\n"
                "- Bitwise operators: | ^ & << >> ~ (or, xor, and, left shift, right shift, not)\n"
                "- Trigonometry functions: sin cos tan\n"
                "- Logarithm functions: log log10 exp\n"
                "- Other functions: sqrt abs round min max pow floor ceil\n"
                "- Base conversion functions: bin hex oct\n"
                "- Constants: pi e j (imaginary unit)\n"
                "- Number literals in different bases: 0b1010 (binary), 0o755 (octal), 0xFF (hexadecimal)\n"
                "- Parentheses\n"
                "- Nested expressions\n"
                "- Complex numbers\n"
                "Important:\n"
                "- The ^ operator is for bitwise XOR and must not be used for exponentiation - use ** or pow() instead\n"
                "- Results are simplified to real numbers when imaginary part is negligible\n"
                "- Bitwise operations only work on integers\n"
                "- Results default to decimal unless using base conversion functions (bin, hex, oct)\n"
                "- Do not use it for anything other than mathematical calculations of the types described\n"
                "- Do not use it unless there is at least one operator or function in the expression\n"
                "The tool is useful where simple 'mental arithmetic' may be a problem, such as:\n"
                "- To generate results where accuracy is important\n"
                "- To handle operations that require more than 2 steps\n"
                "- To process numbers or expressions that are not commonly known\n"
                "- To evaluate results that may generate more than a few digits\n"
                "- To perform bitwise operations and base conversions"
            ),
            parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="A valid mathematical expression using only the calculator's supported capabilities",
                    required=True
                )
            ]
        )

    def _evaluate_expression(self, expression: str) -> str:
        """
        Synchronous helper for expression evaluation.

        Args:
            expression: Mathematical expression to evaluate

        Returns:
            String representation of the result

        Raises:
            Various math-related exceptions
        """
        result = self._evaluator.evaluate(expression)
        return str(result)

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the calculator tool with timeout protection and continuation support.

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
            self._logger.error("Calculator tool called without expression argument")
            raise AIToolExecutionError("Expression is required")

        # Validate expression is a string
        if not isinstance(expression, str):
            self._logger.error("Calculator tool called with non-string expression: %s", type(expression).__name__)
            raise AIToolExecutionError("Expression must be a string")

        try:
            self._logger.debug("Evaluating mathematical expression: %s", expression)

            # Run calculation with timeout protection
            try:
                result = await asyncio.wait_for(
                    asyncio.to_thread(self._evaluate_expression, expression),
                    timeout=5.0  # 5 seconds should be plenty for mathematical calculations
                )
            except asyncio.TimeoutError as e:
                self._logger.warning("Mathematical expression evaluation timed out: %s", expression)
                raise AIToolTimeoutError("Mathematical calculation timed out", 5.0) from e

            self._logger.debug("Expression evaluation successful: %s = %s", expression, result)

            return AIToolResult(
                id=tool_call.id,
                name="calculator",
                content=result
            )

        except AIToolTimeoutError:
            # Re-raise timeout errors
            raise

        except ZeroDivisionError as e:
            self._logger.warning("Division by zero in expression '%s'", expression, exc_info=True)
            raise AIToolExecutionError("Division by zero") from e

        except ValueError as e:
            self._logger.warning("Invalid expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Invalid mathematical expression: {str(e)}") from e

        except OverflowError as e:
            self._logger.warning("Calculation overflow in expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Calculation result is too large: {str(e)}") from e

        except Exception as e:
            self._logger.error("Unexpected error evaluating expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to calculate expression: {str(e)}") from e
