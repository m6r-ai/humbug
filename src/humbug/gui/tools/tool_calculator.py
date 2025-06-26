import ast
import logging
import math
import operator
from typing import Dict, Any, Union

from humbug.ai.ai_tool_manager import AIToolDefinition, AIToolParameter, AITool, ToolExecutionError


logger = logging.getLogger(__name__)


class SafeMathEvaluator:
    """Safe mathematical expression evaluator using AST parsing."""

    # Allowed binary operators
    BINARY_OPERATORS = {
        ast.Add: operator.add,
        ast.Sub: operator.sub,
        ast.Mult: operator.mul,
        ast.Div: operator.truediv,
        ast.FloorDiv: operator.floordiv,
        ast.Mod: operator.mod,
        ast.Pow: operator.pow,
    }

    # Allowed unary operators
    UNARY_OPERATORS = {
        ast.UAdd: operator.pos,
        ast.USub: operator.neg,
    }

    # Allowed mathematical functions
    ALLOWED_FUNCTIONS = {
        'abs': abs,
        'round': round,
        'min': min,
        'max': max,
        'sum': sum,
        'pow': pow,
        'sqrt': math.sqrt,
        'sin': math.sin,
        'cos': math.cos,
        'tan': math.tan,
        'log': math.log,
        'log10': math.log10,
        'exp': math.exp,
        'floor': math.floor,
        'ceil': math.ceil,
    }

    # Maximum recursion depth to prevent stack overflow
    MAX_DEPTH = 100

    def __init__(self):
        """Initialize the safe math evaluator."""
        self._depth = 0

    def evaluate(self, expression: str) -> Union[int, float]:
        """
        Safely evaluate a mathematical expression.

        Args:
            expression: Mathematical expression to evaluate

        Returns:
            Result of the mathematical expression

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

            # Ensure result is a number
            if not isinstance(result, (int, float)):
                raise ValueError(f"Expression must evaluate to a number, got {type(result).__name__}")

            # Check for overflow
            if isinstance(result, float) and (math.isinf(result) or math.isnan(result)):
                raise OverflowError("Result is too large or undefined")

            return result

        except SyntaxError as e:
            raise ValueError(f"Invalid mathematical expression: {e}") from e

        except RecursionError as e:
            raise ValueError("Expression is too complex (too deeply nested)") from e

    def _eval_node(self, node: ast.AST) -> Union[int, float | complex]:
        """
        Recursively evaluate an AST node.

        Args:
            node: AST node to evaluate

        Returns:
            Evaluated result

        Raises:
            ValueError: If node type is not allowed
            ZeroDivisionError: If division by zero occurs
            OverflowError: If computation exceeds limits
        """
        # Prevent stack overflow from deeply nested expressions
        self._depth += 1
        if self._depth > self.MAX_DEPTH:
            raise ValueError("Expression is too deeply nested")

        try:
            # Handle different node types
            if isinstance(node, ast.Constant):
                return self._eval_constant(node)

            if isinstance(node, ast.Constant):
                return node.n

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

    def _eval_constant(self, node: ast.Constant) -> Union[int, float]:
        """Evaluate a constant node."""
        if isinstance(node.value, (int, float)):
            return node.value
        raise ValueError(f"Unsupported constant type: {type(node.value).__name__}")

    def _eval_binop(self, node: ast.BinOp) -> Union[int, float]:
        """Evaluate a binary operation node."""
        if type(node.op) not in self.BINARY_OPERATORS:
            raise ValueError(f"Unsupported binary operator: {type(node.op).__name__}")

        left = self._eval_node(node.left)
        right = self._eval_node(node.right)
        op_func = self.BINARY_OPERATORS[type(node.op)]

        try:
            result = op_func(left, right)

            # Check for overflow
            if isinstance(result, float) and (math.isinf(result) or math.isnan(result)):
                raise OverflowError("Calculation result is too large or undefined")

            return result

        except ZeroDivisionError:
            raise ZeroDivisionError("Division by zero") from None

        except OverflowError as e:
            raise OverflowError(f"Calculation overflow: {e}") from e

    def _eval_unaryop(self, node: ast.UnaryOp) -> Union[int, float]:
        """Evaluate a unary operation node."""
        if type(node.op) not in self.UNARY_OPERATORS:
            raise ValueError(f"Unsupported unary operator: {type(node.op).__name__}")

        operand = self._eval_node(node.operand)
        op_func = self.UNARY_OPERATORS[type(node.op)]

        return op_func(operand)

    def _eval_call(self, node: ast.Call) -> Union[int, float]:
        """Evaluate a function call node."""
        if not isinstance(node.func, ast.Name):
            raise ValueError("Only simple function calls are allowed")

        func_name = node.func.id
        if func_name not in self.ALLOWED_FUNCTIONS:
            raise ValueError(f"Function '{func_name}' is not allowed")

        # Evaluate arguments
        args = [self._eval_node(arg) for arg in node.args]

        # Check for keyword arguments (not allowed for simplicity)
        if node.keywords:
            raise ValueError("Keyword arguments are not allowed in function calls")

        func = self.ALLOWED_FUNCTIONS[func_name]

        try:
            result = func(*args)

            # Ensure result is numeric
            if not isinstance(result, (int, float)):
                raise ValueError(f"Function '{func_name}' must return a number")

            return result

        except (TypeError, ValueError) as e:
            raise ValueError(f"Error calling function '{func_name}': {e}") from e

        except OverflowError as e:
            raise OverflowError(f"Function '{func_name}' caused overflow: {e}") from e

    def _eval_name(self, node: ast.Name) -> Union[int, float]:
        """Evaluate a name node (variables/constants)."""
        # Only allow mathematical constants
        if node.id == 'pi':
            return math.pi

        if node.id == 'e':
            return math.e

        raise ValueError(f"Undefined variable or constant: '{node.id}'")


class ToolCalculator(AITool):
    """Tool that performs mathematical calculations using safe AST evaluation."""

    def __init__(self):
        """Initialize the calculator tool."""
        self._evaluator = SafeMathEvaluator()

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return AIToolDefinition(
            name="calculate",
            description=(
                "Mathematical expression evaluator. "
                "Arithmetic: + - * / // % ** (add, subtract, multiply, divide, floor divide, modulo, power). "
                "Trigonometry: sin cos tan. "
                "Logarithms: log log10 exp. "
                "Other functions: sqrt abs round min max sum pow floor ceil. "
                "Constants: pi e. "
                "Supports parentheses and nested expressions."
            ),
            parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="Mathematical expression using listed operators/functions/constants",
                    required=True
                )
            ]
        )

    async def execute(self, arguments: Dict[str, Any]) -> str:
        """
        Execute the calculator tool.

        Args:
            arguments: Dictionary containing the expression to evaluate

        Returns:
            String representation of the calculation result

        Raises:
            ToolExecutionError: If calculation fails or expression is invalid
        """
        expression = arguments.get("expression", "")

        # Validate expression is provided
        if not expression:
            logger.error("Calculator tool called without expression argument")
            raise ToolExecutionError(
                "Expression is required",
                "calculate",
                arguments
            )

        # Validate expression is a string
        if not isinstance(expression, str):
            logger.error("Calculator tool called with non-string expression: %s", type(expression).__name__)
            raise ToolExecutionError(
                "Expression must be a string",
                "calculate",
                arguments
            )

        try:
            logger.debug("Evaluating mathematical expression: %s", expression)
            result = self._evaluator.evaluate(expression)
            logger.debug("Expression evaluation successful: %s = %s", expression, result)
            return str(result)

        except ZeroDivisionError as e:
            logger.warning("Division by zero in expression '%s'", expression, exc_info=True)
            raise ToolExecutionError(
                "Division by zero",
                "calculate",
                arguments
            ) from e

        except ValueError as e:
            logger.warning("Invalid expression '%s': %s", expression, str(e), exc_info=True)
            raise ToolExecutionError(
                f"Invalid mathematical expression: {str(e)}",
                "calculate",
                arguments
            ) from e

        except OverflowError as e:
            logger.warning("Calculation overflow in expression '%s': %s", expression, str(e), exc_info=True)
            raise ToolExecutionError(
                f"Calculation result is too large: {str(e)}",
                "calculate",
                arguments
            ) from e

        except Exception as e:
            logger.error("Unexpected error evaluating expression '%s': %s", expression, str(e), exc_info=True)
            raise ToolExecutionError(
                f"Failed to calculate expression: {str(e)}",
                "calculate",
                arguments
            ) from e
