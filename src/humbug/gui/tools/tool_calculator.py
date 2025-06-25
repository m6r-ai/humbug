from typing import Dict, Any

from humbug.ai.ai_tool_manager import AIToolDefinition, AIToolParameter, AITool, ToolExecutionError


class ToolCalculator(AITool):
    """Tool that performs basic mathematical calculations."""

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return AIToolDefinition(
            name="calculate",
            description="Perform basic mathematical calculations (addition, subtraction, multiplication, division)",
            parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="Mathematical expression to evaluate (e.g., '2 + 3 * 4')",
                    required=True
                )
            ]
        )

    async def execute(self, arguments: Dict[str, Any]) -> str:
        """Execute the calculator tool."""
        try:
            expression = arguments.get("expression", "")
            if not expression:
                raise ToolExecutionError(
                    "Expression is required",
                    "calculate", 
                    arguments
                )

            # Basic safety: only allow numbers, operators, parentheses, and whitespace
            allowed_chars = set("0123456789+-*/().% ")
            if not all(c in allowed_chars for c in expression):
                raise ToolExecutionError(
                    "Expression contains invalid characters",
                    "calculate",
                    arguments
                )

            # Evaluate the expression safely
            result = eval(expression)
            return str(result)

        except ZeroDivisionError as e:
            raise ToolExecutionError(
                "Division by zero",
                "calculate",
                arguments
            ) from e

        except Exception as e:
            raise ToolExecutionError(
                f"Failed to calculate expression: {str(e)}",
                "calculate",
                arguments
            ) from e
