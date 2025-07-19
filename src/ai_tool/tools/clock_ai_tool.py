from datetime import datetime, timezone

from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationCallback, AIToolResult, AIToolCall
)


class ClockAITool(AITool):
    """Clock tool that returns the current time."""

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return AIToolDefinition(
            name="get_current_time",
            description="Get the current date and time",
            parameters=[
                AIToolParameter(
                    name="format",
                    type="string",
                    description="Time format ('iso', 'human', or 'timestamp')",
                    required=False,
                    enum=["iso", "human", "timestamp"]
                ),
                AIToolParameter(
                    name="timezone",
                    type="string",
                    description="Timezone (e.g., 'UTC', 'America/New_York')",
                    required=False
                )
            ]
        )

    async def execute(
        self,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Execute the get current time tool."""
        arguments = tool_call.arguments
        try:
            format_type = arguments.get("format", "iso")

            now = datetime.now(timezone.utc)

            if format_type == "iso":
                content = now.isoformat()[:26] + "Z"

            elif format_type == "human":
                content = now.strftime("%Y-%m-%d %H:%M:%S UTC")

            elif format_type == "timestamp":
                content = str(int(now.timestamp()))

            else:
                content = now.isoformat()[:26] + "Z"

            return AIToolResult(
                id=tool_call.id,
                name="get_current_time",
                content=content
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get current time: {str(e)}") from e
