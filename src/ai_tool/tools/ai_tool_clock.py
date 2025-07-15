from datetime import datetime, timezone
from typing import Dict, Any

from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError, AIToolAuthorizationCallback
)


class AIToolClock(AITool):
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

    async def execute(self, arguments: Dict[str, Any], request_authorization: AIToolAuthorizationCallback) -> str:
        """Execute the get current time tool."""
        try:
            format_type = arguments.get("format", "iso")

            now = datetime.now(timezone.utc)

            if format_type == "iso":
                return now.isoformat()[:26] + "Z"

            if format_type == "human":
                return now.strftime("%Y-%m-%d %H:%M:%S UTC")

            if format_type == "timestamp":
                return str(int(now.timestamp()))

            return now.isoformat()[:26] + "Z"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to get current time: {str(e)}",
                "get_current_time",
                arguments
            ) from e
