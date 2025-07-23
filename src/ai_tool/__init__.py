"""AI tool calling framework."""

# Re-export all classes for backward compatibility
from ai_tool.ai_tool import AITool, AIToolAuthorizationCallback
from ai_tool.ai_tool_call import AIToolCall
from ai_tool.ai_tool_config import AIToolConfig
from ai_tool.ai_tool_definition import AIToolDefinition
from ai_tool.ai_tool_manager import AIToolManager
from ai_tool.ai_tool_operation_definition import AIToolOperationDefinition
from ai_tool.ai_tool_parameter import AIToolParameter
from ai_tool.ai_tool_registered import AIToolRegistered
from ai_tool.ai_tool_result import AIToolResult
from ai_tool.ai_tool_exceptions import (
    AIToolAuthorizationDenied,
    AIToolExecutionError,
    AIToolTimeoutError,
)

__all__ = [
    "AITool",
    "AIToolCall",
    "AIToolConfig",
    "AIToolDefinition",
    "AIToolManager",
    "AIToolOperationDefinition",
    "AIToolParameter",
    "AIToolRegistered",
    "AIToolResult",
    "AIToolAuthorizationDenied",
    "AIToolExecutionError",
    "AIToolTimeoutError",
    "AIToolAuthorizationCallback",
]
