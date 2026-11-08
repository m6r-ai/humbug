"""Readable labels for tool calls, shared by the mission modules."""

from ai_tool import AIToolCall


def tool_call_label(tool_call: AIToolCall) -> str:
    """
    Return a readable label for a tool call, naming its operation if it has one.

    Args:
        tool_call: The call to describe

    Returns:
        The tool's name, followed by its operation when it has one
    """
    operation = tool_call.arguments.get("operation", "")
    return f"{tool_call.name}.{operation}" if operation else tool_call.name
