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


# Arguments naming what a call acts on, most specific first.
_TARGET_ARGUMENTS = ("path", "output_path", "command", "keystrokes", "url", "ref")


def tool_call_target(tool_call: AIToolCall) -> str:
    """
    Return what a tool call acts on, such as a file, a command or a URL.

    Args:
        tool_call: The call to describe

    Returns:
        The call's target, "source → destination" for a move or copy, or "" if it names none
    """
    arguments = tool_call.arguments
    path = arguments.get("path")
    destination = arguments.get("destination")
    if isinstance(path, str) and path.strip() and isinstance(destination, str) and destination.strip():
        return f"{path.strip()} → {destination.strip()}"

    for name in _TARGET_ARGUMENTS:
        value = arguments.get(name)
        if isinstance(value, str) and value.strip():
            return value.strip()

    return ""
