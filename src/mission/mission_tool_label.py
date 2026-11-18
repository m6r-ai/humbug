"""What a tool call acts on, shared by the mission modules."""

from ai_tool import AIToolCall


# Arguments naming what a call acts on, most specific first.  The last few name
# the work handed to another conversation, for calls that touch no file.
_TARGET_ARGUMENTS = (
    "path", "file_path", "output_path", "command", "keystrokes", "url", "ref", "destination", "task_prompt", "message"
)


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
