"""
How a conversation's turns are written to the interaction log.

A conversation is recorded by its tab when it has one, and by Mission Control
when it runs without one.  Both use these functions, so the same turn is recorded
the same way whichever is recording it.
"""

from ai import AIMessage, AIMessageSource
from ai_tool import AIToolCall


LOG_PREVIEW_CHARS = 120


def summarize_for_log(text: str) -> str:
    """
    Return a single-line excerpt of text, for the interaction log.

    Args:
        text: Text to summarise.

    Returns:
        The text flattened onto one line, with a trailing ellipsis when shortened.
    """
    flat = " ".join(text.split())
    if len(flat) <= LOG_PREVIEW_CHARS:
        return flat

    return f"{flat[:LOG_PREVIEW_CHARS]}..."


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


def completed_message_entry(message: AIMessage) -> str:
    """
    Return the interaction log entry for a completed message.

    Prompts, reasoning and responses are recorded as they complete; tool results
    and messages of any other kind are not.

    Args:
        message: The message that was completed

    Returns:
        The entry to record, or "" if the message is not recorded
    """
    content = summarize_for_log(message.content)

    if message.source == AIMessageSource.REASONING:
        return f"AI reasoning: '{content}'" if content else ""

    if message.source == AIMessageSource.AI:
        if content:
            return f"AI response: '{content}'"

        if message.tool_calls:
            return f"AI requested tool calls: {', '.join(tool_call_label(tc) for tc in message.tool_calls)}"

        return ""

    if message.source == AIMessageSource.USER and not message.tool_results:
        if message.user_name:
            return f"AI '{message.user_name}' submitted prompt: '{content}'"

        attachment_note = f"\nattachments: {len(message.attachments)}" if message.attachments else ""
        return f"Human submitted prompt: '{content}'{attachment_note}"

    if message.source == AIMessageSource.USER_QUEUED:
        return f"Human queued prompt: '{content}'"

    return ""
