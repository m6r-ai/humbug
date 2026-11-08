"""Preview of the prompt that opened a conversation, for a panel tooltip."""

import textwrap

PROMPT_PREVIEW_CHARACTERS = 300
PROMPT_PREVIEW_LINES = 5
PROMPT_PREVIEW_LINE_CHARACTERS = 60


def format_prompt_preview(prompt: str) -> str:
    """
    Wrap the opening of a prompt into a few short lines.

    A tooltip is sized by its longest line, so an unwrapped prompt would stretch
    it across the screen.  Wrapping keeps it beside the item it describes while
    still showing enough of the prompt to recognise the conversation.  A prompt
    that does not fit is cut short with an ellipsis.

    Args:
        prompt: The full text of the conversation's first prompt.

    Returns:
        The preview, with its lines separated by newlines, or an empty string if
        the prompt holds nothing but whitespace.
    """
    collapsed = " ".join(prompt.split())
    if not collapsed:
        return ""

    truncated = len(collapsed) > PROMPT_PREVIEW_CHARACTERS
    lines = textwrap.wrap(collapsed[:PROMPT_PREVIEW_CHARACTERS], PROMPT_PREVIEW_LINE_CHARACTERS)
    if len(lines) > PROMPT_PREVIEW_LINES:
        del lines[PROMPT_PREVIEW_LINES:]
        truncated = True

    if truncated:
        lines[-1] += "…"

    return "\n".join(lines)
