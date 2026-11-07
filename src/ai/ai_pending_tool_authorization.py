"""Tool authorization request awaiting a human decision."""

from dataclasses import dataclass

from ai.ai_message import AIMessage
from ai_tool import AIToolCall


@dataclass(frozen=True)
class AIPendingToolAuthorization:
    """
    A tool authorization request that is waiting for a human decision.

    Attributes:
        message: The tool call message the request belongs to, if any
        tool_call: The tool call being authorized
        reason: Human-readable reason the tool gave for the request
        context: Optional extra detail shown alongside the request
        destructive: Whether the tool considers the operation destructive
    """
    message: AIMessage | None
    tool_call: AIToolCall
    reason: str
    context: str | None
    destructive: bool
