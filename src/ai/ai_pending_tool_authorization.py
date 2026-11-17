"""Tool authorization request awaiting a human decision."""

from dataclasses import dataclass

from ai.ai_message import AIMessage
from ai_tool import AIToolApprovalKind, AIToolCall


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
        kind: What the request would change, as the tool classified it
    """
    message: AIMessage | None
    tool_call: AIToolCall
    reason: str
    context: str | None
    destructive: bool
    kind: AIToolApprovalKind = AIToolApprovalKind.STANDARD
