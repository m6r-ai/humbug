"""Enhanced conversation state management with tool support."""

from enum import Enum


class AIMessageSource(Enum):
    """Enumeration of possible message sources."""
    USER = "user"
    AI_CONNECTED = "ai_connected"
    AI = "ai"
    REASONING = "reasoning"
    SYSTEM = "system"
    TOOL_CALL = "tool_call"
    TOOL_RESULT = "tool_result"
