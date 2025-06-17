"""Enhanced AI response with tool support."""

from dataclasses import dataclass
from typing import Dict, List

from humbug.ai.ai_tool_manager import ToolCall
from humbug.ai.ai_usage import AIUsage


@dataclass
class AIError:
    """Error information from AI backend responses."""
    code: str
    message: str
    retries_exhausted: bool = False
    details: Dict | None = None


@dataclass
class AIResponse:
    """Response from an AI backend."""
    reasoning: str
    content: str
    usage: AIUsage | None = None
    error: AIError | None = None
    model: str | None = None
    temperature: float | None = None
    tool_calls: List[ToolCall] | None = None

    def has_tool_calls(self) -> bool:
        """Check if this response contains tool calls."""
        return self.tool_calls is not None and len(self.tool_calls) > 0
