"""Enhanced AI response with tool support."""

from dataclasses import dataclass
from typing import Dict, List

from ai.ai_usage import AIUsage
from ai_tool import AIToolCall


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
    connected: bool = False
    usage: AIUsage | None = None
    error: AIError | None = None
    model: str | None = None
    temperature: float | None = None
    tool_calls: List[AIToolCall] | None = None
    signature: str | None = None
    redacted_reasoning: str | None = None
