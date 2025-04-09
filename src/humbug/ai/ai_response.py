"""Base class for AI backends."""

from dataclasses import dataclass
from typing import Dict

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
