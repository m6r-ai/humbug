"""Base class for AI backends."""

from dataclasses import dataclass
from typing import Dict, Optional

from humbug.ai.ai_usage import AIUsage


@dataclass
class AIError:
    """Error information from AI backend responses."""
    code: str
    message: str
    retries_exhausted: bool = False
    details: Optional[Dict] = None


@dataclass
class AIResponse:
    """Response from an AI backend."""
    content: str
    usage: Optional[AIUsage] = None
    error: Optional[AIError] = None
    model: Optional[str] = None
    temperature: Optional[float] = None
