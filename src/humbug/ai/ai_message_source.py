"""Conversation state management for the Humbug application."""

from enum import Enum


class AIMessageSource(Enum):
    """Enumeration of possible message sources."""
    USER = "user"
    AI = "ai"
    REASONING = "reasoning"
    SYSTEM = "system"
