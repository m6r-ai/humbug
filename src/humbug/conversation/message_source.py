"""Conversation state management for the Humbug application."""

from enum import Enum


class MessageSource(Enum):
    """Enumeration of possible message sources."""
    USER = "user"
    AI = "ai"
    SYSTEM = "system"
