"""Conversation settings"""


class ConversationSettings:
    """Data class for conversation settings."""

    def __init__(self, model: str = "gpt-4o-mini", temperature: float = 0.7):
        """Initialize conversation settings with defaults."""
        self.model = model
        self.temperature = temperature

