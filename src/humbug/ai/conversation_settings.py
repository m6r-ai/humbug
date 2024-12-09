class ConversationSettings:
    """Data class for conversation settings."""

    AVAILABLE_MODELS = [
        "gpt-4o-mini",
        "gpt-4o",
        "o1-mini",
        "o1-preview"
    ]

    TEMPERATURE_SUPPORTED_MODELS = {
        "gpt-4o-mini": True,
        "gpt-4o": True,
        "o1-mini": False,
        "o1-preview": False
    }

    def __init__(self, model: str = "gpt-4o-mini", temperature: float = 0.7):
        """Initialize conversation settings with defaults."""
        self.model = model
        self.temperature = temperature if self.supports_temperature(model) else None

    @classmethod
    def get_available_models(cls) -> list[str]:
        """Return list of available models."""
        return cls.AVAILABLE_MODELS

    @classmethod
    def supports_temperature(cls, model: str) -> bool:
        """Check if model supports temperature setting."""
        return cls.TEMPERATURE_SUPPORTED_MODELS.get(model, False)
