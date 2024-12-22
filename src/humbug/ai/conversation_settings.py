"""Class to handle conversation settings."""

class ConversationSettings:
    """Data class for conversation settings."""

    AVAILABLE_MODELS = [
        # OpenAI models
        "gpt-4o-mini",
        "gpt-4o",
        "o1-mini",
        "o1-preview",
        # Gemini models
        "gemini-1.5-flash",
        "gemini-1.5-pro"
    ]

    PROVIDER_MAP = {
        "gpt-4o-mini": "openai",
        "gpt-4o": "openai",
        "o1-mini": "openai",
        "o1-preview": "openai",
        "gemini-1.5-flash": "google",
        "gemini-1.5-pro": "google"
    }

    TEMPERATURE_SUPPORTED_MODELS = {
        "gpt-4o-mini": True,
        "gpt-4o": True,
        "o1-mini": False,
        "o1-preview": False,
        "gemini-1.5-flash": True,
        "gemini-1.5-pro": True
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
    def get_provider(cls, model: str) -> str:
        """Get the provider for a given model."""
        return cls.PROVIDER_MAP.get(model, "unknown")

    @classmethod
    def supports_temperature(cls, model: str) -> bool:
        """Check if model supports temperature setting."""
        return cls.TEMPERATURE_SUPPORTED_MODELS.get(model, False)
