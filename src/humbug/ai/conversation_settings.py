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
        "gemini-1.5-pro",
        "gemini-2.0-flash-exp",

        # Anthropic models
        "claude-3-5-haiku-20241022",
        "claude-3-5-sonnet-20241022"
    ]

    PROVIDER_MAP = {
        "gpt-4o-mini": "openai",
        "gpt-4o": "openai",
        "o1-mini": "openai",
        "o1-preview": "openai",
        "gemini-1.5-flash": "google",
        "gemini-1.5-pro": "google",
        "gemini-2.0-flash-exp": "google",
        "claude-3-5-haiku-20241022": "anthropic",
        "claude-3-5-sonnet-20241022": "anthropic"
    }

    TEMPERATURE_SUPPORTED_MODELS = {
        "gpt-4o-mini": True,
        "gpt-4o": True,
        "o1-mini": False,
        "o1-preview": False,
        "gemini-1.5-flash": True,
        "gemini-1.5-pro": True,
        "gemini-2.0-flash-exp": True,
        "claude-3-5-haiku-20241022": True,
        "claude-3-5-sonnet-20241022": True
    }

    MODEL_LIMITS = {
        "gpt-4o-mini": {"context_window": 128000, "max_output_tokens": 16384},
        "gpt-4o": {"context_window": 128000, "max_output_tokens": 16384},
        "o1-mini": {"context_window": 128000, "max_output_tokens": 65536},
        "o1-preview": {"context_window": 200000, "max_output_tokens": 100000},
        "gemini-1.5-flash": {"context_window": 1048576, "max_output_tokens": 8192},
        "gemini-1.5-pro": {"context_window": 2097152, "max_output_tokens": 8192},
        "gemini-2.0-flash-exp": {"context_window": 1048576, "max_output_tokens": 8192},
        "claude-3-5-haiku-20241022": {"context_window": 200000, "max_output_tokens": 4096},
        "claude-3-5-sonnet-20241022": {"context_window": 200000, "max_output_tokens": 8192}
    }

    def __init__(self, model: str = "gpt-4o-mini", temperature: float = 0.7):
        """Initialize conversation settings with defaults."""
        self.model = model
        self.temperature = temperature if self.supports_temperature(model) else None
        model_limits = self.MODEL_LIMITS.get(model, {"context_window": 8192, "max_output_tokens": 2048})
        self.context_window = model_limits["context_window"]
        self.max_output_tokens = model_limits["max_output_tokens"]

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

    @classmethod
    def get_model_limits(cls, model: str) -> dict:
        """Get the context window and max output tokens for a model."""
        return cls.MODEL_LIMITS.get(model, {"context_window": 8192, "max_output_tokens": 2048})
