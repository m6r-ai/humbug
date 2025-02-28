"""Class to handle AI model capabilities."""

from enum import Flag, auto


class ReasoningCapability(Flag):
    """Flag enum for reasoning capabilities supported by models."""

    NO_REASONING = auto()
    HIDDEN_REASONING = auto()
    VISIBLE_REASONING = auto()


class AIModel:
    """Base class for AI model configurations."""

    def __init__(
        self,
        name: str,
        provider: str,
        context_window: int,
        max_output_tokens: int,
        supports_temperature: bool,
        reasoning_capabilities: ReasoningCapability
    ):
        """
        Initialize an AI model configuration.

        Args:
            name: The name of the model
            provider: The provider of the model (e.g., 'anthropic', 'google')
            context_window: Maximum context window size in tokens
            max_output_tokens: Maximum output tokens the model can generate
            supports_temperature: Whether the model supports temperature settings
            reasoning_capabilities: Bitmap of reasoning capabilities supported by the model
        """
        self.name = name
        self.provider = provider
        self.context_window = context_window
        self.max_output_tokens = max_output_tokens
        self.supports_temperature = supports_temperature
        self.reasoning_capabilities = reasoning_capabilities
