"""Class to handle conversation settings."""

from typing import Dict, List, Any
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


class ConversationSettings:
    """Data class for conversation settings."""

    # Single dictionary of all available models
    MODELS = {
        # Anthropic models
        "claude-3-5-haiku-20241022": AIModel(
            name="claude-3-5-haiku-20241022",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=4096,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "claude-3-5-sonnet-20241022": AIModel(
            name="claude-3-5-sonnet-20241022",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "claude-3-7-sonnet-20250219": AIModel(
            name="claude-3-7-sonnet-20250219",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=64000,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "claude-3-7-sonnet-20250219 (thinking)": AIModel(
            name="claude-3-7-sonnet-20250219",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=64000,
            supports_temperature=False,
            reasoning_capabilities=ReasoningCapability.VISIBLE_REASONING
        ),

        # Deepseek models
        "deepseek-chat": AIModel(
            name="deepseek-chat",
            provider="deepseek",
            context_window=64000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "deepseek-reasoner": AIModel(
            name="deepseek-reasoner",
            provider="deepseek",
            context_window=64000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.VISIBLE_REASONING
        ),

        # Google models
        "gemini-1.5-flash": AIModel(
            name="gemini-1.5-flash",
            provider="google",
            context_window=1048576,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "gemini-1.5-pro": AIModel(
            name="gemini-1.5-pro",
            provider="google",
            context_window=2097152,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "gemini-2.0-flash-exp": AIModel(
            name="gemini-2.0-flash-exp",
            provider="google",
            context_window=1048576,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),

        # M6R models
        "tessa": AIModel(
            name="tessa",
            provider="m6r",
            context_window=1024,
            max_output_tokens=1024,
            supports_temperature=False,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),

        # Ollama models
        "llama3.2": AIModel(
            name="llama3.2",
            provider="ollama",
            context_window=2048,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "phi4": AIModel(
            name="phi4",
            provider="ollama",
            context_window=2048,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),

        # OpenAI models
        "gpt-4o-mini": AIModel(
            name="gpt-4o-mini",
            provider="openai",
            context_window=128000,
            max_output_tokens=16384,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "gpt-4o": AIModel(
            name="gpt-4o",
            provider="openai",
            context_window=128000,
            max_output_tokens=16384,
            supports_temperature=True,
            reasoning_capabilities=ReasoningCapability.NO_REASONING
        ),
        "o1-mini": AIModel(
            name="o1-mini",
            provider="openai",
            context_window=128000,
            max_output_tokens=65536,
            supports_temperature=False,
            reasoning_capabilities=ReasoningCapability.HIDDEN_REASONING
        ),
        "o1-preview": AIModel(
            name="o1-preview",
            provider="openai",
            context_window=200000,
            max_output_tokens=100000,
            supports_temperature=False,
            reasoning_capabilities=ReasoningCapability.HIDDEN_REASONING
        )
    }

    # Default fallback values for unknown models
    DEFAULT_CONTEXT_WINDOW = 8192
    DEFAULT_MAX_OUTPUT_TOKENS = 2048
    DEFAULT_REASONING_CAPABILITY = ReasoningCapability.NO_REASONING

    def __init__(
        self, model: str = "gemini-1.5-flash",
        temperature: float = 0.7,
        reasoning: ReasoningCapability = ReasoningCapability.NO_REASONING
    ):
        """
        Initialize conversation settings with defaults.

        Args:
            model: Optional model name. If None, must be set later based on available backends
            temperature: Temperature setting (0.0-1.0)
            reasoning: Reasoning capability

        Raises:
            ValueError: If temperature is out of valid range (0.0-1.0)
        """
        if not temperature:
            temperature = 0.7
        elif not 0 <= temperature <= 1:
            raise ValueError("Temperature must be between 0.0 and 1.0")

        self.model = model
        self.temperature = temperature
        self.reasoning = reasoning

        model_config = self.MODELS.get(model)
        if model_config:
            self.context_window = model_config.context_window
            self.max_output_tokens = model_config.max_output_tokens
        else:
            # Fallback for unknown models
            self.context_window = self.DEFAULT_CONTEXT_WINDOW
            self.max_output_tokens = self.DEFAULT_MAX_OUTPUT_TOKENS

    @classmethod
    def get_available_models(cls) -> List[str]:
        """
        Return list of available models.

        Returns:
            List of model names
        """
        return list(cls.MODELS.keys())

    @classmethod
    def get_name(cls, model: str) -> str:
        """
        Get the name for a given model.

        Args:
            model: Name of the model

        Returns:
            Model name or "unknown" if model not found
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.name

        return "unknown"

    @classmethod
    def get_provider(cls, model: str) -> str:
        """
        Get the provider for a given model.

        Args:
            model: Name of the model

        Returns:
            Provider name or "unknown" if model not found
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.provider

        return "unknown"

    @classmethod
    def supports_temperature(cls, model: str) -> bool:
        """
        Check if model supports temperature setting.

        Args:
            model: Name of the model

        Returns:
            True if the model supports temperature, False otherwise
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.supports_temperature

        return False

    @classmethod
    def get_model_limits(cls, model: str) -> Dict[str, int]:
        """
        Get the context window and max output tokens for a model.

        Args:
            model: Name of the model

        Returns:
            Dictionary with context_window and max_output_tokens keys
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return {
                "context_window": model_config.context_window,
                "max_output_tokens": model_config.max_output_tokens
            }

        return {
            "context_window": cls.DEFAULT_CONTEXT_WINDOW,
            "max_output_tokens": cls.DEFAULT_MAX_OUTPUT_TOKENS
        }

    @classmethod
    def get_reasoning_capability(cls, model: str) -> ReasoningCapability:
        """
        Get the reasoning capabilities supported by a model.

        Args:
            model: Name of the model

        Returns:
            ReasoningCapability bitmap of supported reasoning capabilities

        Raises:
            KeyError: If the model is not found
        """
        model_config = cls.MODELS.get(model)
        print(f"get reasoning for {model}: {model_config.reasoning_capabilities.value}")
        if model_config:
            return model_config.reasoning_capabilities

        return cls.DEFAULT_REASONING_CAPABILITY

    @classmethod
    def iter_models_by_backends(cls, ai_backends: Dict[str, Any]):
        """
        Generator that yields models supported by available backends.

        Args:
            ai_backends: Dictionary of available AI backends

        Yields:
            Model names supported by the available backends
        """
        for model_name, model in cls.MODELS.items():
            if model.provider in ai_backends:
                yield model_name

    @classmethod
    def get_default_model(cls, ai_backends: Dict[str, Any]) -> str:
        """
        Get the default model based on available backends.

        Args:
            ai_backends: Dictionary of available AI backends

        Returns:
            The name of the default model to use
        """
        # Try Google first
        if "google" in ai_backends:
            return "gemini-1.5-flash"

        # Then Anthropic
        if "anthropic" in ai_backends:
            return "claude-3-5-haiku-20241022"

        # Then M6R
        if "m6r" in ai_backends:
            return "tessa"

        # Then Deepseek
        if "deepseek" in ai_backends:
            return "deepseek-chat"

        # Finally OpenAI
        if "openai" in ai_backends:
            return "o1-mini"

        # Shouldn't happen as we require at least one backend
        return "gemini-1.5-flash"
