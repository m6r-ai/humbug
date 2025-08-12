"""Enhanced class to handle conversation settings with tool support."""

from typing import Dict, List, Any, Generator

from ai.ai_model import AIModel, AIReasoningCapability, ToolCapability


class AIConversationSettings:
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
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-sonnet-4-20250514": AIModel(
            name="claude-sonnet-4-20250514",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-sonnet-4-20250514 (thinking)": AIModel(
            name="claude-sonnet-4-20250514",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-3-7-sonnet-20250219": AIModel(
            name="claude-3-7-sonnet-20250219",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-3-7-sonnet-20250219 (thinking)": AIModel(
            name="claude-3-7-sonnet-20250219",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-3-5-sonnet-20241022": AIModel(
            name="claude-3-5-sonnet-20241022",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-opus-4-1-20250805": AIModel(
            name="claude-opus-4-1-20250805",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-opus-4-1-20250805 (thinking)": AIModel(
            name="claude-opus-4-1-20250805",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actuall 64000 but that's too much
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-opus-4-20250805": AIModel(
            name="claude-opus-4-20250805",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "claude-opus-4-20250514 (thinking)": AIModel(
            name="claude-opus-4-20250514",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,  # This is actuall 64000 but that's too much
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # Deepseek models
        "deepseek-chat": AIModel(
            name="deepseek-chat",
            provider="deepseek",
            context_window=65536,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "deepseek-reasoner": AIModel(
            name="deepseek-reasoner",
            provider="deepseek",
            context_window=65536,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # Google models
        "gemini-2.5-flash": AIModel(
            name="gemini-2.5-flash",
            provider="google",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gemini-2.0-flash": AIModel(
            name="gemini-2.0-flash",
            provider="google",
            context_window=1048576,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gemini-1.5-flash": AIModel(
            name="gemini-1.5-flash",
            provider="google",
            context_window=1048576,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gemini-2.5-pro": AIModel(
            name="gemini-2.5-pro",
            provider="google",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gemini-1.5-pro": AIModel(
            name="gemini-1.5-pro",
            provider="google",
            context_window=2097152,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # Mistral models
        "devstral-small-2505": AIModel(
            name="devstral-small-2505",
            provider="mistral",
            context_window=131072,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "codestral-latest": AIModel(
            name="codestral-latest",
            provider="mistral",
            context_window=262144,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "mistral-large-latest": AIModel(
            name="mistral-large-latest",
            provider="mistral",
            context_window=131072,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "mistral-small-latest": AIModel(
            name="mistral-small-latest",
            provider="mistral",
            context_window=32768,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # Ollama models
        "gemma3:4b-it-qat": AIModel(
            name="gemma3:4b-it-qat",
            provider="ollama",
            context_window=131072,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.NO_TOOLS
        ),
        "gpt-oss:20b": AIModel(
            name="gpt-oss:20b",
            provider="ollama",
            context_window=131072,
            max_output_tokens=32768,  # This is actually 131072 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-oss:120b": AIModel(
            name="gpt-oss:120b",
            provider="ollama",
            context_window=131072,
            max_output_tokens=32768,  # This is actually 131072 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "llama3.2": AIModel(
            name="llama3.2",
            provider="ollama",
            context_window=131072,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.NO_TOOLS
        ),
        "phi4": AIModel(
            name="phi4",
            provider="ollama",
            context_window=16384,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.NO_TOOLS
        ),
        "qwen3:4b": AIModel(
            name="qwen3:4b",
            provider="ollama",
            context_window=40960,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "qwen3:4b (thinking)": AIModel(
            name="qwen3:4b",
            provider="ollama",
            context_window=40960,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "qwen3:8b": AIModel(
            name="qwen3:8b",
            provider="ollama",
            context_window=40960,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "qwen3:8b (thinking)": AIModel(
            name="qwen3:8b",
            provider="ollama",
            context_window=40960,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # OpenAI models
        "gpt-5-nano": AIModel(
            name="gpt-5-nano",
            provider="openai",
            context_window=400000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-4.1-nano": AIModel(
            name="gpt-4.1-nano",
            provider="openai",
            context_window=1048576,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-5-mini": AIModel(
            name="gpt-5-mini",
            provider="openai",
            context_window=400000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-4.1-mini": AIModel(
            name="gpt-4.1-mini",
            provider="openai",
            context_window=1048576,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-4o-mini": AIModel(
            name="gpt-4o-mini",
            provider="openai",
            context_window=128000,
            max_output_tokens=16384,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-5": AIModel(
            name="gpt-5",
            provider="openai",
            context_window=400000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-4.1": AIModel(
            name="gpt-4.1",
            provider="openai",
            context_window=1048576,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "gpt-4o": AIModel(
            name="gpt-4o",
            provider="openai",
            context_window=128000,
            max_output_tokens=16384,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "o4-mini": AIModel(
            name="o4-mini",
            provider="openai",
            context_window=200000,
            max_output_tokens=100000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "o3-mini": AIModel(
            name="o3-mini",
            provider="openai",
            context_window=200000,
            max_output_tokens=100000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "o3": AIModel(
            name="o3",
            provider="openai",
            context_window=200000,
            max_output_tokens=100000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "o1": AIModel(
            name="o1",
            provider="openai",
            context_window=200000,
            max_output_tokens=100000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # xAI models
        "grok-3-mini": AIModel(
            name="grok-3-mini",
            provider="xai",
            context_window=131072,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "grok-3-mini-fast": AIModel(
            name="grok-3-mini-fast",
            provider="xai",
            context_window=131072,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "grok-4-0709": AIModel(
            name="grok-4-0709",
            provider="xai",
            context_window=256000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "grok-3": AIModel(
            name="grok-3",
            provider="xai",
            context_window=131072,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "grok-3-fast": AIModel(
            name="grok-3-fast",
            provider="xai",
            context_window=131072,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # Z.ai models
        "glm-4.5": AIModel(
            name="glm-4.5",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "glm-4.5-X": AIModel(
            name="glm-4.5-x",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "glm-4.5-air": AIModel(
            name="glm-4.5-air",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "glm-4.5-airx": AIModel(
            name="glm-4.5-airx",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "glm-4.5-flash": AIModel(
            name="glm-4.5-flash",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        )
    }

    # Default fallback values for unknown models
    DEFAULT_CONTEXT_WINDOW = 8192
    DEFAULT_MAX_OUTPUT_TOKENS = 2048
    DEFAULT_REASONING_CAPABILITY = AIReasoningCapability.NO_REASONING
    DEFAULT_TOOL_CAPABILITY = ToolCapability.NO_TOOLS

    def __init__(
        self, model: str = "gemini-1.5-flash",
        temperature: float | None = 0.7,
        reasoning: AIReasoningCapability = AIReasoningCapability.NO_REASONING
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
        if temperature is None:
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
    def supports_tools(cls, model: str) -> bool:
        """Check if model supports tool calling."""
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.supports_tools()

        return False

    @classmethod
    def get_tool_capabilities(cls, model: str) -> ToolCapability:
        """Get the tool capabilities for a model."""
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.tool_capabilities

        return cls.DEFAULT_TOOL_CAPABILITY

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
    def get_reasoning_capability(cls, model: str) -> AIReasoningCapability:
        """
        Get the reasoning capabilities supported by a model.

        Args:
            model: Name of the model

        Returns:
            AIReasoningCapability bitmap of supported reasoning capabilities

        Raises:
            KeyError: If the model is not found
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.reasoning_capabilities

        return cls.DEFAULT_REASONING_CAPABILITY

    @classmethod
    def iter_models_by_backends(cls, ai_backends: Dict[str, Any]) -> Generator[str, None, None]:
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
        if "google" in ai_backends:
            return "gemini-2.5-flash"

        if "anthropic" in ai_backends:
            return "claude-3-5-haiku-20241022"

        if "deepseek" in ai_backends:
            return "deepseek-chat"

        if "mistral" in ai_backends:
            return "mistral-large-latest"

        if "ollama" in ai_backends:
            return "qwen3:4b"

        if "openai" in ai_backends:
            return "gpt-4.1-mini"

        if "xai" in ai_backends:
            return "grok-4-0709"

        if "zai" in ai_backends:
            return "glm-4.5-flash"

        # Shouldn't happen as we require at least one backend
        return "gemini-2.5-flash"
