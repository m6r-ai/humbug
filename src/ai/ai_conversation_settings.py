"""Enhanced class to handle conversation settings with tool support."""

import json
import os
from typing import Dict, Generator, List, Any, Tuple

from ai.ai_model import AIModel, AIReasoningCapability, AIReasoningEffort, ToolCapability


class AIConversationSettings:
    """Data class for conversation settings."""

    # Registry keyed by (model_name, provider) — the canonical identity of a model.
    MODELS: Dict[Tuple[str, str], AIModel] = {
        # Anthropic models
        ("claude-haiku-4-5", "anthropic"): AIModel(
            name="claude-haiku-4-5",
            provider="anthropic",
            display_name="Claude Haiku 4.5",
            context_window=200000,
            max_output_tokens=32000,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("claude-sonnet-4-6", "anthropic"): AIModel(
            name="claude-sonnet-4-6",
            provider="anthropic",
            display_name="Claude Sonnet 4.6",
            context_window=1000000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
                AIReasoningEffort.MAX
            ],
            temperature_incompatible_efforts={AIReasoningEffort.HIGH},
        ),
        ("claude-opus-4-8", "anthropic"): AIModel(
            name="claude-opus-4-8",
            provider="anthropic",
            display_name="Claude Opus 4.8",
            context_window=1000000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
                AIReasoningEffort.XHIGH,
                AIReasoningEffort.MAX
            ],
            adaptive_thinking_only=True,
        ),
        ("claude-opus-4-7", "anthropic"): AIModel(
            name="claude-opus-4-7",
            provider="anthropic",
            display_name="Claude Opus 4.7",
            context_window=1000000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
                AIReasoningEffort.XHIGH,
                AIReasoningEffort.MAX
            ],
            adaptive_thinking_only=True,
        ),

        # Deepseek models
        ("deepseek-v4-flash", "deepseek"): AIModel(
            name="deepseek-v4-flash",
            provider="deepseek",
            display_name="DeepSeek V4 Flash",
            context_window=1000000,
            max_output_tokens=384000,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("deepseek-v4-pro", "deepseek"): AIModel(
            name="deepseek-v4-pro",
            provider="deepseek",
            display_name="DeepSeek V4 Pro",
            context_window=1000000,
            max_output_tokens=384000,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),

        # Google models
        ("gemini-3.1-flash-lite", "google"): AIModel(
            name="gemini-3.1-flash-lite",
            provider="google",
            display_name="Gemini 3.1 Flash Lite",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MINIMAL,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
            ],
        ),
        ("gemini-3.5-flash", "google"): AIModel(
            name="gemini-3.5-flash",
            provider="google",
            display_name="Gemini 3.5 Flash",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MINIMAL,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
            ],
        ),
        ("gemini-3.1-pro-preview", "google"): AIModel(
            name="gemini-3.1-pro-preview",
            provider="google",
            display_name="Gemini 3.1 Pro Preview",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
            ],
        ),

        # Mistral models
        ("devstral-small-latest", "mistral"): AIModel(
            name="devstral-small-latest",
            provider="mistral",
            display_name="Devstral Small",
            context_window=131072,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
        ),
        ("codestral-latest", "mistral"): AIModel(
            name="codestral-latest",
            provider="mistral",
            display_name="Codestral",
            context_window=131072,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
        ),
        ("mistral-large-latest", "mistral"): AIModel(
            name="mistral-large-latest",
            provider="mistral",
            display_name="Mistral Large",
            context_window=262144,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
        ),
        ("mistral-small-latest", "mistral"): AIModel(
            name="mistral-small-latest",
            provider="mistral",
            display_name="Mistral Small",
            context_window=131072,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
        ),

        # Ollama local models
        ("gpt-oss:20b", "ollama-cloud"): AIModel(
            name="gpt-oss:20b",
            provider="ollama",
            display_name="GPT-OSS (20B)",
            context_window=131072,
            max_output_tokens=32768,  # This is actually 131072 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH
            ],
        ),
        ("qwen3.6:35b", "ollama"): AIModel(
            name="qwen3.6:35b",
            provider="ollama",
            display_name="Qwen 3.6 (35B)",
            context_window=256000,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),

        # Ollama Cloud models
        ("gpt-oss:20b", "ollama-cloud"): AIModel(
            name="gpt-oss:20b",
            provider="ollama-cloud",
            display_name="GPT-OSS (20B)",
            context_window=131072,
            max_output_tokens=32768,  # This is actually 131072 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH
            ],
        ),
        ("gpt-oss:120b", "ollama-cloud"): AIModel(
            name="gpt-oss:120b",
            provider="ollama-cloud",
            display_name="GPT-OSS (120B)",
            context_window=131072,
            max_output_tokens=32768,  # This is actually 131072 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH
            ],
        ),
        ("gemma4:31b-cloud", "ollama-cloud"): AIModel(
            name="gemma4:31b-cloud",
            provider="ollama-cloud",
            display_name="Gemma 4 (31B)",
            context_window=256000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("glm-5.2:cloud", "ollama-cloud"): AIModel(
            name="glm-5.2:cloud",
            provider="ollama-cloud",
            display_name="GLM 5.2",
            context_window=1000000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("glm-5.1:cloud", "ollama-cloud"): AIModel(
            name="glm-5.1:cloud",
            provider="ollama-cloud",
            display_name="GLM 5.1",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("kimi-k2.7-code:cloud", "ollama-cloud"): AIModel(
            name="kimi-k2.7-code:cloud",
            provider="ollama-cloud",
            display_name="Kimi K2.7 Code",
            context_window=262144,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.HIGH
            ],
        ),
        ("kimi-k2.6:cloud", "ollama-cloud"): AIModel(
            name="kimi-k2.6:cloud",
            provider="ollama-cloud",
            display_name="Kimi K2.6",
            context_window=262144,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("minimax-m3:cloud", "ollama-cloud"): AIModel(
            name="minimax-m3:cloud",
            provider="ollama-cloud",
            display_name="MiniMax M3",
            context_window=512000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.HIGH
            ],
        ),
        ("minimax-m2.7:cloud", "ollama-cloud"): AIModel(
            name="minimax-m2.7:cloud",
            provider="ollama-cloud",
            display_name="MiniMax M2.7",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.HIGH
            ],
        ),
        ("mistral-large-3:675b-cloud", "ollama-cloud"): AIModel(
            name="mistral-large-3:675b-cloud",
            provider="ollama-cloud",
            display_name="Mistral Large 3",
            context_window=262144,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        ("qwen3.5:cloud", "ollama-cloud"): AIModel(
            name="qwen3.5:cloud",
            provider="ollama-cloud",
            display_name="Qwen 3.5",
            context_window=256000,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),

        # OpenAI models
        ("gpt-5.4-nano", "openai"): AIModel(
            name="gpt-5.4-nano",
            provider="openai",
            display_name="GPT 5.4 Nano",
            context_window=400000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MEDIUM
            ],
        ),
        ("gpt-5.4-mini", "openai"): AIModel(
            name="gpt-5.4-mini",
            provider="openai",
            display_name="GPT 5.4 Mini",
            context_window=400000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MEDIUM
            ],
        ),
        ("gpt-5.5", "openai"): AIModel(
            name="gpt-5.5",
            provider="openai",
            display_name="GPT 5.5",
            context_window=1000000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MEDIUM,
            ],
        ),
        ("gpt-5.4", "openai"): AIModel(
            name="gpt-5.4",
            provider="openai",
            display_name="GPT 5.4",
            context_window=1000000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.MEDIUM
            ],
        ),

        # vLLM models
        ("gemma3:27b", "vllm"): AIModel(
            name="gemma3:27b",
            provider="vllm",
            display_name="Gemma 3 (27B)",
            context_window=131072,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.NO_TOOLS
        ),

        # xAI models
        ("grok-4.3", "xai"): AIModel(
            name="grok-4.3",
            provider="xai",
            display_name="Grok 4.3",
            context_window=1000000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH
            ],
        ),

        # Z.ai models
        ("glm-5.2", "zai"): AIModel(
            name="glm-5.2",
            provider="zai",
            display_name="GLM 5.2",
            context_window=1000000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH,
                AIReasoningEffort.MAX
            ],
        ),
        ("glm-5.1", "zai"): AIModel(
            name="glm-5.1",
            provider="zai",
            display_name="GLM 5.1",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("glm-4.7", "zai"): AIModel(
            name="glm-4.7",
            provider="zai",
            display_name="GLM 4.7",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("glm-4.5-x", "zai"): AIModel(
            name="glm-4.5-x",
            provider="zai",
            display_name="GLM 4.5-X",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("glm-4.5-air", "zai"): AIModel(
            name="glm-4.5-air",
            provider="zai",
            display_name="GLM 4.5 Air",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
        ("glm-4.5-airx", "zai"): AIModel(
            name="glm-4.5-airx",
            provider="zai",
            display_name="GLM 4.5 AirX",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.HIGH
            ],
        ),
    }

    # Snapshot of keys that are built-in (set once at class definition time).
    _BUILTIN_MODEL_KEYS: frozenset = frozenset(MODELS.keys())

    # Keys added via load_user_config (accumulated across calls, never removed).
    _USER_CONFIG_MODEL_KEYS: set = set()

    # Default fallback values for unknown models
    DEFAULT_CONTEXT_WINDOW = 8192
    DEFAULT_MAX_OUTPUT_TOKENS = 2048
    DEFAULT_REASONING_CAPABILITY = AIReasoningCapability.NO_REASONING
    DEFAULT_TOOL_CAPABILITY = ToolCapability.NO_TOOLS

    def __init__(
        self,
        model: str = "",
        provider: str = "",
        temperature: float | None = 0.7,
        reasoning: AIReasoningCapability = AIReasoningCapability.NO_REASONING,
        reasoning_effort: str | None = None,
    ):
        """
        Initialize conversation settings with defaults.

        Args:
            model: The API model name (e.g. 'claude-sonnet-4-6')
            provider: The provider name (e.g. 'anthropic')
            temperature: Temperature setting (0.0-1.0)
            reasoning: Reasoning capability
            reasoning_effort: Selected reasoning effort level, or None for the model default.

        Raises:
            ValueError: If temperature is out of valid range (0.0-1.0)
        """
        if temperature is None:
            temperature = 0.7

        elif not 0 <= temperature <= 1:
            raise ValueError("Temperature must be between 0.0 and 1.0")

        self.model = model
        self.provider = provider
        self.temperature = temperature
        self.reasoning = reasoning
        self.reasoning_effort = reasoning_effort

        model_config = self.MODELS.get((model, provider))
        if model_config:
            self.context_window = model_config.context_window
            self.max_output_tokens = model_config.max_output_tokens

            # If no effort was specified, use the model's default
            if self.reasoning_effort is None:
                self.reasoning_effort = model_config.default_reasoning_effort()

        else:
            # Fallback for unknown models
            self.context_window = self.DEFAULT_CONTEXT_WINDOW
            self.max_output_tokens = self.DEFAULT_MAX_OUTPUT_TOKENS

    @classmethod
    def get_display_name(cls, model: str, provider: str) -> str:
        """
        Get the display name for a given model and provider.

        Falls back to a generated name if the model is not in the registry.

        Args:
            model: The API model name
            provider: The provider name

        Returns:
            Human-readable display name
        """
        model_config = cls.MODELS.get((model, provider))
        if model_config:
            return model_config.display_name

        if provider:
            return f"{model} ({provider})"

        return model

    @classmethod
    def get_available_models(cls) -> List[Tuple[str, str]]:
        """
        Return list of available (model, provider) keys.

        Returns:
            List of (model_name, provider) tuples
        """
        return list(cls.MODELS.keys())

    @classmethod
    def supports_temperature(cls, model: str, provider: str, reasoning_effort: str | None = None) -> bool:
        """
        Check if model supports temperature setting at the given reasoning effort level.

        Args:
            model: The API model name
            provider: The provider name
            reasoning_effort: The currently selected effort level, or None.

        Returns:
            True if the model supports temperature at that effort level, False otherwise
        """
        model_config = cls.MODELS.get((model, provider))
        if model_config:
            return model_config.supports_temperature_for_effort(reasoning_effort)

        return False

    @classmethod
    def supports_tools(cls, model: str, provider: str) -> bool:
        """Check if model supports tool calling."""
        model_config = cls.MODELS.get((model, provider))
        if model_config:
            return model_config.supports_tools()

        return False

    @classmethod
    def get_tool_capabilities(cls, model: str, provider: str) -> ToolCapability:
        """Get the tool capabilities for a model."""
        model_config = cls.MODELS.get((model, provider))
        if model_config:
            return model_config.tool_capabilities

        return cls.DEFAULT_TOOL_CAPABILITY

    @classmethod
    def get_model_limits(cls, model: str, provider: str) -> Dict[str, int]:
        """
        Get the context window and max output tokens for a model.

        Args:
            model: The API model name
            provider: The provider name

        Returns:
            Dictionary with context_window and max_output_tokens keys
        """
        model_config = cls.MODELS.get((model, provider))
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
    def get_reasoning_capability(cls, model: str, provider: str) -> AIReasoningCapability:
        """
        Get the reasoning capabilities supported by a model.

        Args:
            model: The API model name
            provider: The provider name

        Returns:
            AIReasoningCapability bitmap of supported reasoning capabilities
        """
        model_config = cls.MODELS.get((model, provider))
        if model_config:
            return model_config.reasoning_capabilities

        return cls.DEFAULT_REASONING_CAPABILITY

    @classmethod
    def get_supported_reasoning_efforts(cls, model: str, provider: str) -> List[str]:
        """
        Get the ordered list of reasoning effort levels supported by a model.

        Args:
            model: The API model name
            provider: The provider name

        Returns:
            List of effort level strings in ascending order, or empty list if
            the model does not support variable reasoning effort.
        """
        model_config = cls.MODELS.get((model, provider))
        if model_config:
            return list(model_config.supported_reasoning_efforts)

        return []

    @classmethod
    def iter_models_by_backends(
        cls, ai_backends: Dict[str, Any]
    ) -> Generator[Tuple[str, str], None, None]:
        """
        Generator that yields (model, provider) keys for models supported by available backends.

        Args:
            ai_backends: Dictionary of available AI backends

        Yields:
            (model_name, provider) tuples for models whose provider has an active backend
        """
        for key, model in cls.MODELS.items():
            if model.provider in ai_backends:
                yield key

    @classmethod
    def get_default_model(cls, ai_backends: Dict[str, Any]) -> Tuple[str, str]:
        """
        Get the default (model, provider) based on available backends.

        Args:
            ai_backends: Dictionary of available AI backends

        Returns:
            (model_name, provider) tuple for the default model
        """
        provider_preference = [
            "google", "anthropic", "deepseek", "mistral",
            "openai", "xai", "zai", "ollama-cloud", "ollama", "vllm",
        ]
        for provider in provider_preference:
            if provider not in ai_backends:
                continue

            for key, model in cls.MODELS.items():
                if model.provider == provider:
                    return key

        return next(iter(cls.MODELS))

    @classmethod
    def find_by_model_and_provider(cls, model: str, provider: str) -> AIModel | None:
        """
        Look up an AIModel by its API name and provider.

        Args:
            model: The API model name
            provider: The provider name

        Returns:
            AIModel if found, None otherwise
        """
        return cls.MODELS.get((model, provider))

    @classmethod
    def find_provider_for_model(cls, model: str) -> str | None:
        """
        Find the provider for a model name when only the model name is known.

        Used for migration of old transcripts/settings that lack a provider field.
        If multiple providers offer the same model name, returns the first match
        in registry insertion order.

        Args:
            model: The API model name

        Returns:
            Provider string if found, None otherwise
        """
        for (m, p) in cls.MODELS:
            if m == model:
                return p

        return None

    # Provider-level defaults for models fetched from an API (capabilities unknown).
    _PROVIDER_FETCH_DEFAULTS: Dict[str, Dict[str, Any]] = {
        "anthropic": {
            "context_window": 200000, "max_output_tokens": 32000,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "openai": {
            "context_window": 128000, "max_output_tokens": 16384,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "google": {
            "context_window": 1048576, "max_output_tokens": 65536,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "deepseek": {
            "context_window": 64000, "max_output_tokens": 8192,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "mistral": {
            "context_window": 128000, "max_output_tokens": 8192,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "ollama": {
            "context_window": 128000, "max_output_tokens": 8192,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "ollama-cloud": {
            "context_window": 128000, "max_output_tokens": 8192,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "xai": {
            "context_window": 131072, "max_output_tokens": 16384,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "zai": {
            "context_window": 128000, "max_output_tokens": 8192,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
        "vllm": {
            "context_window": 128000, "max_output_tokens": 8192,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.FUNCTION_CALLING,
        },
    }

    @classmethod
    def _is_fetched_key(cls, key: Tuple[str, str]) -> bool:
        """Return True if key was added via register_fetched_models (not built-in or user-config)."""
        return key not in cls._BUILTIN_MODEL_KEYS and key not in cls._USER_CONFIG_MODEL_KEYS

    @classmethod
    def get_fetched_models_by_provider(cls, provider: str) -> List[Tuple[str, str]]:
        """Return (model, provider) keys that were fetched (not built-in) for the given provider."""
        return [
            key for key, model in cls.MODELS.items()
            if model.provider == provider and cls._is_fetched_key(key)
        ]

    @classmethod
    def remove_fetched_model(cls, model: str, provider: str) -> bool:
        """
        Remove a fetched model from the registry.

        Built-in and user-config models cannot be removed.

        Args:
            model: The API model name
            provider: The provider name

        Returns:
            True if the model was found and removed, False otherwise.
        """
        key = (model, provider)
        if not cls._is_fetched_key(key) or key not in cls.MODELS:
            return False

        del cls.MODELS[key]
        return True

    @classmethod
    def register_fetched_models(
        cls, model_ids: List[str], provider: str
    ) -> Tuple[List[str], List[str]]:
        """
        Register models fetched from a provider API using provider-level defaults.

        Args:
            model_ids: List of model ID strings from the provider's list endpoint.
            provider: Provider name (e.g. "anthropic", "openai").

        Returns:
            Tuple of (newly_added, already_present) model ID lists.
        """
        defaults = cls._PROVIDER_FETCH_DEFAULTS.get(provider, {
            "context_window": cls.DEFAULT_CONTEXT_WINDOW,
            "max_output_tokens": cls.DEFAULT_MAX_OUTPUT_TOKENS,
            "supports_temperature": True,
            "reasoning_capabilities": AIReasoningCapability.NO_REASONING,
            "tool_capabilities": ToolCapability.NO_TOOLS,
        })
        newly_added: List[str] = []
        already_present: List[str] = []
        for model_id in model_ids:
            key = (model_id, provider)
            if key in cls.MODELS:
                already_present.append(model_id)

            else:
                cls.MODELS[key] = AIModel(
                    name=model_id,
                    provider=provider,
                    display_name=model_id,
                    context_window=defaults["context_window"],
                    max_output_tokens=defaults["max_output_tokens"],
                    supports_temperature=defaults["supports_temperature"],
                    reasoning_capabilities=defaults["reasoning_capabilities"],
                    tool_capabilities=defaults["tool_capabilities"],
                )
                newly_added.append(model_id)

        return newly_added, already_present

    @classmethod
    def save_fetched_models_cache(cls, path: str) -> None:
        """
        Persist fetched (non-built-in) model IDs to a JSON cache file.

        The cache stores {provider: [model_id, ...]} for all models that were
        added via register_fetched_models().

        Args:
            path: Absolute path for the cache JSON file.
        """
        cache: Dict[str, List[str]] = {}
        for (model_id, provider), _ in cls.MODELS.items():
            if cls._is_fetched_key((model_id, provider)) and provider in cls._PROVIDER_FETCH_DEFAULTS:
                cache.setdefault(provider, []).append(model_id)

        try:
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w", encoding="utf-8") as f:
                json.dump(cache, f, indent=2)

        except OSError:
            pass

    @classmethod
    def load_fetched_models_cache(cls, path: str) -> None:
        """
        Load and register models from a previously saved fetch cache.

        Models already present in MODELS are skipped silently.

        Args:
            path: Absolute path to the cache JSON file.
        """
        if not os.path.exists(path):
            return

        try:
            with open(path, encoding="utf-8") as f:
                cache = json.load(f)

        except (json.JSONDecodeError, OSError):
            return

        if not isinstance(cache, dict):
            return

        for provider, model_ids in cache.items():
            if isinstance(model_ids, list):
                cls.register_fetched_models(
                    [m for m in model_ids if isinstance(m, str)], provider
                )

    @classmethod
    def load_user_config(cls, path: str) -> List[str]:
        """
        Load user-defined AI model entries from a JSON config file and register them.

        The file is optional — if it does not exist the method returns an empty list.
        Each entry in the file's "models" array is validated before being added to
        MODELS.  All errors found across all entries are collected and returned so the
        caller can surface them to the user in a single message.

        Collision policy: a (name, provider) pair that already exists in MODELS
        (whether built-in or previously registered by this method) is treated as an error.

        Args:
            path: Absolute path to the JSON config file.

        Returns:
            A list of human-readable error strings.  Empty on full success.
        """
        if not os.path.exists(path):
            return []

        try:
            with open(path, encoding="utf-8") as f:
                raw = json.load(f)

        except json.JSONDecodeError as exc:
            return [f"user-ai-config.json: invalid JSON — {exc}"]

        except OSError as exc:
            return [f"user-ai-config.json: could not read file — {exc}"]

        if not isinstance(raw, dict):
            return ["user-ai-config.json: top-level value must be a JSON object"]

        models_raw = raw.get("models")
        if models_raw is None:
            return ['user-ai-config.json: missing required key "models"']

        if not isinstance(models_raw, list):
            return ['user-ai-config.json: "models" must be a JSON array']

        valid_providers = {
            "anthropic", "deepseek", "google", "mistral",
            "ollama", "ollama-cloud", "openai", "vllm", "xai", "zai",
        }
        reasoning_map: Dict[str, AIReasoningCapability] = {
            "NO_REASONING": AIReasoningCapability.NO_REASONING,
            "HIDDEN_REASONING": AIReasoningCapability.HIDDEN_REASONING,
            "VISIBLE_REASONING": AIReasoningCapability.VISIBLE_REASONING,
        }
        tool_map: Dict[str, ToolCapability] = {
            "NO_TOOLS": ToolCapability.NO_TOOLS,
            "FUNCTION_CALLING": ToolCapability.FUNCTION_CALLING,
            "PARALLEL_TOOLS": ToolCapability.PARALLEL_TOOLS,
        }

        errors: List[str] = []
        pending: List[Tuple[Tuple[str, str], AIModel]] = []
        seen_keys: set[Tuple[str, str]] = set()

        required_str_fields = ("display_name", "name", "provider")
        required_int_fields = ("context_window", "max_output_tokens")

        for idx, entry in enumerate(models_raw):
            prefix = f"user-ai-config.json models[{idx}]"

            if not isinstance(entry, dict):
                errors.append(f"{prefix}: each entry must be a JSON object")
                continue

            entry_errors: List[str] = []

            for field in required_str_fields:
                if field not in entry:
                    entry_errors.append(f'missing required field "{field}"')

                elif not isinstance(entry[field], str) or not entry[field].strip():
                    entry_errors.append(f'"{field}" must be a non-empty string')

            for field in required_int_fields:
                if field not in entry:
                    entry_errors.append(f'missing required field "{field}"')

                elif not isinstance(entry[field], int) or entry[field] <= 0:
                    entry_errors.append(f'"{field}" must be a positive integer')

            if "supports_temperature" not in entry:
                entry_errors.append('missing required field "supports_temperature"')

            elif not isinstance(entry["supports_temperature"], bool):
                entry_errors.append('"supports_temperature" must be a boolean')

            rc_raw = entry.get("reasoning_capabilities", "")
            if rc_raw not in reasoning_map:
                entry_errors.append(
                    f'"reasoning_capabilities" must be one of: {", ".join(sorted(reasoning_map))}'
                )

            tc_raw = entry.get("tool_capabilities", "")
            if tc_raw not in tool_map:
                entry_errors.append(
                    f'"tool_capabilities" must be one of: {", ".join(sorted(tool_map))}'
                )

            # Validate optional supported_reasoning_efforts list
            supported_efforts: List[str] = []
            if "supported_reasoning_efforts" in entry:
                efforts_raw = entry["supported_reasoning_efforts"]
                if not isinstance(efforts_raw, list):
                    entry_errors.append('"supported_reasoning_efforts" must be a JSON array')

                else:
                    for effort in efforts_raw:
                        if not isinstance(effort, str) or not AIReasoningEffort.is_valid(effort):
                            entry_errors.append(
                                f'"supported_reasoning_efforts" contains invalid value "{effort}"; '
                                f'must be one of: {", ".join(AIReasoningEffort.values())}'
                            )
                            break

                        supported_efforts.append(effort)

            # Validate optional temperature_incompatible_efforts set
            temperature_incompatible: set[str] = set()
            if "temperature_incompatible_efforts" in entry:
                incompatible_raw = entry["temperature_incompatible_efforts"]
                if not isinstance(incompatible_raw, list):
                    entry_errors.append('"temperature_incompatible_efforts" must be a JSON array')

                else:
                    for effort in incompatible_raw:
                        if not isinstance(effort, str) or not AIReasoningEffort.is_valid(effort):
                            entry_errors.append(
                                f'"temperature_incompatible_efforts" contains invalid value "{effort}"; '
                                f'must be one of: {", ".join(AIReasoningEffort.values())}'
                            )
                            break

                        temperature_incompatible.add(effort)

            if entry_errors:
                errors.append(f"{prefix}: " + "; ".join(entry_errors))
                continue

            provider: str = entry["provider"]
            if provider not in valid_providers:
                errors.append(
                    f'{prefix}: "provider" must be one of: {", ".join(sorted(valid_providers))}'
                )
                continue

            name: str = entry["name"]
            display_name: str = entry["display_name"]
            key: Tuple[str, str] = (name, provider)

            if key in cls.MODELS:
                errors.append(
                    f'{prefix}: model "{name}" for provider "{provider}" conflicts with an existing model'
                )
                continue

            if key in seen_keys:
                errors.append(
                    f'{prefix}: model "{name}" for provider "{provider}" is duplicated within the config file'
                )
                continue

            seen_keys.add(key)
            pending.append((
                key,
                AIModel(
                    name=name,
                    provider=provider,
                    display_name=display_name,
                    context_window=entry["context_window"],
                    max_output_tokens=entry["max_output_tokens"],
                    supports_temperature=entry["supports_temperature"],
                    reasoning_capabilities=reasoning_map[rc_raw],
                    tool_capabilities=tool_map[tc_raw],
                    supported_reasoning_efforts=supported_efforts if supported_efforts else None,
                    temperature_incompatible_efforts=temperature_incompatible if temperature_incompatible else None,
                ),
            ))

        if errors:
            return errors

        for key, model in pending:
            cls.MODELS[key] = model
            cls._USER_CONFIG_MODEL_KEYS.add(key)

        return []
