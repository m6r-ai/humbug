"""Enhanced class to handle conversation settings with tool support."""

import json
import os
from typing import Dict, List, Any, Generator

from ai.ai_model import AIModel, AIReasoningCapability, AIReasoningEffort, ToolCapability


class AIConversationSettings:
    """Data class for conversation settings."""

    # Single dictionary of all available models
    MODELS = {
        # Anthropic models
        "claude-haiku-4-5": AIModel(
            name="claude-haiku-4-5",
            provider="anthropic",
            context_window=200000,
            max_output_tokens=32000,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "claude-sonnet-4-6": AIModel(
            name="claude-sonnet-4-6",
            provider="anthropic",
            context_window=1000000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
            temperature_incompatible_efforts={AIReasoningEffort.HIGH},
        ),
        "claude-opus-4-7": AIModel(
            name="claude-opus-4-7",
            provider="anthropic",
            context_window=1000000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "claude-opus-4-6": AIModel(
            name="claude-opus-4-6",
            provider="anthropic",
            context_window=1000000,
            max_output_tokens=32000,  # This is actually 64000 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
            temperature_incompatible_efforts={AIReasoningEffort.HIGH},
        ),

        # Deepseek models
        "deepseek-v4-flash": AIModel(
            name="deepseek-v4-flash",
            provider="deepseek",
            context_window=1000000,
            max_output_tokens=384000,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "deepseek-v4-pro": AIModel(
            name="deepseek-v4-pro",
            provider="deepseek",
            context_window=1000000,
            max_output_tokens=384000,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),

        # Google models
        "gemini-3.1-flash-lite": AIModel(
            name="gemini-3.1-flash-lite",
            provider="google",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MINIMAL,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
            ]
        ),
        "gemini-3-flash-preview": AIModel(
            name="gemini-3-flash-preview",
            provider="google",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MINIMAL,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
            ]
        ),
        "gemini-3.1-pro-preview": AIModel(
            name="gemini-3.1-pro-preview",
            provider="google",
            context_window=1048576,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
            ]
        ),

        # Mistral models
        "devstral-small-latest": AIModel(
            name="devstral-small-latest",
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
            context_window=131072,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "mistral-large-latest": AIModel(
            name="mistral-large-latest",
            provider="mistral",
            context_window=262144,
            max_output_tokens=65536,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "mistral-small-latest": AIModel(
            name="mistral-small-latest",
            provider="mistral",
            context_window=131072,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),

        # Ollama models
        "gemma4:31b-cloud (Ollama)": AIModel(
            name="gemma4:31b-cloud",
            provider="ollama",
            context_window=256000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "glm-5.1 (Ollama)": AIModel(
            name="glm-5.1:cloud",
            provider="ollama",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "glm-4.7 (Ollama)": AIModel(
            name="glm-4.7:cloud",
            provider="ollama",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "gpt-oss:20b": AIModel(
            name="gpt-oss:20b",
            provider="ollama",
            context_window=131072,
            max_output_tokens=32768,  # This is actually 131072 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.LOW, AIReasoningEffort.MEDIUM, AIReasoningEffort.HIGH],
        ),
        "gpt-oss:120b": AIModel(
            name="gpt-oss:120b",
            provider="ollama",
            context_window=131072,
            max_output_tokens=32768,  # This is actually 131072 but that's too much
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.LOW, AIReasoningEffort.MEDIUM, AIReasoningEffort.HIGH],
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
        "kimi-k2.6": AIModel(
            name="kimi-k2.6:cloud",
            provider="ollama",
            context_window=262144,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "minimax-m2.7": AIModel(
            name="minimax-m2.7:cloud",
            provider="ollama",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.HIGH],
        ),
        "minimax-m2.5": AIModel(
            name="minimax-m2.5:cloud",
            provider="ollama",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.HIGH],
        ),
        "mistral-large-3 (Ollama)": AIModel(
            name="mistral-large-3:675b-cloud",
            provider="ollama",
            context_window=262144,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING
        ),
        "qwen3.6": AIModel(
            name="qwen3.6:35b",
            provider="ollama",
            context_window=256000,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "qwen3.5": AIModel(
            name="qwen3.5:cloud",
            provider="ollama",
            context_window=256000,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),

        # OpenAI models
        "gpt-5.4-nano": AIModel(
            name="gpt-5.4-nano",
            provider="openai",
            context_window=400000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MEDIUM,
            ]
        ),
        "gpt-5.4-mini": AIModel(
            name="gpt-5.4-mini",
            provider="openai",
            context_window=400000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MEDIUM,
            ]
        ),
        "gpt-5.5": AIModel(
            name="gpt-5.5",
            provider="openai",
            context_window=1000000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.MEDIUM,
            ]
        ),
        "gpt-5.4": AIModel(
            name="gpt-5.4",
            provider="openai",
            context_window=1000000,
            max_output_tokens=128000,
            supports_temperature=False,
            reasoning_capabilities=AIReasoningCapability.HIDDEN_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.MEDIUM,
            ]
        ),

        # vLLM models
        "gemma3:27b": AIModel(
            name="gemma3:27b",
            provider="vllm",
            context_window=131072,
            max_output_tokens=2048,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.NO_REASONING,
            tool_capabilities=ToolCapability.NO_TOOLS
        ),

        # xAI models
        "grok-4.3": AIModel(
            name="grok-4.3",
            provider="xai",
            context_window=1000000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[
                AIReasoningEffort.NONE,
                AIReasoningEffort.LOW,
                AIReasoningEffort.MEDIUM,
                AIReasoningEffort.HIGH,
            ]
        ),

        # Z.ai models
        "glm-5.1 (Z.ai)": AIModel(
            name="glm-5.1",
            provider="zai",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "glm-4.7 (Z.ai)": AIModel(
            name="glm-4.7",
            provider="zai",
            context_window=200000,
            max_output_tokens=32768,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "glm-4.5-X": AIModel(
            name="glm-4.5-x",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "glm-4.5-air": AIModel(
            name="glm-4.5-air",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
        "glm-4.5-airx": AIModel(
            name="glm-4.5-airx",
            provider="zai",
            context_window=128000,
            max_output_tokens=8192,
            supports_temperature=True,
            reasoning_capabilities=AIReasoningCapability.VISIBLE_REASONING,
            tool_capabilities=ToolCapability.FUNCTION_CALLING,
            supported_reasoning_efforts=[AIReasoningEffort.NONE, AIReasoningEffort.HIGH],
        ),
    }

    # Default fallback values for unknown models
    DEFAULT_CONTEXT_WINDOW = 8192
    DEFAULT_MAX_OUTPUT_TOKENS = 2048
    DEFAULT_REASONING_CAPABILITY = AIReasoningCapability.NO_REASONING
    DEFAULT_TOOL_CAPABILITY = ToolCapability.NO_TOOLS

    def __init__(
        self, model: str = "gemini-1.5-flash",
        temperature: float | None = 0.7,
        reasoning: AIReasoningCapability = AIReasoningCapability.NO_REASONING,
        reasoning_effort: str | None = None,
    ):
        """
        Initialize conversation settings with defaults.

        Args:
            model: Optional model name. If None, must be set later based on available backends
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
        self.temperature = temperature
        self.reasoning = reasoning
        self.reasoning_effort = reasoning_effort

        model_config = self.MODELS.get(model)
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
    def supports_temperature(cls, model: str, reasoning_effort: str | None = None) -> bool:
        """
        Check if model supports temperature setting at the given reasoning effort level.

        Args:
            model: Name of the model
            reasoning_effort: The currently selected effort level, or None.

        Returns:
            True if the model supports temperature at that effort level, False otherwise
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.supports_temperature_for_effort(reasoning_effort)

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
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return model_config.reasoning_capabilities

        return cls.DEFAULT_REASONING_CAPABILITY

    @classmethod
    def get_supported_reasoning_efforts(cls, model: str) -> List[str]:
        """
        Get the ordered list of reasoning effort levels supported by a model.

        Args:
            model: Name of the model

        Returns:
            List of effort level strings in ascending order, or empty list if
            the model does not support variable reasoning effort.
        """
        model_config = cls.MODELS.get(model)
        if model_config:
            return list(model_config.supported_reasoning_efforts)

        return []

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
            return "claude-haiku-4-5"

        if "deepseek" in ai_backends:
            return "deepseek-chat"

        if "mistral" in ai_backends:
            return "mistral-large-latest"

        if "ollama" in ai_backends:
            return "qwen3.5"

        if "openai" in ai_backends:
            return "gpt-5-mini"

        if "xai" in ai_backends:
            return "grok-4-0709"

        if "zai" in ai_backends:
            return "glm-4.7 (Z.ai)"

        # Shouldn't happen as we require at least one backend
        return "gemini-2.5-flash"

    @classmethod
    def load_user_config(cls, path: str) -> List[str]:
        """
        Load user-defined AI model entries from a JSON config file and register them.

        The file is optional — if it does not exist the method returns an empty list.
        Each entry in the file's "models" array is validated before being added to
        MODELS.  All errors found across all entries are collected and returned so the
        caller can surface them to the user in a single message.

        Collision policy: a display_name that already exists in MODELS (whether built-in
        or previously registered by this method) is treated as an error.

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
            "ollama", "openai", "vllm", "xai", "zai",
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
        pending: List[tuple[str, AIModel]] = []
        seen_display_names: set[str] = set()

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

            display_name: str = entry["display_name"]

            if display_name in cls.MODELS:
                errors.append(
                    f'{prefix}: display_name "{display_name}" conflicts with an existing model'
                )
                continue

            if display_name in seen_display_names:
                errors.append(
                    f'{prefix}: display_name "{display_name}" is duplicated within the config file'
                )
                continue

            provider: str = entry["provider"]
            if provider not in valid_providers:
                errors.append(
                    f'{prefix}: "provider" must be one of: {", ".join(sorted(valid_providers))}'
                )
                continue

            seen_display_names.add(display_name)
            pending.append((
                display_name,
                AIModel(
                    name=entry["name"],
                    provider=provider,
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

        for display_name, model in pending:
            cls.MODELS[display_name] = model

        return []
