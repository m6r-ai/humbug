"""Enhanced AI model with tool calling capabilities."""

from enum import Flag, auto
from typing import List, Set


class AIReasoningEffort:
    """
    Reasoning effort level constants for models that support variable reasoning intensity.

    Values map directly to API parameter strings.  The ordering from lowest to highest
    is: NONE, MINIMAL, LOW, MEDIUM, HIGH, XHIGH, MAX.
    """

    NONE = "none"
    MINIMAL = "minimal"
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    XHIGH = "xhigh"
    MAX = "max"

    _VALUES = [NONE, MINIMAL, LOW, MEDIUM, HIGH, XHIGH, MAX]

    @classmethod
    def values(cls) -> List[str]:
        """Return all valid effort level strings in ascending order."""
        return list(cls._VALUES)

    @classmethod
    def is_valid(cls, value: str) -> bool:
        """Return True if value is a recognised effort level."""
        return value in cls._VALUES


class AIReasoningCapability(Flag):
    """Flag enum for reasoning capabilities supported by models."""

    NO_REASONING = auto()
    HIDDEN_REASONING = auto()
    VISIBLE_REASONING = auto()


class ToolCapability(Flag):
    """Flag enum for tool calling capabilities supported by models."""

    NO_TOOLS = auto()
    FUNCTION_CALLING = auto()
    PARALLEL_TOOLS = auto()  # Can call multiple tools simultaneously


class AIModel:
    """AI model configuration."""

    def __init__(
        self,
        name: str,
        provider: str,
        display_name: str,
        context_window: int,
        max_output_tokens: int,
        supports_temperature: bool,
        reasoning_capabilities: AIReasoningCapability,
        tool_capabilities: ToolCapability = ToolCapability.NO_TOOLS,
        supported_reasoning_efforts: List[str] | None = None,
        temperature_incompatible_efforts: Set[str] | None = None,
        adaptive_thinking_only: bool = False,
    ):
        """
        Initialize an AI model configuration.

        Args:
            name: The API model name sent to the provider
            provider: The provider of the model (e.g., 'anthropic', 'google')
            display_name: The human-readable name shown in the UI
            context_window: Maximum context window size in tokens
            max_output_tokens: Maximum output tokens the model can generate
            supports_temperature: Whether the model supports temperature settings
            reasoning_capabilities: Bitmap of reasoning capabilities supported by the model
            tool_capabilities: Bitmap of tool calling capabilities supported by the model
            supported_reasoning_efforts: Ordered list of effort levels this model accepts.
                Empty list means the model does not support variable reasoning effort.
                By convention the first entry is the default (lowest/off) level.
            temperature_incompatible_efforts: Set of effort levels that disable temperature
                support (e.g. Anthropic thinking mode disables temperature).
            adaptive_thinking_only: If True, the model only supports adaptive thinking
                (thinking: {type: "adaptive"}) and rejects manual budget-based thinking.
        """
        self.name = name
        self.provider = provider
        self.display_name = display_name
        self.context_window = context_window
        self.max_output_tokens = max_output_tokens
        self.supports_temperature = supports_temperature
        self.reasoning_capabilities = reasoning_capabilities
        self.tool_capabilities = tool_capabilities
        self.supported_reasoning_efforts: List[str] = supported_reasoning_efforts or []
        self.temperature_incompatible_efforts: Set[str] = temperature_incompatible_efforts or set()
        self.adaptive_thinking_only: bool = adaptive_thinking_only

    def supports_tools(self) -> bool:
        """Check if this model supports any tool calling capabilities."""
        return self.tool_capabilities != ToolCapability.NO_TOOLS

    def supports_parallel_tools(self) -> bool:
        """Check if this model supports parallel tool calling."""
        return (self.tool_capabilities & ToolCapability.PARALLEL_TOOLS) == ToolCapability.PARALLEL_TOOLS

    def supports_reasoning_effort(self) -> bool:
        """Check if this model supports variable reasoning effort levels."""
        return len(self.supported_reasoning_efforts) > 0

    def default_reasoning_effort(self) -> str | None:
        """
        Return the default (lowest) reasoning effort for this model.

        Returns the first entry in supported_reasoning_efforts, which by
        convention is the lowest/off level.  Returns None if the model does
        not support variable effort.
        """
        if not self.supported_reasoning_efforts:
            return None

        return self.supported_reasoning_efforts[0]

    def supports_temperature_for_effort(self, effort: str | None) -> bool:
        """
        Check whether temperature is supported for a given reasoning effort level.

        For most models this simply returns supports_temperature.  Models such as
        Anthropic thinking variants disable temperature when reasoning is active.

        Args:
            effort: The selected reasoning effort level, or None.

        Returns:
            True if temperature is supported at this effort level.
        """
        if effort and effort in self.temperature_incompatible_efforts:
            return False

        return self.supports_temperature
