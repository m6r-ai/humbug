"""Enhanced AI model with tool calling capabilities."""

from enum import Flag, auto


class ReasoningCapability(Flag):
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
        context_window: int,
        max_output_tokens: int,
        supports_temperature: bool,
        reasoning_capabilities: ReasoningCapability,
        tool_capabilities: ToolCapability = ToolCapability.NO_TOOLS
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
            tool_capabilities: Bitmap of tool calling capabilities supported by the model
        """
        self.name = name
        self.provider = provider
        self.context_window = context_window
        self.max_output_tokens = max_output_tokens
        self.supports_temperature = supports_temperature
        self.reasoning_capabilities = reasoning_capabilities
        self.tool_capabilities = tool_capabilities

    def supports_tools(self) -> bool:
        """Check if this model supports any tool calling capabilities."""
        return self.tool_capabilities != ToolCapability.NO_TOOLS

    def supports_parallel_tools(self) -> bool:
        """Check if this model supports parallel tool calling."""
        return (self.tool_capabilities & ToolCapability.PARALLEL_TOOLS) == ToolCapability.PARALLEL_TOOLS
