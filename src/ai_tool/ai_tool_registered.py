"""Internal representation of registered AI tool."""

from dataclasses import dataclass

from ai_tool.ai_tool import AITool


@dataclass
class AIToolRegistered:
    """Internal representation of a registered tool."""
    tool: AITool
    display_name: str
    enabled_by_default: bool
