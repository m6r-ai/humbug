"""AI tool definition."""

from dataclasses import dataclass
from typing import List

from ai_tool.ai_tool_parameter import AIToolParameter


@dataclass
class AIToolDefinition:
    """Definition of an available tool."""
    name: str
    description: str
    parameters: List[AIToolParameter]
