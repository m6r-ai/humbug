"""Singleton manager for AI tools."""

import logging
from typing import Dict, List

from ai_tool.ai_tool import AITool
from ai_tool.ai_tool_config import AIToolConfig
from ai_tool.ai_tool_definition import AIToolDefinition
from ai_tool.ai_tool_registered import AIToolRegistered


class AIToolManager:
    """Singleton manager for AI tools."""

    _instance: 'AIToolManager | None' = None

    def __new__(cls) -> 'AIToolManager':
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        if not hasattr(self, '_initialized'):
            self._registered_tools: Dict[str, AIToolRegistered] = {}
            self._enabled_tools: Dict[str, bool] = {}
            self._logger = logging.getLogger("AIToolManager")
            self._initialized = True

    def get_all_tool_configs(self) -> List[AIToolConfig]:
        """
        Get all available tool configurations from registered tools.

        Returns:
            List of all tool configurations
        """
        configs = []
        for tool_name, registered_tool in self._registered_tools.items():
            definition = registered_tool.tool.get_definition()
            configs.append(AIToolConfig(
                name=tool_name,
                display_name=registered_tool.display_name,
                description=definition.description,
                enabled_by_default=registered_tool.enabled_by_default
            ))
        return configs

    def register_tool(self, tool: AITool, display_name: str, enabled_by_default: bool = True) -> None:
        """
        Register a tool for use with AI models.

        Args:
            tool: The tool to register
            display_name: Human-readable name for the tool (used in UI)
            enabled_by_default: Whether the tool should be enabled by default

        Raises:
            ValueError: If a tool with the same name is already registered
        """
        definition = tool.get_definition()

        if definition.name in self._registered_tools:
            raise ValueError(f"Tool '{definition.name}' is already registered")

        self._registered_tools[definition.name] = AIToolRegistered(
            tool=tool,
            display_name=display_name,
            enabled_by_default=enabled_by_default
        )

        # Set default enabled state if not already configured
        if definition.name not in self._enabled_tools:
            self._enabled_tools[definition.name] = enabled_by_default

        self._logger.info("Registered tool: %s (display: %s)", definition.name, display_name)

    def update_tool_display_name(self, tool: AITool, new_display_name: str) -> None:
        """
        Update the display name of a previously registered tool.

        Args:
            tool: The tool instance to update
            new_display_name: New human-readable name for the tool

        Raises:
            ValueError: If the tool is not registered
        """
        # Find the tool by instance
        tool_name = None
        for name, registered_tool in self._registered_tools.items():
            if registered_tool.tool is tool:
                tool_name = name
                break

        if tool_name is None:
            raise ValueError("Tool instance is not registered")

        # Update the display name
        registered_tool = self._registered_tools[tool_name]
        self._registered_tools[tool_name] = AIToolRegistered(
            tool=registered_tool.tool,
            display_name=new_display_name,
            enabled_by_default=registered_tool.enabled_by_default
        )

        self._logger.debug("Updated display name for tool '%s' to '%s'", tool_name, new_display_name)

    def unregister_tool(self, name: str) -> None:
        """
        Unregister a tool.

        Args:
            name: Name of the tool to unregister
        """
        if name in self._registered_tools:
            del self._registered_tools[name]
            # Also remove from enabled tools to clean up
            if name in self._enabled_tools:
                del self._enabled_tools[name]

            self._logger.info("Unregistered tool: %s", name)

    def set_tool_enabled(self, tool_name: str, enabled: bool) -> None:
        """
        Enable or disable a tool.

        Args:
            tool_name: Name of the tool to enable/disable
            enabled: Whether the tool should be enabled
        """
        self._enabled_tools[tool_name] = enabled
        self._logger.debug("Tool '%s' %s", tool_name, "enabled" if enabled else "disabled")

    def is_tool_enabled(self, tool_name: str) -> bool:
        """
        Check if a tool is enabled.

        Args:
            tool_name: Name of the tool to check

        Returns:
            True if the tool is enabled, False otherwise
        """
        return self._enabled_tools.get(tool_name, True)

    def set_tool_enabled_states(self, enabled_states: Dict[str, bool]) -> None:
        """
        Set enabled states for multiple tools.

        Args:
            enabled_states: Dictionary mapping tool names to their enabled state
        """
        for tool_name, enabled in enabled_states.items():
            self.set_tool_enabled(tool_name, enabled)

    def get_tool_enabled_states(self) -> Dict[str, bool]:
        """
        Get enabled states for all tools.

        Returns:
            Dictionary mapping tool names to their enabled state
        """
        return self._enabled_tools.copy()

    def get_default_enabled_tools(self) -> Dict[str, bool]:
        """
        Get default enabled state for all registered tools.

        Returns:
            Dictionary mapping tool names to their default enabled state
        """
        return {
            tool_name: registered_tool.enabled_by_default
            for tool_name, registered_tool in self._registered_tools.items()
        }

    def get_tool_definitions(self) -> List[AIToolDefinition]:
        """
        Get definitions for all registered and enabled tools.

        Returns:
            List of tool definitions for enabled tools only
        """
        return [
            registered_tool.tool.get_definition()
            for tool_name, registered_tool in self._registered_tools.items()
            if self.is_tool_enabled(tool_name)
        ]

    def get_tool(self, name: str) -> AITool | None:
        """
        Get a registered tool by its name.

        Args:
            name: Name of the tool to retrieve

        Returns:
            The registered tool instance, or None if not found
        """
        registered_tool = self._registered_tools.get(name)
        return registered_tool.tool if registered_tool else None

    def get_tool_names(self) -> List[str]:
        """Get names of all registered tools."""
        return list(self._registered_tools.keys())

    def get_enabled_tool_names(self) -> List[str]:
        """Get names of all enabled tools."""
        return [
            tool_name for tool_name in self._registered_tools
            if self.is_tool_enabled(tool_name)
        ]
