"""Help tool for accessing AI tool documentation."""

import json
import logging
from typing import Any, Dict

from ai_tool import (
    AITool, AIToolDefinition, AIToolParameter, AIToolOperationDefinition,
    AIToolCall, AIToolResult, AIToolAuthorizationCallback, AIToolExecutionError,
    AIToolManager
)


class HelpAITool(AITool):
    """
    Tool for accessing detailed documentation about other AI tools.

    This tool provides lazy-loading of tool documentation, allowing the system
    prompt to remain minimal while still providing comprehensive help when needed.
    """

    def __init__(self, tool_manager: AIToolManager):
        """
        Initialize the help tool.

        Args:
            tool_manager: AIToolManager instance to query for tool information
        """
        self._tool_manager = tool_manager
        self._logger = logging.getLogger("HelpAITool")

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return self._build_definition_from_operations(
            name="help",
            description_prefix=(
                "Get detailed documentation for AI tools and their operations. "
                "Use this when you need to understand tool parameters, constraints, or usage patterns."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="tool_name",
                    type="string",
                    description="Name of the tool to get help for",
                    required=False
                ),
                AIToolParameter(
                    name="operation_name",
                    type="string",
                    description="Name of the operation to get help for (requires tool_name)",
                    required=False
                )
            ]
        )

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """Get operation definitions for this tool."""
        return {
            "list_tools": AIToolOperationDefinition(
                name="list_tools",
                handler=self._list_tools,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="List all available tools with brief descriptions and operations"
            ),
            "get_help": AIToolOperationDefinition(
                name="get_help",
                handler=self._get_help,
                extract_context=None,
                allowed_parameters={"tool_name", "operation_name"},
                required_parameters={"tool_name"},
                description="Get detailed documentation for a tool or specific operation"
            )
        }

    async def _list_tools(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        List all available tools with brief descriptions.

        Returns a JSON object containing all enabled tools with their
        brief descriptions and available operations.
        """
        tools = []

        for tool_name in sorted(self._tool_manager.get_enabled_tool_names()):
            # Skip self to avoid recursion
            if tool_name == "help":
                continue

            tool = self._tool_manager.get_tool(tool_name)
            if tool:
                brief = tool.get_brief_description()
                operation_summary = tool.get_operation_summary()

                tools.append({
                    "name": tool_name,
                    "description": brief,
                    "operations": list(operation_summary.keys())
                })

        result = {
            "available_tools": len(tools),
            "tools": tools,
            "usage": "Use 'describe' operation for detailed documentation on any tool"
        }

        self._logger.debug("Listed %d tools", len(tools))

        return AIToolResult(
            id=tool_call.id,
            name="help",
            content=json.dumps(result, indent=2),
            context="json"
        )

    async def _get_help(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Get detailed documentation for a tool or specific operation.

        Returns detailed markdown documentation including:
        - Tool overview (if operation_name not specified)
        - All operations with descriptions (if operation_name not specified)
        - Parameters for operations with types, constraints, and valid values
        - Focused operation documentation (if operation_name specified)
        """
        arguments = tool_call.arguments
        tool_name = self._get_required_str_value("tool_name", arguments)
        operation_name = self._get_optional_str_value("operation_name", arguments)

        # Validate tool exists
        tool = self._tool_manager.get_tool(tool_name)
        if not tool:
            available_tools = ", ".join(sorted(self._tool_manager.get_enabled_tool_names()))
            raise AIToolExecutionError(
                f"Unknown tool: '{tool_name}'. Available tools: {available_tools}"
            )

        # Get detailed help from the tool
        help_text = tool.get_detailed_help(operation_name)

        if operation_name:
            self._logger.debug(
                "Provided detailed help for operation: %s.%s", tool_name, operation_name
            )

        else:
            self._logger.debug("Provided detailed help for tool: %s", tool_name)

        return AIToolResult(
            id=tool_call.id,
            name="help",
            content=help_text,
            context="markdown"
        )
