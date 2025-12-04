import logging
from typing import Any, Dict

from ai_tool import (
    AITool,
    AIToolAuthorizationCallback,
    AIToolCall,
    AIToolDefinition,
    AIToolExecutionError,
    AIToolOperationDefinition,
    AIToolParameter,
    AIToolResult,
)
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.preview.preview_tab import PreviewTab


class PreviewAITool(AITool):
    """
    AI tool for preview operations.

    Provides operations for searching and navigating preview content.
    Requires a preview tab to be open (use system tool to open previews).
    """

    def __init__(self, column_manager: ColumnManager):
        """
        Initialize the preview tool.

        Args:
            column_manager: Column manager for accessing preview tabs
        """
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("PreviewAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        operations = self.get_operation_definitions()
        operation_names = list(operations.keys())

        # Build description from operations
        operation_list = []
        for name, op_def in operations.items():
            operation_list.append(f"- {name}: {op_def.description}")

        description = (
            "Operations for browsing and searching preview content. Use this tool to search "
            "and navigate file/directory previews. You must have a preview tab open first "
            "(use the system tool to open previews).\n\n"
            "Available operations:\n\n" + "\n".join(operation_list)
        )

        return AIToolDefinition(
            name="preview",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Preview operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="GUID of the preview tab to operate on",
                    required=True
                ),
                AIToolParameter(
                    name="search_text",
                    type="string",
                    description="Text to search for (for search operation)",
                    required=False
                ),
                AIToolParameter(
                    name="case_sensitive",
                    type="boolean",
                    description="Whether search should be case-sensitive",
                    required=False
                ),
                AIToolParameter(
                    name="max_results",
                    type="integer",
                    description="Maximum number of search results to return",
                    required=False
                ),
                AIToolParameter(
                    name="block_index",
                    type="integer",
                    description="Content block index (0-based) for scroll_to operation",
                    required=False
                ),
                AIToolParameter(
                    name="section_index",
                    type="integer",
                    description="Section index within content block (0-based) for scroll_to operation",
                    required=False
                ),
                AIToolParameter(
                    name="text_position",
                    type="integer",
                    description="Text position within section (0-based) for scroll_to operation",
                    required=False
                ),
                AIToolParameter(
                    name="position",
                    type="string",
                    description="Viewport position for scrolling: 'top', 'center', or 'bottom'",
                    required=False
                ),
            ]
        )

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "get_info": AIToolOperationDefinition(
                name="get_info",
                handler=self._get_info,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get high-level metadata about preview content including path, content type, and block count"
            ),
            "search": AIToolOperationDefinition(
                name="search",
                handler=self._search,
                allowed_parameters={"tab_id", "search_text", "case_sensitive", "max_results"},
                required_parameters={"tab_id", "search_text"},
                description="Search for text in preview content. "
                    "Returns matches with surrounding context. Supports case-sensitive search and limiting results"
            ),
            "scroll_to": AIToolOperationDefinition(
                name="scroll_to",
                handler=self._scroll_to,
                allowed_parameters={"tab_id", "block_index", "section_index", "text_position", "position"},
                required_parameters={"tab_id", "block_index"},
                description="Scroll preview to a specific content position. "
                    "Must provide block_index (0-based). Optional section_index and text_position "
                    "for finer control within a section. Optional position parameter controls where in "
                    "viewport: 'top', 'center' (default), or 'bottom'"
            ),
        }

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute a preview operation.

        Args:
            tool_call: Tool call containing operation and arguments
            requester_ref: Reference to the requester
            request_authorization: Function to request user authorization

        Returns:
            Result of the operation

        Raises:
            AIToolExecutionError: If operation fails
        """
        arguments = tool_call.arguments
        operation = arguments.get("operation")

        if not operation:
            raise AIToolExecutionError("No 'operation' argument provided")

        if not isinstance(operation, str):
            raise AIToolExecutionError("'operation' must be a string")

        # Get operation definition
        operation_definitions = self.get_operation_definitions()
        if operation not in operation_definitions:
            available_operations = ", ".join(sorted(operation_definitions.keys()))
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}. Available operations: {available_operations}"
            )

        operation_def = operation_definitions[operation]

        self._logger.debug("Preview operation requested: %s", operation)

        try:
            return await operation_def.handler(tool_call, request_authorization)

        except AIToolExecutionError:
            raise

        except Exception as e:
            self._logger.error("Unexpected error in preview operation '%s': %s", operation, str(e), exc_info=True)
            raise AIToolExecutionError(f"Preview operation failed: {str(e)}") from e

    def _get_preview_tab(self, arguments: Dict[str, Any]) -> PreviewTab:
        """
        Get a preview tab by ID.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            PreviewTab instance

        Raises:
            AIToolExecutionError: If no preview tab found
        """
        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, PreviewTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not a preview tab")

        return tab

    async def _get_info(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get preview content metadata."""
        arguments = tool_call.arguments
        preview_tab = self._get_preview_tab(arguments)
        tab_id = preview_tab.tab_id()

        try:
            info = preview_tab.get_preview_info()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested preview info\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="preview",
                content=str(info)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get preview info: {str(e)}") from e

    async def _search(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Search for text in preview content."""
        arguments = tool_call.arguments
        preview_tab = self._get_preview_tab(arguments)
        tab_id = preview_tab.tab_id()

        if "search_text" not in arguments:
            raise AIToolExecutionError("No 'search_text' argument provided")

        search_text = arguments["search_text"]
        if not isinstance(search_text, str):
            raise AIToolExecutionError("'search_text' must be a string")

        case_sensitive = arguments.get("case_sensitive", False)
        if not isinstance(case_sensitive, bool):
            raise AIToolExecutionError("'case_sensitive' must be a boolean")

        max_results = arguments.get("max_results", 50)
        if not isinstance(max_results, int):
            raise AIToolExecutionError("'max_results' must be an integer")

        try:
            result = preview_tab.search_content(
                search_text, case_sensitive, max_results
            )

            case_desc = " (case-sensitive)" if case_sensitive else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI searched preview for '{search_text}'{case_desc}: "
                f"{result['total_matches']} matches\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="preview",
                content=str(result)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to search preview: {str(e)}") from e

    async def _scroll_to(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Scroll preview to a specific content position."""
        arguments = tool_call.arguments
        preview_tab = self._get_preview_tab(arguments)
        tab_id = preview_tab.tab_id()

        if "block_index" not in arguments:
            raise AIToolExecutionError("No 'block_index' argument provided")

        block_index = arguments["block_index"]
        if not isinstance(block_index, int):
            raise AIToolExecutionError("'block_index' must be an integer")

        section_index = arguments.get("section_index", 0)
        if not isinstance(section_index, int):
            raise AIToolExecutionError("'section_index' must be an integer")

        text_position = arguments.get("text_position", 0)
        if not isinstance(text_position, int):
            raise AIToolExecutionError("'text_position' must be an integer")

        position = arguments.get("position", "center")
        if not isinstance(position, str):
            raise AIToolExecutionError("'position' must be a string")

        if position not in ("top", "center", "bottom"):
            raise AIToolExecutionError("'position' must be 'top', 'center', or 'bottom'")

        try:
            success = preview_tab.scroll_to_content_position(
                block_index, section_index, text_position, position
            )

            if not success:
                raise AIToolExecutionError(
                    f"Could not scroll to position: block={block_index}, "
                    f"section={section_index}, pos={text_position}"
                )

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI scrolled preview to block {block_index}, section {section_index}, "
                f"position {text_position} ({position})\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="preview",
                content=f"Scrolled to block {block_index}, section {section_index}, "
                f"position {text_position} ({position})"
            )

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Failed to scroll preview: {str(e)}") from e
