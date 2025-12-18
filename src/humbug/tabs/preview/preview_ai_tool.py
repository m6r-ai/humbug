import json
import logging
from typing import Any, Dict, cast

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
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get high-level metadata about preview content including path, content type, and block count"
            ),
            "search": AIToolOperationDefinition(
                name="search",
                handler=self._search,
                extract_context=None,
                allowed_parameters={"tab_id", "search_text", "case_sensitive", "max_results"},
                required_parameters={"tab_id", "search_text"},
                description="Search for text in preview content. "
                    "Returns matches with surrounding context. Supports case-sensitive search and limiting results"
            ),
            "scroll_to": AIToolOperationDefinition(
                name="scroll_to",
                handler=self._scroll_to,
                extract_context=None,
                allowed_parameters={"tab_id", "block_index", "section_index", "text_position", "position"},
                required_parameters={"tab_id", "block_index"},
                description="Scroll preview to a specific content position. "
                    "Must provide block_index (0-based). Optional section_index and text_position "
                    "for finer control within a section. Optional position parameter controls where in "
                    "viewport: 'top', 'center' (default), or 'bottom'"
            ),
        }

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
        _requester_ref: Any,
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
                content=json.dumps(info, indent=2),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get preview info: {str(e)}") from e

    async def _search(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Search for text in preview content."""
        arguments = tool_call.arguments
        preview_tab = self._get_preview_tab(arguments)
        tab_id = preview_tab.tab_id()

        search_text = self._get_required_str_value("search_text", arguments)

        case_sensitive = self._get_optional_bool_value("case_sensitive", arguments, False)
        max_results = cast(int, self._get_optional_int_value("max_results", arguments, 50))

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
                content=json.dumps(result, indent=2),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to search preview: {str(e)}") from e

    async def _scroll_to(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
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

        section_index = cast(int, self._get_optional_int_value("section_index", arguments, 0))
        text_position = cast(int, self._get_optional_int_value("text_position", arguments, 0))
        position = self._get_optional_str_value("position", arguments, "center")

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
