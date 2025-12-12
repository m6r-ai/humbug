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
from humbug.tabs.log.log_tab import LogTab


class LogAITool(AITool):
    """
    AI tool for log operations.

    Provides operations for reading, searching, and navigating log messages.
    Requires a log tab to be open (use system tool to show the log).
    """

    def __init__(self, column_manager: ColumnManager):
        """
        Initialize the log tool.

        Args:
            column_manager: Column manager for accessing log tabs
        """
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("LogAITool")

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
            "Operations for browsing and searching mindspace log messages. Use this tool to read "
            "log entries, search content, and navigate the log. You must have a log tab open first "
            "(use the system tool to show the log).\n\n"
            "Available operations:\n\n" + "\n".join(operation_list)
        )

        return AIToolDefinition(
            name="log",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Log operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="GUID of the log tab to operate on",
                    required=True
                ),
                AIToolParameter(
                    name="start_index",
                    type="integer",
                    description="Start message index (0-based) for read_messages operation",
                    required=False
                ),
                AIToolParameter(
                    name="end_index",
                    type="integer",
                    description="End message index (0-based) for read_messages operation",
                    required=False
                ),
                AIToolParameter(
                    name="levels",
                    type="array",
                    description="List of log levels to filter by. Valid levels: trace, info, warn, error",
                    required=False
                ),
                AIToolParameter(
                    name="limit",
                    type="integer",
                    description="Maximum number of messages to return",
                    required=False
                ),
                AIToolParameter(
                    name="include_content",
                    type="boolean",
                    description="Include full message content (for read_messages operation)",
                    required=False
                ),
                AIToolParameter(
                    name="message_id",
                    type="string",
                    description="Message UUID (for get_message and scroll_to operations)",
                    required=False
                ),
                AIToolParameter(
                    name="message_index",
                    type="integer",
                    description="Message index (0-based) (for get_message and scroll_to operations)",
                    required=False
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
                description="Get high-level metadata about the log including message count, "
                    "timestamps, and level distribution"
            ),
            "read_messages": AIToolOperationDefinition(
                name="read_messages",
                handler=self._read_messages,
                extract_context=None,
                allowed_parameters={"tab_id", "start_index", "end_index", "levels", "limit", "include_content"},
                required_parameters={"tab_id"},
                description="Read log messages with filtering and pagination. "
                    "Supports filtering by index range, log levels (trace, info, warn, error), "
                    "and limiting results. Can optionally exclude full content for efficiency"
            ),
            "get_message": AIToolOperationDefinition(
                name="get_message",
                handler=self._get_message,
                extract_context=None,
                allowed_parameters={"tab_id", "message_id", "message_index"},
                required_parameters={"tab_id"},
                description="Get a specific log message by ID or index. "
                    "Must provide either message_id (UUID) or message_index (0-based)"
            ),
            "search": AIToolOperationDefinition(
                name="search",
                handler=self._search,
                extract_context=None,
                allowed_parameters={"tab_id", "search_text", "case_sensitive", "levels", "max_results"},
                required_parameters={"tab_id", "search_text"},
                description="Search for text across all log messages. "
                    "Returns matches with surrounding context. Supports case-sensitive search, "
                    "filtering by log levels, and limiting results"
            ),
            "scroll_to": AIToolOperationDefinition(
                name="scroll_to",
                handler=self._scroll_to,
                extract_context=None,
                allowed_parameters={"tab_id", "message_id", "message_index"},
                required_parameters={"tab_id"},
                description="Scroll the log view to a specific message (at top of viewport). "
                    "Must provide either message_id (UUID) or message_index (0-based)"
            ),
        }

    def _get_log_tab(self, arguments: Dict[str, Any]) -> LogTab:
        """
        Get a log tab by ID.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            LogTab instance

        Raises:
            AIToolExecutionError: If no log tab found
        """
        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, LogTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not a log tab")

        return tab

    async def _get_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get log metadata."""
        arguments = tool_call.arguments
        log_tab = self._get_log_tab(arguments)
        tab_id = log_tab.tab_id()

        try:
            info = log_tab.get_log_info()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested log info\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="log",
                content=str(info)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get log info: {str(e)}") from e

    async def _read_messages(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read log messages."""
        arguments = tool_call.arguments
        log_tab = self._get_log_tab(arguments)
        tab_id = log_tab.tab_id()

        # Extract optional parameters
        start_index = self._get_optional_int_value("start_index", arguments)
        end_index = self._get_optional_int_value("end_index", arguments)
        levels = self._get_optional_list_value("levels", arguments)
        limit = self._get_optional_int_value("limit", arguments)
        include_content = self._get_optional_bool_value("include_content", arguments, True)

        try:
            result = log_tab.read_messages(
                start_index, end_index, levels, limit, include_content
            )

            # Build log message
            log_parts = ["AI read log messages"]
            if start_index is not None or end_index is not None:
                log_parts.append(f"range: {start_index or 0}-{end_index or 'end'}")

            if levels:
                log_parts.append(f"levels: {', '.join(levels)}")

            if limit:
                log_parts.append(f"limit: {limit}")

            log_parts.append(f"tab ID: {tab_id}")

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                "\n".join(log_parts)
            )

            return AIToolResult(
                id=tool_call.id,
                name="log",
                content=str(result)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to read log messages: {str(e)}") from e

    async def _get_message(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get a specific log message by ID or index."""
        arguments = tool_call.arguments
        log_tab = self._get_log_tab(arguments)
        tab_id = log_tab.tab_id()

        message_id = arguments.get("message_id")
        message_index = arguments.get("message_index")

        if message_id is None and message_index is None:
            raise AIToolExecutionError("Must provide either 'message_id' or 'message_index'")

        if message_id is not None and not isinstance(message_id, str):
            raise AIToolExecutionError("'message_id' must be a string")

        if message_index is not None and not isinstance(message_index, int):
            raise AIToolExecutionError("'message_index' must be an integer")

        try:
            message = log_tab.get_message_by_id_or_index(message_id, message_index)

            if message is None:
                identifier = f"ID {message_id}" if message_id else f"index {message_index}"
                raise AIToolExecutionError(f"Log message not found: {identifier}")

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested log message {message_id or message_index}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="log",
                content=str(message)
            )

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get log message: {str(e)}") from e

    async def _search(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Search for text in log messages."""
        arguments = tool_call.arguments
        log_tab = self._get_log_tab(arguments)
        tab_id = log_tab.tab_id()

        search_text = self._get_required_str_value("search_text", arguments)

        case_sensitive = self._get_optional_bool_value("case_sensitive", arguments, False)
        levels = self._get_optional_list_value("levels", arguments)
        max_results = cast(int, self._get_optional_int_value("max_results", arguments, 50))

        try:
            result = log_tab.search_messages(
                search_text, case_sensitive, levels, max_results
            )

            case_desc = " (case-sensitive)" if case_sensitive else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI searched log for '{search_text}'{case_desc}: "
                f"{result['total_matches']} matches\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="log",
                content=str(result)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to search log: {str(e)}") from e

    async def _scroll_to(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Scroll log to a specific message."""
        arguments = tool_call.arguments
        log_tab = self._get_log_tab(arguments)
        tab_id = log_tab.tab_id()

        message_id = arguments.get("message_id")
        message_index = arguments.get("message_index")

        if message_id is None and message_index is None:
            raise AIToolExecutionError("Must provide either 'message_id' or 'message_index'")

        if message_id is not None and not isinstance(message_id, str):
            raise AIToolExecutionError("'message_id' must be a string")

        if message_index is not None and not isinstance(message_index, int):
            raise AIToolExecutionError("'message_index' must be an integer")

        try:
            # This is slightly odd but we have to log the action before executing it as the log message
            # can change where we're scrolling to!
            identifier = message_id if message_id else f"index {message_index}"
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI scrolled log to message {identifier}\\ntab ID: {tab_id}"
            )

            success = log_tab.scroll_to_message(message_id, message_index)

            if not success:
                identifier = f"ID {message_id}" if message_id else f"index {message_index}"
                raise AIToolExecutionError(f"Log message not found: {identifier}")

            return AIToolResult(
                id=tool_call.id,
                name="log",
                content=f"Scrolled to log message {identifier}"
            )

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Failed to scroll to log message: {str(e)}") from e
