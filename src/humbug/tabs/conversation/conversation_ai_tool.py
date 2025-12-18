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
from humbug.tabs.conversation.conversation_tab import ConversationTab


class ConversationAITool(AITool):
    """
    AI tool for conversation operations.

    Provides operations for reading, searching, and navigating conversation messages.
    Requires a conversation tab to be open (use system tool to create conversations).
    """

    def __init__(self, column_manager: ColumnManager):
        """
        Initialize the conversation tool.

        Args:
            column_manager: Column manager for accessing conversation tabs
        """
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("ConversationAITool")

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
            "Operations for browsing and searching conversation history. Use this tool to read "
            "messages, search content, and navigate conversations. You must have a conversation tab open first "
            "(use the system tool to open or create conversations).\n\n"
            "Available operations:\n\n" + "\n".join(operation_list)
        )

        return AIToolDefinition(
            name="conversation",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Conversation operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="GUID of the conversation tab to operate on",
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
                    name="message_types",
                    type="array",
                    description="List of message types to filter by. "
                        "Valid types: user_message, ai_response, ai_connected, tool_call, tool_result, "
                        "system_message, ai_reasoning",
                    required=False
                ),
                AIToolParameter(
                    name="limit",
                    type="integer",
                    description="Maximum number of messages to return",
                    required=False
                ),
                AIToolParameter(
                    name="include_tool_details",
                    type="boolean",
                    description="Include full tool call/result details (for read_messages operation)",
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
                description="Get high-level metadata about a conversation including message count, "
                    "timestamps, models used, token counts, and parent conversation reference"
            ),
            "read_messages": AIToolOperationDefinition(
                name="read_messages",
                handler=self._read_messages,
                extract_context=None,
                allowed_parameters={"tab_id", "start_index", "end_index", "message_types", "limit", "include_tool_details"},
                required_parameters={"tab_id"},
                description="Read messages from a conversation with filtering and pagination. "
                    "Supports filtering by index range, message types (user_message, ai_response, tool_call, tool_result, etc.), "
                    "and limiting results. Can optionally exclude tool call details for efficiency"
            ),
            "get_message": AIToolOperationDefinition(
                name="get_message",
                handler=self._get_message,
                extract_context=None,
                allowed_parameters={"tab_id", "message_id", "message_index"},
                required_parameters={"tab_id"},
                description="Get a specific message by ID or index. "
                    "Must provide either message_id (UUID) or message_index (0-based)"
            ),
            "search": AIToolOperationDefinition(
                name="search",
                handler=self._search,
                extract_context=None,
                allowed_parameters={"tab_id", "search_text", "case_sensitive", "message_types", "max_results"},
                required_parameters={"tab_id", "search_text"},
                description="Search for text across all messages in a conversation. "
                    "Returns matches with surrounding context. Supports case-sensitive search, "
                    "filtering by message types, and limiting results"
            ),
            "scroll_to": AIToolOperationDefinition(
                name="scroll_to",
                handler=self._scroll_to,
                extract_context=None,
                allowed_parameters={"tab_id", "message_id", "message_index"},
                required_parameters={"tab_id"},
                description="Scroll the conversation view to a specific message (header at top of viewport). "
                    "Must provide either message_id (UUID) or message_index (0-based)"
            ),
        }

    def _get_conversation_tab(self, arguments: Dict[str, Any]) -> ConversationTab:
        """
        Get a conversation tab by ID.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            ConversationTab instance

        Raises:
            AIToolExecutionError: If no conversation tab found
        """
        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, ConversationTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not a conversation tab")

        return tab

    async def _get_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get conversation metadata."""
        arguments = tool_call.arguments
        conversation_tab = self._get_conversation_tab(arguments)
        tab_id = conversation_tab.tab_id()

        try:
            info = conversation_tab.get_conversation_info()

            # Add file path to info
            info['file_path'] = self._mindspace_manager.get_relative_path(conversation_tab.path())
            info['tab_id'] = tab_id

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested conversation info\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="conversation",
                content=str(info),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get conversation info: {str(e)}") from e

    async def _read_messages(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read messages from conversation."""
        arguments = tool_call.arguments
        conversation_tab = self._get_conversation_tab(arguments)
        tab_id = conversation_tab.tab_id()

        # Extract optional parameters
        start_index = self._get_optional_int_value("start_index", arguments)
        end_index = self._get_optional_int_value("end_index", arguments)
        message_types = self._get_optional_list_value("message_types", arguments)
        limit = self._get_optional_int_value("limit", arguments)
        include_tool_details = self._get_optional_bool_value("include_tool_details", arguments, True)

        try:
            result = conversation_tab.read_messages(
                start_index, end_index, message_types, limit, include_tool_details
            )

            # Build log message
            log_parts = ["AI read conversation messages"]
            if start_index is not None or end_index is not None:
                log_parts.append(f"range: {start_index or 0}-{end_index or 'end'}")

            if message_types:
                log_parts.append(f"types: {', '.join(message_types)}")

            if limit:
                log_parts.append(f"limit: {limit}")

            log_parts.append(f"tab ID: {tab_id}")

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                "\n".join(log_parts)
            )

            return AIToolResult(
                id=tool_call.id,
                name="conversation",
                content=str(result),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to read messages: {str(e)}") from e

    async def _get_message(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get a specific message by ID or index."""
        arguments = tool_call.arguments
        conversation_tab = self._get_conversation_tab(arguments)
        tab_id = conversation_tab.tab_id()

        message_id = arguments.get("message_id")
        message_index = arguments.get("message_index")

        if message_id is None and message_index is None:
            raise AIToolExecutionError("Must provide either 'message_id' or 'message_index'")

        if message_id is not None and not isinstance(message_id, str):
            raise AIToolExecutionError("'message_id' must be a string")

        if message_index is not None and not isinstance(message_index, int):
            raise AIToolExecutionError("'message_index' must be an integer")

        try:
            message = conversation_tab.get_message_by_id_or_index(message_id, message_index)

            if message is None:
                identifier = f"ID {message_id}" if message_id else f"index {message_index}"
                raise AIToolExecutionError(f"Message not found: {identifier}")

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested message {message_id or message_index}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="conversation",
                content=str(message),
                context="json"
            )

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get message: {str(e)}") from e

    async def _search(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Search for text in conversation."""
        arguments = tool_call.arguments
        conversation_tab = self._get_conversation_tab(arguments)
        tab_id = conversation_tab.tab_id()

        search_text = self._get_required_str_value("search_text", arguments)

        case_sensitive = self._get_optional_bool_value("case_sensitive", arguments, False)
        message_types = self._get_optional_list_value("message_types", arguments)
        max_results = cast(int, self._get_optional_int_value("max_results", arguments, 50))

        try:
            result = conversation_tab.search_messages(
                search_text, case_sensitive, message_types, max_results
            )

            case_desc = " (case-sensitive)" if case_sensitive else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI searched conversation for '{search_text}'{case_desc}: "
                f"{result['total_matches']} matches\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="conversation",
                content=str(result),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to search conversation: {str(e)}") from e

    async def _scroll_to(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Scroll conversation to a specific message."""
        arguments = tool_call.arguments
        conversation_tab = self._get_conversation_tab(arguments)
        tab_id = conversation_tab.tab_id()

        message_id = arguments.get("message_id")
        message_index = arguments.get("message_index")

        if message_id is None and message_index is None:
            raise AIToolExecutionError("Must provide either 'message_id' or 'message_index'")

        if message_id is not None and not isinstance(message_id, str):
            raise AIToolExecutionError("'message_id' must be a string")

        if message_index is not None and not isinstance(message_index, int):
            raise AIToolExecutionError("'message_index' must be an integer")

        try:
            result = conversation_tab.scroll_to_message(message_id, message_index)

            if not result["success"]:
                error_msg = result.get("error", "Unknown error")
                raise AIToolExecutionError(error_msg)

            # Build identifier for logging and response
            actual_identifier = f"index {result['actual_index']}"

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI scrolled conversation to message {actual_identifier}\\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="conversation",
                content=f"Scrolled to message {actual_identifier}"
            )

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Failed to scroll to message: {str(e)}") from e
