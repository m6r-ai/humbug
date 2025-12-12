import logging
from typing import Any, Dict, cast

from ai_tool import (
    AITool,
    AIToolAuthorizationCallback,
    AIToolAuthorizationDenied,
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
from humbug.tabs.editor.editor_tab import EditorTab


class EditorAITool(AITool):
    """
    AI tool for editor operations.

    Provides operations for reading, searching, navigating, and modifying
    editor content. Requires an editor tab to be open (use system tool to create tabs).
    """

    def __init__(self, column_manager: ColumnManager):
        """
        Initialize the editor tool.

        Args:
            column_manager: Column manager for accessing editor tabs
        """
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("EditorAITool")

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
            "Operations for working with editor tabs. Use this tool to read, search, navigate, "
            "and modify text content in open editor tabs. You must have an editor tab open first "
            "(use the system tool to open files).\n\n"
            "Available operations:\n\n" + "\n".join(operation_list)
        )

        return AIToolDefinition(
            name="editor",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Editor operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="GUID of the editor tab to operate on",
                    required=True
                ),
                AIToolParameter(
                    name="start_line",
                    type="integer",
                    description="Start line number (1-indexed) for read_lines operation",
                    required=False
                ),
                AIToolParameter(
                    name="end_line",
                    type="integer",
                    description="End line number (1-indexed) for read_lines operation",
                    required=False
                ),
                AIToolParameter(
                    name="line",
                    type="integer",
                    description="Line number (1-indexed) for goto_line operation",
                    required=False
                ),
                AIToolParameter(
                    name="column",
                    type="integer",
                    description="Column number (1-indexed) for goto_line operation",
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
                    description="Whether search should be case-sensitive (for search operation)",
                    required=False
                ),
                AIToolParameter(
                    name="diff_content",
                    type="string",
                    description="Unified diff content to apply (for apply_diff operation)",
                    required=False
                ),
                AIToolParameter(
                    name="context_lines",
                    type="integer",
                    description="Number of context lines for get_diff operation (default 3)",
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
            "read_lines": AIToolOperationDefinition(
                name="read_lines",
                handler=self._read_lines,
                extract_context=None,
                allowed_parameters={"tab_id", "start_line", "end_line"},
                required_parameters={"tab_id"},
                description="Read content lines from an editor tab. Optionally specify line range with "
                    "start_line and end_line (1-indexed, inclusive). Returns line numbers and content as a "
                    "dictionary-like structure"
            ),
            "get_cursor_info": AIToolOperationDefinition(
                name="get_cursor_info",
                handler=self._get_cursor_info,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get cursor position and selection information from an editor tab"
            ),
            "get_info": AIToolOperationDefinition(
                name="get_info",
                handler=self._get_info,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get editor metadata including line count, language, encoding, and modification status"
            ),
            "goto_line": AIToolOperationDefinition(
                name="goto_line",
                handler=self._goto_line,
                extract_context=None,
                allowed_parameters={"tab_id", "line", "column"},
                required_parameters={"tab_id", "line"},
                description="Move cursor to specific line and optional column in an editor tab (1-indexed)"
            ),
            "search": AIToolOperationDefinition(
                name="search",
                handler=self._search,
                extract_context=None,
                allowed_parameters={"tab_id", "search_text", "case_sensitive"},
                required_parameters={"tab_id", "search_text"},
                description="Find precise line numbers for all occurrences of text in an editor tab. "
                    "Returns list of matches with line (1-indexed), column (1-indexed), and context"
            ),
            "get_selected_text": AIToolOperationDefinition(
                name="get_selected_text",
                handler=self._get_selected_text,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get the currently selected text from an editor tab"
            ),
            "get_diff": AIToolOperationDefinition(
                name="get_diff",
                handler=self._get_diff,
                extract_context=None,
                allowed_parameters={"tab_id", "context_lines"},
                required_parameters={"tab_id"},
                description="Get unified diff between saved file content and current editor buffer. "
                    "Shows what changes would be saved. Returns empty if no modifications exist. "
                    "Useful for previewing changes before save_file operation"
            ),
            "apply_diff": AIToolOperationDefinition(
                name="apply_diff",
                handler=self._apply_diff,
                extract_context=None,
                allowed_parameters={"tab_id", "diff_content"},
                required_parameters={"tab_id", "diff_content"},
                description="Apply a unified diff to the editor content. This operation is atomic - either all "
                    "hunks apply successfully or none do. The diff is applied with fuzzy matching to handle minor "
                    "line movements. The diff should be in standard unified diff format, though the file headers "
                    "(--- and +++) are optional. Where possible the diff should have at least 3 lines of context "
                    "before and after each hunk. Diff line numbers are best computed using the read_lines or search "
                    "operations. Editor contents are not saved automatically - you must call save_file to persist changes"
            ),
            "save_file": AIToolOperationDefinition(
                name="save_file",
                handler=self._save_file,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Save the current editor content to file. Requires user authorization"
            ),
        }

    def extract_context(self, tool_call: AIToolCall) -> str | None:
        """
        Extract context information from the tool call.

        Args:
            tool_call: Tool call containing arguments and metadata

        Returns:
            Context string if available, otherwise None
        """
        arguments = tool_call.arguments
        operation = arguments.get("operation")
        if operation != "apply_diff":
            return None

        if "diff_content" not in arguments:
            return None

        diff_content = arguments["diff_content"]
        return f"Diff content:\n```diff\n{diff_content}\n```"

    def _get_editor_tab(self, arguments: Dict[str, Any]) -> EditorTab:
        """
        Get an editor tab by ID.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            EditorTab instance

        Raises:
            AIToolExecutionError: If no editor tab found
        """
        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, EditorTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not an editor tab")

        return tab

    async def _read_lines(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read content from an editor tab."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        start_line = self._get_optional_int_value("start_line", arguments)
        end_line = self._get_optional_int_value("end_line", arguments)

        try:
            content = editor_tab.get_text_range(start_line, end_line)
            context_object = {}

            # Handle empty content - should still show line 1 as empty string
            if not content:
                context_object[1] = ""

            else:
                content_lines = content.splitlines()
                for line_num, line_text in enumerate(content_lines):
                    context_object[line_num + start_line if start_line is not None else line_num + 1] = line_text

            if start_line is not None or end_line is not None:
                range_desc = f"lines {start_line or 1}-{end_line or 'end'}"
                log_msg = f"AI read editor content ({range_desc})\ntab ID: {tab_id}"

            else:
                log_msg = f"AI read editor content\ntab ID: {tab_id}"

            self._mindspace_manager.add_interaction(MindspaceLogLevel.INFO, log_msg)

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=str(context_object)
            )

        except ValueError as e:
            raise AIToolExecutionError(f"Invalid line range: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to read editor content: {str(e)}") from e

    async def _get_cursor_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get cursor position and selection information."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        try:
            cursor_info = editor_tab.get_cursor_info()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested cursor info\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=str(cursor_info)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get cursor info: {str(e)}") from e

    async def _get_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get editor metadata and document information."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        try:
            editor_info = editor_tab.get_editor_info()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested editor info\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=str(editor_info)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get editor info: {str(e)}") from e

    async def _goto_line(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Move cursor to specific line and column."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        if "line" not in arguments:
            raise AIToolExecutionError("No 'line' argument provided")

        line = arguments["line"]
        if not isinstance(line, int):
            raise AIToolExecutionError("'line' must be an integer")

        column = cast(int, self._get_optional_int_value("column", arguments, 1))

        try:
            editor_tab.goto_line(line, column)

            col_desc = f", column {column}" if column != 1 else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI moved cursor to line {line}{col_desc}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=f"Moved cursor to line {line}{col_desc}"
            )

        except ValueError as e:
            raise AIToolExecutionError(f"Invalid position: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to goto line: {str(e)}") from e

    async def _search(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Find all occurrences of text."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        search_text = self._get_required_str_value("search_text", arguments)

        case_sensitive = self._get_optional_bool_value("case_sensitive", arguments, False)

        try:
            matches = editor_tab.find_all_occurrences(search_text, case_sensitive)

            case_desc = " (case-sensitive)" if case_sensitive else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI searched for '{search_text}'{case_desc}: {len(matches)} matches\ntab ID: {tab_id}"
            )

            if not matches:
                return AIToolResult(
                    id=tool_call.id,
                    name="editor",
                    content=f"No matches found for '{search_text}'{case_desc}"
                )

            result = {
                "search_text": search_text,
                "case_sensitive": case_sensitive,
                "match_count": len(matches),
                "matches": matches
            }

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=str(result)
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to search in editor: {str(e)}") from e

    async def _get_selected_text(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get the currently selected text."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        try:
            selected_text = editor_tab.get_selected_text()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested selected text\ntab ID: {tab_id}"
            )

            if not selected_text:
                return AIToolResult(
                    id=tool_call.id,
                    name="editor",
                    content="No text selected"
                )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=selected_text
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get selected text: {str(e)}") from e

    async def _get_diff(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get unified diff between saved file and current buffer."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        context_lines = cast(int, self._get_optional_int_value("context_lines", arguments, 3))
        if context_lines < 0:
            raise AIToolExecutionError("'context_lines' must be non-negative")

        try:
            diff = editor_tab.get_diff(context_lines)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested diff\ntab ID: {tab_id}"
            )

            if not diff:
                # Get editor info to provide helpful message
                editor_info = editor_tab.get_editor_info()
                if not editor_info.get('file_path'):
                    message = "No diff available: file has never been saved (untitled)"

                else:
                    message = "No changes: buffer matches saved file content"

                return AIToolResult(
                    id=tool_call.id,
                    name="editor",
                    content=message
                )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=diff
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get diff: {str(e)}") from e

    async def _save_file(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Save the current editor content to file."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        # Get editor info to show file path
        editor_info = editor_tab.get_editor_info()
        file_path = editor_info.get('file_path', '')

        if not file_path:
            raise AIToolExecutionError("Cannot save: editor has no file path (untitled file)")

        # Build authorization context
        context = f"Save editor content to file: {file_path} (tab {tab_id})"
        diff = editor_tab.get_diff(3)

        # Request authorization - writing to filesystem
        authorized = await request_authorization("editor", arguments, context, diff, True)
        if not authorized:
            raise AIToolAuthorizationDenied("User denied permission to save file")

        try:
            success = editor_tab.save()

            if not success:
                raise AIToolExecutionError("Save operation failed")

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI saved editor file: {file_path}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=f"Saved file: '{file_path}'"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to save file: {str(e)}") from e

    async def _apply_diff(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Apply a unified diff to editor content."""
        arguments = tool_call.arguments
        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        diff_content = self._get_required_str_value("diff_content", arguments)

        try:
            result = editor_tab.apply_diff(diff_content)

            if result['success']:
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"AI applied diff to editor ({result.get('hunks_applied', 0)} hunks)\ntab ID: {tab_id}"
                )

                return AIToolResult(
                    id=tool_call.id,
                    name="editor",
                    content=result['message']
                )

            # Diff failed to apply
            error_details = result.get('error_details', {})
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI diff application failed: {result['message']}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=f"Failed to apply diff: {result['message']}\n\nError details:\n{error_details}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to apply diff: {str(e)}") from e
