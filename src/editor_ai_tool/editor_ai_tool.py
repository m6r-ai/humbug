import asyncio
import difflib
import json
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
    AIToolTimeoutError,
)
from menai import Menai, MenaiError, MenaiCancelledException, MenaiString, MenaiList, MenaiValue
from menai import MenaiBufferingTraceWatcher
from editor_context.editor_context import EditorContext
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace import Mindspace


class EditorAITool(AITool):
    """
    AI tool for editor operations.

    Provides operations for reading, searching, navigating, and modifying
    editor content. Requires an editor tab to be open (use system tool to create tabs).
    """

    def __init__(self, mindspace: Mindspace):
        """
        Initialize the editor tool.

        Args:
            mindspace: The active mindspace model
        """
        self._mindspace = mindspace
        self._logger = logging.getLogger("EditorAITool")
        self._menai = Menai()

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return self._build_definition_from_operations(
            name="editor",
            description_prefix=(
            "Operations for working with editor tabs. Use this tool to read, search, navigate, "
            "and modify text content in open editor tabs. You must have an editor tab open first "
            "(use the system tool to open files)."
            ),
            additional_parameters=[
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
                    name="regexp",
                    type="boolean",
                    description="Whether to treat search_text as a regular expression (for search operation)",
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
                AIToolParameter(
                    name="program",
                    type="string",
                    description=(
                        "Menai expression for the transform operation. "
                        "May reference 'input-text' (full buffer content as a string) "
                        "and 'input-lines' (buffer lines as a list of strings). "
                        "Must evaluate to a string (new content) or a list of strings (new lines). "
                        "Changes are applied to the buffer; use save_file to persist."
                    ),
                    required=False
                ),
                AIToolParameter(
                    name="dry_run",
                    type="boolean",
                    description="If True, return the diff without applying changes to the buffer (for transform operation)",
                    required=False
                ),
            ]
        )

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Read, search, navigate, and modify text in open editor tabs."

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
                    "start_line and end_line (1-indexed, inclusive). Returns a dictionary with integer "
                    "line numbers as keys and line content as string values. Note: line content is returned as "
                    "JSON-encoded strings, so special characters (double quotes, backslashes, etc.) will appear "
                    "escaped — these escape sequences are not present in the source."
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
                allowed_parameters={"tab_id", "search_text", "case_sensitive", "regexp"},
                required_parameters={"tab_id", "search_text"},
                description="Find precise line numbers for all occurrences of text in an editor tab. "
                    "Returns list of matches with line (1-indexed), column (1-indexed), and context. "
                    "Supports optional case-sensitive and regular expression matching"
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
                    "operations. You must use the save_file operation to persist changes as this operation does not do "
                    "this automatically. Write diffs as they should appear in the editor text and do not add extra "
                    "escape characters to the diff content"
            ),
            "save_file": AIToolOperationDefinition(
                name="save_file",
                handler=self._save_file,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Save the current editor content to file. Requires user authorization"
            ),
            "transform": AIToolOperationDefinition(
                name="transform",
                handler=self._transform,
                extract_context=self._transform_context,
                allowed_parameters={"tab_id", "program", "dry_run"},
                required_parameters={"tab_id", "program"},
                description=(
                    "Apply a Menai program to the full editor buffer content and write the result "
                    "back to the buffer. The program may reference 'input-text' (full content as a "
                    "string) and 'input-lines' (content split on newlines as a list of strings). "
                    "It must return a string or a list of strings. "
                    "A unified diff is shown for user approval before the buffer is modified. "
                    "If dry_run is True, returns the diff without requesting authorisation or applying anything. "
                    "Use save_file afterward to persist the changes."
                )
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

        if operation == "transform":
            program = arguments.get("program", "")
            return f"`program` is:\n```menai\n{program}\n```"

        if operation != "apply_diff":
            return None

        if "diff_content" not in arguments:
            return None

        diff_content = arguments["diff_content"]
        return f"Diff content:\n```diff\n{diff_content}\n```"

    def _get_editor_context(self, arguments: Dict[str, Any]) -> EditorContext:
        """
        Retrieve the EditorContext for the given context_id.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            EditorContext instance

        Raises:
            AIToolExecutionError: If no editor context found for the given ID
        """
        context_id = self._get_required_str_value("tab_id", arguments)
        context = self._mindspace.contexts().get_model(context_id, EditorContext)
        if context is None:
            raise AIToolExecutionError(
                f"No editor context found with ID: {context_id}"
            )

        return context

    async def _read_lines(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read content from an editor tab."""
        arguments = tool_call.arguments
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        start_line = self._get_optional_int_value("start_line", arguments)
        end_line = self._get_optional_int_value("end_line", arguments)

        try:
            content = context.get_text_range(start_line, end_line)
            context_object = {}

            if not content:
                context_object[1] = ""
            else:
                content_lines = content.splitlines()
                for line_num, line_text in enumerate(content_lines):
                    context_object[line_num + start_line if start_line is not None else line_num + 1] = line_text

            if start_line is not None or end_line is not None:
                range_desc = f"lines {start_line or 1}-{end_line or 'end'}"
                log_msg = f"AI read editor content ({range_desc})\ntab ID: {context_id}"
            else:
                log_msg = f"AI read editor content\ntab ID: {context_id}"

            self._mindspace.add_interaction(MindspaceLogLevel.INFO, log_msg)

            range_value = "all lines"
            if start_line is not None or end_line is not None:
                range_value = f"{start_line or 1}-{end_line or 'end'}"

            result_object = {
                "range": range_value,
                "lines": context_object
            }

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=json.dumps(result_object, indent=2),
                context="json"
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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        try:
            cursor_info = context.get_cursor_info()

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested cursor info\ntab ID: {context_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=json.dumps(cursor_info, indent=2),
                context="json"
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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        try:
            editor_info = context.get_editor_info()

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested editor info\ntab ID: {context_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=json.dumps(editor_info, indent=2),
                context="json"
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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        if "line" not in arguments:
            raise AIToolExecutionError("No 'line' argument provided")

        line = arguments["line"]
        if not isinstance(line, int):
            raise AIToolExecutionError("'line' must be an integer")

        column = cast(int, self._get_optional_int_value("column", arguments, 1))

        try:
            context.goto_line(line, column)

            col_desc = f", column {column}" if column != 1 else ""
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI moved cursor to line {line}{col_desc}\ntab ID: {context_id}"
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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        search_text = self._get_required_str_value("search_text", arguments)

        case_sensitive = self._get_optional_bool_value("case_sensitive", arguments, False)
        regexp = self._get_optional_bool_value("regexp", arguments, False)

        try:
            matches = context.find_all_occurrences(search_text, case_sensitive, regexp)

            flags = []
            if case_sensitive:
                flags.append("case-sensitive")

            if regexp:
                flags.append("regexp")

            flag_desc = f" ({', '.join(flags)})" if flags else ""
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI searched for '{search_text}'{flag_desc}: {len(matches)} matches\ntab ID: {context_id}"
            )

            if not matches:
                return AIToolResult(
                    id=tool_call.id,
                    name="editor",
                    content=f"No matches found for '{search_text}'{flag_desc}"
                )

            result = {
                "search_text": search_text,
                "case_sensitive": case_sensitive,
                "regexp": regexp,
                "match_count": len(matches),
                "matches": matches
            }

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=json.dumps(result, indent=2),
                context="json"
            )

        except ValueError as e:
            raise AIToolExecutionError(str(e)) from e

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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        try:
            selected_text = context.get_selected_text()

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested selected text\ntab ID: {context_id}"
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
                content=selected_text,
                context="text"
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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        context_lines = cast(int, self._get_optional_int_value("context_lines", arguments, 3))
        if context_lines < 0:
            raise AIToolExecutionError("'context_lines' must be non-negative")

        try:
            diff = context.get_diff(context_lines)

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested diff\ntab ID: {context_id}"
            )

            if not diff:
                editor_info = context.get_editor_info()
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
                content=diff,
                context="diff"
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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        editor_info = context.get_editor_info()
        file_path = editor_info.get('file_path', '')

        if not file_path:
            raise AIToolExecutionError("Cannot save: editor has no file path (untitled file)")

        context_str = f"Save editor content to file: {file_path} (tab {context_id})"
        diff = context.get_diff(3)

        authorized = await request_authorization("editor", arguments, context_str, diff, True)
        if not authorized:
            raise AIToolAuthorizationDenied("User denied permission to save file")

        try:
            success = context.save()

            if not success:
                raise AIToolExecutionError("Save operation failed")

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI saved editor file: {file_path}\ntab ID: {context_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=f"Saved file: '{file_path}'"
            )

        except AIToolExecutionError:
            raise

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
        context = self._get_editor_context(arguments)
        context_id = context.context_id()

        diff_content = self._get_required_str_value("diff_content", arguments)

        try:
            result = context.apply_diff(diff_content)

            if result['success']:
                self._mindspace.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"AI applied diff to editor ({result.get('hunks_applied', 0)} hunks)\ntab ID: {context_id}"
                )

                return AIToolResult(
                    id=tool_call.id,
                    name="editor",
                    content=result['message']
                )

            error_details = result.get('error_details', {})
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI diff application failed: {result['message']}\ntab ID: {context_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=(
                    f"Failed to apply diff: {result['message']} No changes were applied"
                    " - the operation is atomic and all hunks must succeed for any"
                    f" changes to take effect.\n\nError details:\n{error_details}"
                )
            )

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to apply diff: {str(e)} No changes were applied"
                " - the operation is atomic and all hunks must succeed for any"
                " changes to take effect."
            ) from e

    def _transform_context(self, arguments: Dict[str, Any]) -> str | None:
        """Extract context for transform operation."""
        tab_id = arguments.get("tab_id", "")
        program = arguments.get("program", "")
        return (
            f"`tab_id` is: {tab_id}\n"
            f"`program` is:\n```menai\n{program}\n```"
        )

    def _transform_sync(
        self,
        content: str,
        expression: str
    ) -> tuple[str, list[str], bool]:
        """Apply a Menai transform to buffer content synchronously.

        Args:
            content: The current buffer text.
            expression: Menai expression referencing 'input-text' and 'input-lines'.

        Returns:
            Tuple of (new_content, traces, traces_clipped).

        Raises:
            AIToolExecutionError: If the program returns an invalid type.
            Various Menai exceptions: Propagated to the async caller.
        """
        lines = content.split('\n')
        bindings: Dict[str, MenaiValue] = {
            'input-text': MenaiString(content),
            'input-lines': MenaiList(tuple(MenaiString(line) for line in lines)),
        }

        watcher = MenaiBufferingTraceWatcher(max_traces=200)
        self._menai.set_trace_watcher(watcher)
        try:
            raw_result = self._menai.evaluate_raw_with_bindings(expression, bindings)
            traces = watcher.get_traces()
            was_clipped = watcher.is_clipped()

        finally:
            self._menai.set_trace_watcher(None)

        if isinstance(raw_result, MenaiString):
            new_content = raw_result.value

        elif isinstance(raw_result, MenaiList):
            if not all(isinstance(e, MenaiString) for e in raw_result.elements):
                raise AIToolExecutionError(
                    "Transform program returned a list containing non-string elements"
                )
            new_content = '\n'.join(cast(MenaiString, e).value for e in raw_result.elements)

        else:
            raise AIToolExecutionError(
                f"Transform program must return a string or list of strings, "
                f"got {raw_result.type_name()}"
            )

        return new_content, traces, was_clipped

    async def _transform(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Apply a Menai transform program to the editor buffer."""
        arguments = tool_call.arguments
        context = self._get_editor_context(arguments)
        context_id = context.context_id()
        program = self._get_required_str_value("program", arguments)
        dry_run = self._get_optional_bool_value("dry_run", arguments, False)

        if not program.strip():
            raise AIToolExecutionError("'program' must not be empty")

        original_content = context.get_text_range(None, None)

        try:
            task = asyncio.create_task(
                asyncio.to_thread(self._transform_sync, original_content, program)
            )
            try:
                new_content, traces, watcher_clipped = await asyncio.wait_for(task, timeout=30.0)

            except asyncio.TimeoutError:
                self._logger.warning("Menai transform timed out for tab '%s'", context_id)
                self._menai.vm.cancel()
                if not task.done():
                    try:
                        await asyncio.wait_for(task, timeout=1.0)

                    except (asyncio.TimeoutError, asyncio.CancelledError, MenaiCancelledException):
                        pass

                    except Exception as e:
                        self._logger.debug("Exception during transform cancellation: %s", e)

                raise AIToolTimeoutError("Menai transform timed out", 30.0)  # pylint: disable=raise-missing-from

        except AIToolTimeoutError:
            raise

        except AIToolExecutionError:
            raise

        except MenaiCancelledException as e:
            raise AIToolTimeoutError("Menai transform timed out", 30.0) from e

        except MenaiError as e:
            raise AIToolExecutionError(str(e)) from e

        except Exception as e:
            self._logger.error("Unexpected error in transform for tab '%s': %s", context_id, str(e), exc_info=True)
            raise AIToolExecutionError(f"Transform failed: {str(e)}") from e

        if original_content == new_content:
            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content="Transform produced no changes."
            )

        diff_lines = list(difflib.unified_diff(
            original_content.splitlines(),
            new_content.splitlines(),
            lineterm=''
        ))
        diff_str = '\n'.join(diff_lines)

        if dry_run:
            return AIToolResult(
                id=tool_call.id,
                name="editor",
                content=(
                    f"Dry run: transform would modify the buffer "
                    f"({len(diff_lines)} diff lines). No changes applied.\n\n{diff_str}"
                )
            )

        editor_info = context.get_editor_info()
        file_path = editor_info.get('file_path', f'tab {context_id}')
        context_str = f"Apply Menai transform to editor buffer: {file_path} (tab {context_id})"
        authorized = await request_authorization("editor", arguments, context_str, diff_str, True)
        if not authorized:
            raise AIToolAuthorizationDenied("User denied permission to apply Menai transform")

        result = context.apply_diff(diff_str)
        if not result['success']:
            raise AIToolExecutionError(f"Failed to apply transform: {result['message']}")

        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"AI applied Menai transform to editor ({result.get('hunks_applied', 0)} hunks)\ntab ID: {context_id}"
        )

        trace_str = '\n'.join(traces) if traces else ''
        result_obj: Dict[str, Any] = {
            "message": (
                f"Transform applied to buffer ({result.get('hunks_applied', 0)} hunks). "
                "Use save_file to persist."
            ),
            "trace_data": trace_str,
            "trace_data_clipped": "yes" if watcher_clipped else "no"
        }
        return AIToolResult(
            id=tool_call.id,
            name="editor",
            content=json.dumps(result_obj, indent=2),
            context="json"
        )
