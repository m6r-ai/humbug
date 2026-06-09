import logging
import json
import os
import platform
import sys
from datetime import datetime, timezone
from typing import Any, Dict, List

from ai import AIConversation, AIConversationSettings, AIManager, AIReasoningCapability
from ai.ai_model import AIReasoningEffort
from ai_transcript_conversation import AITranscriptConversation
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
from conversation_context.conversation_context import ConversationContext
from mindspace.mindspace_error import MindspaceError
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace import Mindspace

from desktop.tab_manager import TabManager, TabManagerError
from desktop.version import CURRENT_VERSION


class SystemAITool(AITool):
    """
    System operations tool for tab lifecycle management.

    Provides operations for creating, opening, closing, and organizing tabs.
    This is the primary tool for managing the workspace layout. Use specific
    tab tools (editor, terminal, conversation, log, preview) for working with
    tab content.
    """

    def __init__(self, tab_manager: TabManager, mindspace: Mindspace):
        """
        Initialize the system tool.

        Args:
            column_manager: Column manager for layout queries and tab protection
            mindspace: The active mindspace model
        """
        self._tab_manager = tab_manager
        self._mindspace = mindspace
        self._ai_manager = AIManager()
        self._logger = logging.getLogger("SystemAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return self._build_definition_from_operations(
            name="system",
            description_prefix=(
            "Tab lifecycle and workspace management operations. Use this tool to create, open, "
            "close, and organize tabs. Returns tab IDs (GUIDs) that can be used with specific tab tools "
            "(editor, terminal, conversation, log, preview) to work with tab content."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="file_path",
                    type="string",
                    description=(
                        "Path to file or directory (for open_editor_tab, open_conversation_tab, open_preview_tab, open_diff_tab)"
                    ),
                    required=False
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="GUID of tab (for get_tab_info, close_tab, move_tab operations)",
                    required=False
                ),
                AIToolParameter(
                    name="target_column",
                    type="integer",
                    description="Target column index (0-based) for move_tab operation. Maximum 6 columns",
                    required=False
                ),
                AIToolParameter(
                    name="model",
                    type="string",
                    description="AI model to use (for new_conversation_tab operation)",
                    required=False
                ),
                AIToolParameter(
                    name="temperature",
                    type="number",
                    description="Temperature setting 0.0-1.0 (for new_conversation_tab operation)",
                    required=False
                ),
                AIToolParameter(
                    name="reasoning_effort",
                    type="string",
                    description="Reasoning effort level for models that support it "
                        f"(for new_conversation_tab operation). "
                        f"One of: {', '.join(AIReasoningEffort.values())}.",
                    required=False
                ),
            ]
        )

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Create, open, close, and organize UI tabs (editor, terminal, conversation, etc.)."

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "open_editor_tab": AIToolOperationDefinition(
                name="open_editor_tab",
                handler=self._open_editor_tab,
                extract_context=None,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open a file in an editor tab for the user to browse/edit. "
                    "Returns the tab GUID for use with the editor tool"
            ),
            "new_terminal_tab": AIToolOperationDefinition(
                name="new_terminal_tab",
                handler=self._new_terminal_tab,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Create a fully interactive terminal tab. "
                "This provides a terminal emulator connected to a new shell. "
                "You may interact with this terminal using the terminal tool. "
                    "Returns the tab GUID for use with the terminal tool"
            ),
            "open_conversation_tab": AIToolOperationDefinition(
                name="open_conversation_tab",
                handler=self._open_conversation_tab,
                extract_context=None,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open an existing conversation in a conversation tab for the user to browse/edit. "
                "You (the AI) cannot use this to send messages. "
                    "Returns the tab GUID for use with the conversation tool"
            ),
            "new_conversation_tab": AIToolOperationDefinition(
                name="new_conversation_tab",
                handler=self._new_conversation_tab,
                extract_context=None,
                allowed_parameters={"model", "temperature", "reasoning_effort"},
                required_parameters=set(),
                description="Create a new AI conversation tab, with optional model/temperature. "
                    "Returns the tab GUID for use with the conversation tool"
            ),
            "open_preview_tab": AIToolOperationDefinition(
                name="open_preview_tab",
                handler=self._open_preview_tab,
                extract_context=None,
                allowed_parameters={"file_path"},
                required_parameters=set(),
                description="Open a file/directory in a preview view tab. "
                    "If no file_path provided, opens mindspace root. "
                    "Returns the tab GUID for use with the preview tool"
            ),
            "open_diff_tab": AIToolOperationDefinition(
                name="open_diff_tab",
                handler=self._open_diff_tab,
                extract_context=None,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open a side-by-side git diff tab showing working-tree vs HEAD for a file. "
                    "Returns the tab GUID for use with the preview tool"
            ),
            "get_tab_info": AIToolOperationDefinition(
                name="get_tab_info",
                handler=self._get_tab_info,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters=set(),
                description="Get information about a tab, given its tab GUID. "
                    "If no tab_id provided, gets the current tab info"
            ),
            "close_tab": AIToolOperationDefinition(
                name="close_tab",
                handler=self._close_tab,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Close an existing tab. You must provide the tab_id parameter"
            ),
            "list_tabs": AIToolOperationDefinition(
                name="list_tabs",
                handler=self._list_tabs,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Enumerate all currently open tabs across all columns"
            ),
            "move_tab": AIToolOperationDefinition(
                name="move_tab",
                handler=self._move_tab,
                extract_context=None,
                allowed_parameters={"tab_id", "target_column"},
                required_parameters={"tab_id", "target_column"},
                description="Move a tab to a specific column by index. You must provide the tab_id and target_column "
                    "parameters. There are a maximum of 6 columns"
            ),
            "get_system_info": AIToolOperationDefinition(
                name="get_system_info",
                handler=self._get_system_info,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Get information about the Humbug system, current mindspace, operating system, "
                    "available AI models, and default shell"
            ),
        }

    def _validate_and_resolve_path(self, path_str: str) -> str:
        """
        Validate path is within mindspace and resolve to absolute path.

        Args:
            path_str: String path to validate and resolve

        Returns:
            Resolved absolute path within mindspace

        Raises:
            AIToolExecutionError: If path is invalid or outside mindspace
        """
        if not path_str:
            raise AIToolExecutionError("Path parameter is required")

        if path_str.startswith(os.sep):
            path_str = path_str[1:]

        abs_path = self._mindspace.get_absolute_path(path_str)

        relative_path = self._mindspace.get_mindspace_relative_path(abs_path)
        if relative_path is None:
            raise AIToolExecutionError(f"Path is outside mindspace boundaries: {path_str}")

        return abs_path

    def _requester_tab_id(self, requester_ref: Any) -> str | None:
        """Return the tab_id of the conversation that issued this tool call."""
        for info in self._mindspace.contexts().list_all():
            model = self._mindspace.contexts().get_model(info.context_id, ConversationContext)
            if model is not None and model.ai_transcript_conversation().inner_conversation() is requester_ref:
                return info.context_id

        return None

    async def _open_editor_tab(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open or create a file in an editor tab."""
        arguments = tool_call.arguments
        file_path_arg = self._get_required_str_value("file_path", arguments)
        file_path = self._validate_and_resolve_path(file_path_arg)

        # Return existing tab if this file is already open
        existing = self._mindspace.contexts().get_by_path_and_type(file_path, "editor")
        if existing is not None:
            self._tab_manager.make_tab_permanent(existing.context_id)
            self._mindspace.contexts().focus(existing.context_id)
            relative_path = self._mindspace.get_relative_path(file_path)
            return AIToolResult(
                id=tool_call.id, name="system",
                content=f"Opened editor tab for file: '{relative_path}', tab ID: {existing.context_id}"
            )

        try:
            directory = os.path.dirname(file_path)
            if directory and not os.path.exists(directory):
                os.makedirs(directory, exist_ok=True)

            requester_id = self._requester_tab_id(requester_ref) or ""
            context_id = self._mindspace.contexts().open(
                context_type="editor",
                path=file_path,
                title=os.path.basename(file_path),
                requester_id=requester_id,
            )

            relative_path = self._mindspace.get_relative_path(file_path)
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened editor for file: '{relative_path}'\ntab ID: {context_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened editor tab for file: '{relative_path}', tab ID: {context_id}"
            )

        except OSError as e:
            raise AIToolExecutionError(f"Failed to access file '{file_path_arg}': {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open file '{file_path_arg}' for editing: {str(e)}") from e

    async def _new_terminal_tab(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Create a new terminal tab."""
        try:
            requester_id = self._requester_tab_id(requester_ref) or ""
            context_id = self._mindspace.contexts().open(
                context_type="terminal",
                title="Terminal",
                requester_id=requester_id,
            )

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI created new terminal\ntab ID: {context_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Created new terminal, tab ID: {context_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to create terminal: {str(e)}") from e

    async def _open_conversation_tab(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open an existing conversation tab."""
        arguments = tool_call.arguments
        file_path_arg = self._get_required_str_value("file_path", arguments)
        conversation_path = self._validate_and_resolve_path(file_path_arg)

        # Return existing tab if this conversation is already open
        existing = self._mindspace.contexts().get_by_path_and_type(conversation_path, "conversation")
        if existing is not None:
            self._tab_manager.make_tab_permanent(existing.context_id)
            self._mindspace.contexts().focus(existing.context_id)
            return AIToolResult(
                id=tool_call.id, name="system",
                content=f"Opened conversation for: '{conversation_path}', tab ID: {existing.context_id}"
            )

        try:
            self._mindspace.ensure_mindspace_dir("conversations")

            requester_id = self._requester_tab_id(requester_ref) or ""
            title = os.path.splitext(os.path.basename(conversation_path))[0]
            context_id = self._mindspace.contexts().open(
                context_type="conversation",
                path=conversation_path,
                title=title,
                requester_id=requester_id,
            )

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened conversation for: '{conversation_path}'\ntab ID: {context_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened conversation for: '{conversation_path}', tab ID: {context_id}"
            )

        except MindspaceError as e:
            raise AIToolExecutionError(f"Failed to create conversation directory: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open conversation: {str(e)}") from e

    async def _new_conversation_tab(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Create a new conversation tab."""
        arguments = tool_call.arguments
        model = self._get_optional_str_value("model", arguments)
        temperature = arguments.get("temperature")
        reasoning_effort_arg = self._get_optional_str_value("reasoning_effort", arguments)

        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError("'temperature' must be a number")
            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError("'temperature' must be between 0.0 and 1.0")

        reasoning = None
        reasoning_effort: str | None = None
        effective_model: str = ""
        effective_provider: str = ""

        if model:
            ai_backends = self._ai_manager.get_backends()
            available_keys = list(AIConversationSettings.iter_models_by_backends(ai_backends))
            available_display = [
                AIConversationSettings.get_display_name(m, p) for (m, p) in available_keys
            ]
            matched_key = next(
                (k for k in available_keys if AIConversationSettings.get_display_name(k[0], k[1]) == model),
                None
            )
            if matched_key is None:
                raise AIToolExecutionError(
                    f"Model '{model}' is not available. Available models: {', '.join(available_display)}"
                )
            effective_model, effective_provider = matched_key
            model_config = AIConversationSettings.MODELS.get(matched_key)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        if reasoning_effort_arg is not None:
            if not AIReasoningEffort.is_valid(reasoning_effort_arg):
                raise AIToolExecutionError(
                    f"'reasoning_effort' must be one of: {', '.join(AIReasoningEffort.values())}"
                )
            if effective_model and effective_provider:
                supported = AIConversationSettings.get_supported_reasoning_efforts(effective_model, effective_provider)
                if supported and reasoning_effort_arg not in supported:
                    raise AIToolExecutionError(
                        f"Model '{effective_model}' does not support reasoning_effort '{reasoning_effort_arg}'. "
                        f"Supported efforts: {', '.join(supported)}"
                    )
            reasoning_effort = reasoning_effort_arg

        try:
            self._mindspace.ensure_mindspace_dir("conversations")

            # Build the conversation settings
            settings = self._mindspace.settings()
            resolved_model = effective_model or (settings.model if settings else "")
            resolved_provider = effective_provider or (settings.provider if settings else "")
            resolved_temperature = temperature if temperature is not None else (settings.temperature if settings else None)
            resolved_reasoning = reasoning or (settings.reasoning if settings else AIReasoningCapability.NO_REASONING)
            resolved_effort = reasoning_effort or (settings.reasoning_effort if settings else None)

            conversation_settings = AIConversationSettings(
                model=resolved_model,
                provider=resolved_provider,
                temperature=resolved_temperature if AIConversationSettings.supports_temperature(
                    resolved_model, resolved_provider, resolved_effort
                ) else None,
                reasoning=resolved_reasoning,
                reasoning_effort=resolved_effort,
            )

            # Generate conversation path
            timestamp = datetime.now(timezone.utc)
            conversation_title = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
            filename = os.path.join("conversations", f"{conversation_title}.conv")
            full_path = self._mindspace.get_absolute_path(filename)

            # Build the AIConversation with settings applied, wrap in transcript
            ai_conversation = AIConversation()
            ai_conversation.update_conversation_settings(conversation_settings)
            transcript = AITranscriptConversation(full_path, ai_conversation)

            requester_id = self._requester_tab_id(requester_ref) or ""
            context_id = self._mindspace.contexts().open(
                context_type="conversation",
                path=full_path,
                title=conversation_title,
                initial_model=transcript,
                requester_id=requester_id,
            )

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI created new conversation\ntab ID: {context_id}"
            )
            result_parts = [f"Created new conversation, tab ID: {context_id}"]
            if model:
                result_parts.append(f"model: {model}")
            if temperature is not None:
                result_parts.append(f"temperature: {temperature}")
            if reasoning_effort:
                result_parts.append(f"reasoning_effort: {reasoning_effort}")

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=", ".join(result_parts)
            )

        except (MindspaceError, TabManagerError) as e:
            raise AIToolExecutionError(f"Failed to create conversation: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to create conversation: {str(e)}") from e

    async def _open_preview_tab(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open preview view for a specific location or mindspace root."""
        arguments = tool_call.arguments
        file_path_arg = arguments.get("file_path", "")

        if file_path_arg:
            preview_path = self._validate_and_resolve_path(file_path_arg)
        else:
            preview_path = self._mindspace.get_absolute_path(".")

        # Return existing tab if this path is already open in a preview
        existing = self._mindspace.contexts().get_by_path_and_type(preview_path, "preview")
        if existing is not None:
            self._tab_manager.make_tab_permanent(existing.context_id)
            self._mindspace.contexts().focus(existing.context_id)
            relative_path = self._mindspace.get_relative_path(preview_path)
            location = relative_path if relative_path else "."
            return AIToolResult(
                id=tool_call.id, name="system",
                content=f"Opened preview tab for: '{location}', tab ID: {existing.context_id}"
            )

        try:
            requester_id = self._requester_tab_id(requester_ref) or ""
            norm_path = os.path.normpath(preview_path)
            name = os.path.basename(norm_path)
            is_mindspace_root = norm_path == os.path.normpath(self._mindspace.mindspace_path())
            title = f"[{name.upper()}]" if is_mindspace_root else name

            context_id = self._mindspace.contexts().open(
                context_type="preview",
                path=preview_path,
                title=title,
                requester_id=requester_id,
            )

            relative_path = self._mindspace.get_relative_path(preview_path)
            location = relative_path if relative_path else "."
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened preview tab for: '{location}'\ntab ID: {context_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened preview tab for: '{location}', tab ID: {context_id}"
            )

        except TabManagerError as e:
            raise AIToolExecutionError(f"Failed to open preview: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open preview: {str(e)}") from e

    async def _open_diff_tab(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open a side-by-side git diff tab for a file."""
        arguments = tool_call.arguments
        file_path_arg = self._get_required_str_value("file_path", arguments)
        file_path = self._validate_and_resolve_path(file_path_arg)

        if not os.path.exists(file_path):
            raise AIToolExecutionError(f"File not found: '{file_path_arg}'")

        if os.path.isdir(file_path):
            raise AIToolExecutionError(f"Cannot diff a directory: '{file_path_arg}'")

        # Return existing tab if this file is already open in a diff tab
        existing = self._mindspace.contexts().get_by_path_and_type(file_path, "diff")
        if existing is not None:
            self._tab_manager.make_tab_permanent(existing.context_id)
            self._mindspace.contexts().focus(existing.context_id)
            relative_path = self._mindspace.get_relative_path(file_path)
            return AIToolResult(
                id=tool_call.id, name="system",
                content=f"Opened diff tab for file: '{relative_path}', tab ID: {existing.context_id}"
            )

        try:
            requester_id = self._requester_tab_id(requester_ref) or ""
            context_id = self._mindspace.contexts().open(
                context_type="diff",
                path=file_path,
                title=os.path.basename(file_path),
                requester_id=requester_id,
            )

            relative_path = self._mindspace.get_relative_path(file_path)
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened diff for file: '{relative_path}'\ntab ID: {context_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened diff tab for file: '{relative_path}', tab ID: {context_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open diff for '{file_path_arg}': {str(e)}") from e

    async def _get_tab_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get information about a specific tab by ID or the current tab."""
        arguments = tool_call.arguments
        tab_id = arguments.get("tab_id")

        if not tab_id:
            current_tab = self._tab_manager.get_current_tab()
            if not current_tab:
                raise AIToolExecutionError("No current tab is open")
            tab_id = current_tab.tab_id()

        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        tab_info = self._tab_manager.get_tab_info_by_id(tab_id)
        if not tab_info:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"AI requested info for tab ID: {tab_id}"
        )

        return AIToolResult(
            id=tool_call.id,
            name="system",
            content=json.dumps(tab_info, indent=2),
            context="json"
        )

    async def _close_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Close an existing tab by ID."""
        arguments = tool_call.arguments

        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        try:
            tab = self._tab_manager.get_tab_by_id(tab_id)
            if not tab:
                raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

            if tab.is_modified():
                tab_info = self._tab_manager.get_tab_info_by_id(tab_id)
                tab_title = tab_info.get('title', tab_id) if tab_info else tab_id
                context = f"Close tab '{tab_title}' with unsaved changes? Unsaved modifications will be lost."
                authorized = await request_authorization("system", arguments, context, None, True)
                if not authorized:
                    raise AIToolAuthorizationDenied(
                        f"User denied permission to close modified tab '{tab_title}'"
                    )

            # Close via registry — TabManager._on_context_closed will close the Qt tab
            self._mindspace.contexts().close(tab_id)

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI closed tab\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content="Closed tab"
            )

        except AIToolAuthorizationDenied:
            raise

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Failed to close tab {tab_id}: {str(e)}") from e

    async def _list_tabs(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """List all currently open tabs across all columns."""
        try:
            tab_info = self._tab_manager.list_all_tabs()

            if not tab_info:
                self._mindspace.add_interaction(
                    MindspaceLogLevel.INFO,
                    "AI requested tab list: no tabs currently open"
                )
                return AIToolResult(
                    id=tool_call.id,
                    name="system",
                    content="No tabs are currently open."
                )

            result = {
                "total_tabs": len(tab_info),
                "total_columns": self._tab_manager.num_colunns(),
                "tabs": tab_info
            }

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested tab list: {len(tab_info)} tabs across {result['total_columns']} columns"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=json.dumps(result, indent=2),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to list tabs: {str(e)}") from e

    async def _move_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Move a tab to a specific column."""
        arguments = tool_call.arguments

        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        if "target_column" not in arguments:
            raise AIToolExecutionError("No 'target_column' argument provided")

        target_column = arguments["target_column"]
        if not isinstance(target_column, int):
            raise AIToolExecutionError("'target_column' must be an integer")

        if target_column < 0:
            raise AIToolExecutionError(f"Target column must be non-negative, got {target_column}")

        try:
            self._tab_manager.move_tab_to_column(tab_id, target_column)

        except TabManagerError as e:
            raise AIToolExecutionError(str(e)) from e

        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"AI moved tab {tab_id} to column {target_column}"
        )
        return AIToolResult(
            id=tool_call.id,
            name="system",
            content=f"Moved tab {tab_id} to column {target_column}"
        )

    async def _get_system_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get system and mindspace information."""
        try:
            system_info = {
                "version": f"v{CURRENT_VERSION}",
                "platform": sys.platform,
                "platform_details": platform.platform(),
                "architecture": platform.machine()
            }

            mindspace_path = self._mindspace.mindspace_path()
            mindspace_name = os.path.basename(mindspace_path)
            mindspace_info = {
                "name": mindspace_name,
                "path": mindspace_path
            }

            ai_backends = self._ai_manager.get_backends()
            models_by_backend: Dict[str, List[str]] = {}
            for (model_name, provider) in AIConversationSettings.iter_models_by_backends(ai_backends):
                display = AIConversationSettings.get_display_name(model_name, provider)
                models_by_backend.setdefault(provider, []).append(display)

            ai_info = {"models_by_backend": models_by_backend}

            if sys.platform == 'win32':
                shell_env = os.environ.get('COMSPEC', 'cmd.exe')
            else:
                shell_env = os.environ.get('SHELL', '/bin/sh')

            if os.path.isabs(shell_env):
                shell_path = shell_env
            else:
                shell_path = shell_env
                for path_dir in os.environ.get('PATH', '').split(os.pathsep):
                    potential_path = os.path.join(path_dir, shell_env)
                    if os.path.isfile(potential_path) and os.access(potential_path, os.X_OK):
                        shell_path = potential_path
                        break

            shell_info = {
                "default_shell": os.path.basename(shell_env),
                "shell_path": shell_path,
                "cwd": mindspace_path
            }

            result = {
                "system": system_info,
                "mindspace": mindspace_info,
                "ai": ai_info,
                "shell": shell_info
            }

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                "AI requested system information"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=json.dumps(result, indent=2),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get system info: {str(e)}") from e
