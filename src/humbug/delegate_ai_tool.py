"""GUI adapter that wires DelegateAITool to the Humbug column manager."""

import os
from datetime import datetime, timezone
from typing import cast, Dict

from ai import AIConversation, AIConversationSettings
from ai_tool import AIToolExecutionError
from delegate_ai_tool import DelegateAITool
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel

from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.column_manager_error import ColumnManagerError


def _make_delegate_ai_tool(column_manager: ColumnManager, mindspace: Mindspace) -> DelegateAITool:
    """
    Build a fully wired DelegateAITool for the GUI environment.

    Args:
        column_manager: The application column manager
        mindspace: The active mindspace model

    Returns:
        A DelegateAITool instance ready for registration with the tool manager
    """
    # tab_ids tracks session_path -> tab_id for cleanup on completion
    tab_ids: Dict[str, str] = {}

    def generate_conversation_path() -> str:
        timestamp = datetime.now(timezone.utc)
        title = "dAI-" + timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
        filename = os.path.join("conversations", f"{title}.conv")
        mindspace.ensure_mindspace_dir("conversations")
        return mindspace.get_absolute_path(filename)

    def resolve_session_path(session_id: str) -> str:
        if not session_id:
            raise AIToolExecutionError("session_id must not be empty")

        if session_id.startswith("/") or session_id.startswith(os.sep):
            session_id = session_id[1:]

        normalized = os.path.normpath(session_id)
        if normalized.startswith(".."):
            raise AIToolExecutionError(f"session_id attempts path traversal: {session_id}")

        abs_path = mindspace.get_absolute_path(normalized)
        relative = mindspace.get_mindspace_relative_path(abs_path)
        if relative is None:
            raise AIToolExecutionError(f"session_id is outside mindspace boundaries: {session_id}")

        if not os.path.isfile(abs_path):
            raise AIToolExecutionError(f"session_id does not refer to an existing file: {session_id}")

        return abs_path

    def compute_session_id(absolute_path: str) -> str:
        relative = mindspace.get_mindspace_relative_path(absolute_path)
        return relative if relative is not None else absolute_path

    def get_default_settings() -> AIConversationSettings:
        settings = mindspace.settings()
        if settings is None:
            return AIConversationSettings()
        return AIConversationSettings(
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
        )

    def log_interaction(level: str, message: str) -> None:
        log_level = MindspaceLogLevel.INFO
        if level == "warn":
            log_level = MindspaceLogLevel.WARN
        elif level == "error":
            log_level = MindspaceLogLevel.ERROR
        elif level == "trace":
            log_level = MindspaceLogLevel.TRACE
        mindspace.add_interaction(log_level, message)

    def on_conversation_created(
        child_conversation: AIConversation,
        session_path: str,
        parent_conversation: AIConversation
    ) -> None:
        parent_tab = column_manager.find_tab_by_ai_conversation(parent_conversation)
        if parent_tab:
            column_manager.protect_tab(parent_tab.tab_id())

        try:
            conversation_tab = column_manager.new_conversation(
                child=True,
                ai_conversation=child_conversation
            )

        except ColumnManagerError:
            # Tab creation failed — the conversation will still run and persist;
            # the user just won't have a live view.
            return

        finally:
            if parent_tab:
                column_manager.unprotect_tab(parent_tab.tab_id())

        tab_ids[session_path] = conversation_tab.tab_id()

        # Move the child tab one column to the right of the parent
        if parent_tab:
            parent_info = column_manager.get_tab_info_by_id(parent_tab.tab_id())
            if parent_info:
                target_column = min(cast(int, parent_info["column_index"]) + 1, 5)
                try:
                    column_manager.move_tab_to_column(conversation_tab.tab_id(), target_column)
                except ColumnManagerError:
                    pass  # Best effort — tab is still functional if move fails

    def on_conversation_completed(session_path: str) -> None:
        tab_id = tab_ids.pop(session_path, None)
        if tab_id:
            column_manager.close_tab_by_id(tab_id)

    return DelegateAITool(
        generate_conversation_path=generate_conversation_path,
        compute_session_id=compute_session_id,
        resolve_session_path=resolve_session_path,
        get_default_settings=get_default_settings,
        log_interaction=log_interaction,
        on_conversation_created=on_conversation_created,
        on_conversation_completed=on_conversation_completed,
    )
