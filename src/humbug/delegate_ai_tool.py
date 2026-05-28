"""GUI adapter that wires DelegateAITool to the Humbug column manager."""

import os
from datetime import datetime, timezone
from typing import Dict

from ai import AIConversation, AIConversationSettings
from ai_tool import AIToolExecutionError
from ai_transcript_conversation import AITranscriptConversation
from delegate_ai_tool import DelegateAITool
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.context.conversation_context import ConversationContext


def _make_delegate_ai_tool(mindspace: Mindspace) -> DelegateAITool:
    """
    Build a fully wired DelegateAITool for the GUI environment.

    Args:
        mindspace: The active mindspace model

    Returns:
        A DelegateAITool instance ready for registration with the tool manager
    """
    # context_ids tracks session_path -> context_id for cleanup on completion
    context_ids: Dict[str, str] = {}

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
        child_transcript: AITranscriptConversation,
        session_path: str,
        parent_conversation: AIConversation
    ) -> None:
        parent_context_id: str | None = None
        for info in mindspace.contexts().list_all():
            model = mindspace.contexts().get_model(info.context_id, ConversationContext)
            if model is not None and model.ai_transcript_conversation().inner_conversation() is parent_conversation:
                parent_context_id = info.context_id
                break

        try:
            title = os.path.splitext(os.path.basename(child_transcript.path()))[0]
            context_id = mindspace.contexts().open(
                context_type="conversation",
                path=child_transcript.path(),
                title=title,
                initial_model=child_transcript,
                requester_id=parent_context_id or "",
            )
            context_ids[session_path] = context_id
        except Exception:
            # Tab creation failed — the conversation will still run and persist;
            # the user just won't have a live view.
            pass

    def on_conversation_completed(session_path: str) -> None:
        context_id = context_ids.pop(session_path, None)
        if context_id:
            mindspace.contexts().close(context_id)

    return DelegateAITool(
        generate_conversation_path=generate_conversation_path,
        compute_session_id=compute_session_id,
        resolve_session_path=resolve_session_path,
        get_default_settings=get_default_settings,
        log_interaction=log_interaction,
        on_conversation_created=on_conversation_created,
        on_conversation_completed=on_conversation_completed,
    )
