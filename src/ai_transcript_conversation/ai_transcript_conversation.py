"""Transcript-backed AI conversation."""

import logging
from typing import Any, Callable, Dict, List

from ai import (
    AIConversation, AIConversationEvent, AIConversationHistory,
    AIConversationSettings, AIMessage
)
from ai_transcript_conversation.ai_conversation_transcript_error import (
    AIConversationTranscriptError
)
from ai_transcript_conversation.ai_conversation_transcript_handler import (
    AIConversationTranscriptHandler
)


class AITranscriptConversation:
    """
    A persistent, transcript-backed AI conversation.

    Wraps AIConversation with an AIConversationTranscriptHandler so that
    the conversation state is automatically persisted after each significant
    event.  The GUI should use this class rather than AIConversation directly.

    AIConversation itself has no knowledge of persistence — it is kept pure.
    This wrapper is the composition point between conversation logic and storage.
    """

    def __init__(
        self,
        path: str,
        ai_conversation: AIConversation | None = None
    ) -> None:
        """
        Initialise a transcript conversation.

        Args:
            path: Full path to the transcript file.  The file is created if it
                does not exist.
            ai_conversation: An existing AIConversation to adopt (used for
                delegated child conversations).  If None a new one is created.
        """
        self._logger = logging.getLogger("AITranscriptConversation")
        self._transcript_handler = AIConversationTranscriptHandler(path)

        if ai_conversation is not None:
            self._conversation = ai_conversation

        else:
            self._conversation = AIConversation()

        self._register_persistence_callbacks()

    def path(self) -> str:
        """Return the transcript file path."""
        return self._transcript_handler.get_path()

    def set_path(self, new_path: str) -> None:
        """
        Update the transcript file path.

        Args:
            new_path: New path for the transcript file
        """
        self._transcript_handler.set_path(new_path)

    def read(self) -> AIConversationHistory:
        """
        Read and return the conversation history from the transcript file.

        Returns:
            AIConversationHistory loaded from disk
        """
        return self._transcript_handler.read()

    def write_transcript(self) -> None:
        """
        Write the current conversation history to the transcript file.

        Logs but does not raise on failure so callers do not need try/except
        for the common case.
        """
        try:
            self._transcript_handler.write(self._conversation.get_conversation_history())

        except AIConversationTranscriptError:
            self._logger.exception("Failed to write transcript")

    def _register_persistence_callbacks(self) -> None:
        """Register callbacks that persist the transcript after key events."""
        self._conversation.register_callback(
            AIConversationEvent.AI_CONNECTED, self._on_persist_event
        )
        self._conversation.register_callback(
            AIConversationEvent.ERROR, self._on_persist_on_error
        )
        self._conversation.register_callback(
            AIConversationEvent.MESSAGE_COMPLETED, self._on_persist_event
        )

    def _unregister_persistence_callbacks(self) -> None:
        """Unregister persistence callbacks."""
        self._conversation.unregister_callback(
            AIConversationEvent.AI_CONNECTED, self._on_persist_event
        )
        self._conversation.unregister_callback(
            AIConversationEvent.ERROR, self._on_persist_on_error
        )
        self._conversation.unregister_callback(
            AIConversationEvent.MESSAGE_COMPLETED, self._on_persist_event
        )

    async def _on_persist_event(self, *_args: Any, **_kwargs: Any) -> None:
        """Persist the transcript after any event that modifies history."""
        self.write_transcript()

    async def _on_persist_on_error(
        self, _retries_exhausted: bool, _message: AIMessage
    ) -> None:
        """Persist the transcript after an error message is added."""
        self.write_transcript()

    def get_conversation_history(self) -> AIConversationHistory:
        """Return the current conversation history."""
        return self._conversation.get_conversation_history()

    def set_conversation_history(self, history: AIConversationHistory) -> None:
        """
        Replace the conversation history and persist it immediately.

        Used when a forked history is applied to a new tab.

        Args:
            history: The history to apply
        """
        try:
            self._transcript_handler.write(history)

        except AIConversationTranscriptError as e:
            raise AIConversationTranscriptError(
                f"Failed to write transcript for new history: {str(e)}"
            ) from e

        self._conversation.load_history(history)

    def load_message_history(self, messages: List[AIMessage]) -> None:
        """
        Load a list of messages into the conversation history.

        Args:
            messages: Messages to load
        """
        self._conversation.load_message_history(messages)

    def load_history(self, history: AIConversationHistory) -> None:
        """
        Load a complete conversation history including messages and attachments.

        Prefer this over load_message_history when you have a full
        AIConversationHistory object, as it preserves the attachment store.

        Args:
            history: Full conversation history to restore
        """
        self._conversation.load_history(history)

    def truncate_to_message(self, message_id: str) -> str | None:
        """
        Truncate the conversation to just before the given user message, then
        persist the result.

        Args:
            message_id: ID of the user message to truncate from

        Returns:
            The content of the truncated message (for restoring to the input
            box), or None if the message was not found or is not a user message.
        """
        content = self._conversation.truncate_to_message(message_id)
        if content is not None:
            self.write_transcript()

        return content

    def fork_history(self, message_index: int) -> AIConversationHistory:
        """
        Return a copy of the conversation history up to (but not including)
        the given message index, with only the relevant attachments included.

        The caller is responsible for creating a new AITranscriptConversation
        with a new path and calling set_conversation_history with the result.
        The current conversation is not modified.

        Args:
            message_index: Messages before this index are included in the fork

        Returns:
            AIConversationHistory suitable for use in a new conversation
        """
        return self._conversation.fork_history_to_index(message_index)

    def inner_conversation(self) -> AIConversation:
        """
        Return the underlying AIConversation instance.

        This is provided for cases where the caller needs to register its own
        callbacks directly on the inner conversation (e.g. the GUI widget).
        """
        return self._conversation

    def is_streaming(self) -> bool:
        """Check if the conversation is currently streaming."""
        return self._conversation.is_streaming()

    def conversation_settings(self) -> AIConversationSettings:
        """Return current conversation settings."""
        return self._conversation.conversation_settings()

    def update_conversation_settings(self, new_settings: AIConversationSettings) -> None:
        """
        Update conversation settings.

        Args:
            new_settings: New settings to apply
        """
        self._conversation.update_conversation_settings(new_settings)

    def get_token_counts(self) -> Dict[str, int]:
        """Return token counts from the last response."""
        return self._conversation.get_token_counts()

    def register_callback(
        self, event: AIConversationEvent, callback: Callable
    ) -> None:
        """
        Register a callback for a conversation event.

        Args:
            event: Event to register for
            callback: Callback to invoke
        """
        self._conversation.register_callback(event, callback)

    def unregister_callback(
        self, event: AIConversationEvent, callback: Callable
    ) -> None:
        """
        Unregister a previously registered callback.

        Args:
            event: Event to unregister from
            callback: Callback to remove
        """
        self._conversation.unregister_callback(event, callback)

    async def submit_message(
        self,
        requester: str | None,
        user_message: str,
        attachment_guids: List[str] | None = None
    ) -> None:
        """
        Submit a user message to the conversation.

        Args:
            requester: Name of the user submitting the message (or None)
            user_message: The user message to submit
            attachment_guids: Optional list of attachment GUIDs
        """
        await self._conversation.submit_message(
            requester, user_message, attachment_guids=attachment_guids
        )

    def cancel_current_tasks(self, notify: bool = True) -> None:
        """
        Cancel any ongoing AI response tasks.

        Args:
            notify: Whether to emit a cancellation error message
        """
        self._conversation.cancel_current_tasks(notify)

    async def retry_last_request(self) -> List[str]:
        """
        Retry the last failed request.

        Returns:
            List of message IDs removed from history
        """
        return await self._conversation.retry_last_request()

    async def approve_pending_tool_calls(self) -> None:
        """Approve the current pending tool authorisation."""
        await self._conversation.approve_pending_tool_calls()

    async def reject_pending_tool_calls(self, reason: str = "User rejected tool call") -> None:
        """
        Reject the current pending tool authorisation.

        Args:
            reason: Human-readable reason for rejection
        """
        await self._conversation.reject_pending_tool_calls(reason)

    def close(self) -> None:
        """
        Clean up the transcript conversation.

        Unregisters persistence callbacks so the inner AIConversation can be
        garbage collected cleanly.
        """
        self._unregister_persistence_callbacks()
