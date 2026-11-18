"""Audit trail and usage for mission conversations that have no tab."""

from typing import Any

from ai import AIConversationEvent, AIMessage
from ai_transcript_conversation import AITranscriptConversation
from mindspace.mindspace import Mindspace
from mindspace.mindspace_conversation_log import completed_message_entry
from mindspace.mindspace_log_level import MindspaceLogLevel


class MissionConversationAuditor:
    """
    Records a mission conversation that runs without a tab.

    A conversation tab is what writes a conversation's turns to the interaction
    log and adds its token usage to the mindspace totals.  A mission followed in
    Mission Control runs its lead conversation without a tab, so this records
    the same entries in the same form.  It is detached before a tab takes the
    conversation over, so nothing is recorded twice.
    """

    def __init__(self, mindspace: Mindspace, transcript: AITranscriptConversation) -> None:
        """
        Start recording a conversation.

        Args:
            mindspace: The open mindspace
            transcript: The conversation to record
        """
        self._mindspace = mindspace
        self._transcript = transcript
        self._conversation = transcript.inner_conversation()
        self._history = transcript.get_conversation_history()
        self._baseline = self._usage_totals()
        # Kept so detach() can tell whether the history's one usage callback is still this one.
        self._usage_callback = self._on_usage_changed
        self._history.set_on_usage_changed(self._usage_callback)
        self._conversation.register_callback(AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed)
        self._conversation.register_callback(
            AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_completed
        )

    def detach(self) -> None:
        """Stop recording."""
        # The history has one usage callback.  Only this auditor's own is cleared, so a
        # tab that has already installed its callback keeps it.
        if self._history.on_usage_changed() is self._usage_callback:
            self._history.set_on_usage_changed(None)

        self._conversation.unregister_callback(AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed)
        self._conversation.unregister_callback(
            AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_completed
        )

    def _log(self, content: str) -> None:
        """Append an entry attributed to the conversation."""
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            content,
            context=self._mindspace.conversation_log_context(self._conversation)
        )

    async def _on_message_completed(self, message: AIMessage, *_args: Any) -> None:
        """Record a completed prompt, reasoning step or response."""
        entry = completed_message_entry(message)
        if entry:
            self._log(entry)

    def _usage_totals(self) -> dict[str, int]:
        """Return the conversation's cumulative token totals."""
        counts = self._history.get_token_counts()
        cache = self._history.get_cache_totals()
        return {
            "input_total": counts["input_total"],
            "output_total": counts["output_total"],
            "cache_write_total": cache["cache_write_total"],
            "cache_read_total": cache["cache_read_total"],
        }

    def _on_usage_changed(self) -> None:
        """Add the tokens used since the last update to the mindspace's usage."""
        current = self._usage_totals()
        baseline = self._baseline
        self._baseline = current
        delta = {key: value - baseline[key] for key, value in current.items()}
        if not any(delta.values()):
            return

        settings = self._transcript.conversation_settings()
        self._mindspace.update_usage(
            provider=settings.provider,
            model=settings.model,
            input_tokens=delta["input_total"],
            output_tokens=delta["output_total"],
            cache_write_tokens=delta["cache_write_total"],
            cache_read_tokens=delta["cache_read_total"],
        )
