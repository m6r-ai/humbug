"""Audit trail and usage for mission conversations that have no tab."""

from typing import Any

from ai import AIConversationEvent, AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel
from mission.mission_tool_label import tool_call_label


LOG_PREVIEW_CHARS = 120


def _summarize_for_log(text: str) -> str:
    """Return a single-line excerpt of text for the interaction log."""
    flat = " ".join(text.split())
    if len(flat) <= LOG_PREVIEW_CHARS:
        return flat

    return f"{flat[:LOG_PREVIEW_CHARS]}..."


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
        self._history.set_on_usage_changed(self._on_usage_changed)
        self._conversation.register_callback(AIConversationEvent.MESSAGE_COMPLETED, self._on_message_completed)
        self._conversation.register_callback(
            AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, self._on_message_completed
        )

    def detach(self) -> None:
        """Stop recording."""
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
        content = _summarize_for_log(message.content)

        if message.source == AIMessageSource.REASONING:
            if content:
                self._log(f"AI reasoning: '{content}'")

        elif message.source == AIMessageSource.AI:
            if content:
                self._log(f"AI response: '{content}'")

            elif message.tool_calls:
                tools = ", ".join(tool_call_label(tc) for tc in message.tool_calls)
                self._log(f"AI requested tool calls: {tools}")

        elif message.source == AIMessageSource.USER and not message.tool_results:
            if message.user_name:
                self._log(f"AI '{message.user_name}' submitted prompt: '{content}'")

            else:
                self._log(f"Human submitted prompt: '{content}'")

        elif message.source == AIMessageSource.USER_QUEUED:
            self._log(f"Human queued prompt: '{content}'")

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
