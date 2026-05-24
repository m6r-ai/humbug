"""Listener protocol for delegate AI tool frontend integration."""

from typing import Protocol

from ai import AIConversation


class DelegateAIListener(Protocol):
    """
    Protocol for receiving delegate AI lifecycle notifications.

    Implementors attach a display (GUI tab, CLI output, etc.) to the child
    conversation before the prompt is submitted, and clean it up afterwards.

    on_conversation_created is called synchronously after the child
    AITranscriptConversation is created but before the prompt is submitted,
    so the implementor can register event callbacks on the AIConversation
    before any events fire.

    on_conversation_completed is called after the child conversation has
    finished (successfully or with an error), allowing the implementor to
    close or flush any display it created.
    """

    def on_conversation_created(
        self,
        child_conversation: AIConversation,
        session_path: str,
        parent_conversation: AIConversation
    ) -> None:
        """
        Notify that a child conversation has been created and is ready for display.

        Called synchronously before the prompt is submitted.  The implementor
        must register any event callbacks it needs before returning.

        Args:
            child_conversation: The child AIConversation instance
            session_path: Mindspace-relative path to the conversation transcript file
            parent_conversation: The parent AIConversation that initiated the delegation
        """

    def on_conversation_completed(self, session_path: str) -> None:
        """
        Notify that a child conversation has completed.

        Called after the continuation resolves.  The implementor should close
        or flush any display it created for this session.

        Args:
            session_path: Mindspace-relative path to the conversation transcript file
        """
