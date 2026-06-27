"""AI conversation state management."""

from dataclasses import dataclass
from typing import Dict, List, Tuple
import uuid

from ai.ai_message import AIMessage
from ai.ai_usage import AIUsage


@dataclass
class AIConversationParent:
    """Identifies the parent delegation relationship for a child conversation."""
    message_id: str
    tool_call_id: str


class AIConversationHistory:
    """Manages the conversation history and state."""

    def __init__(
        self,
        messages: List[AIMessage] | None = None,
        version: str = "0.1",
        parent: AIConversationParent | None = None,
        attachments: Dict[str, Dict] | None = None
    ) -> None:
        """Initialize conversation history with optional metadata."""
        self._messages: List[AIMessage] = messages if messages is not None else []
        self._version = version
        self._parent = parent
        self._attachments: Dict[str, Dict] = attachments if attachments is not None else {}
        self._last_response_tokens = {"input": 0, "output": 0, "input_total": 0, "output_total": 0}

    def version(self) -> str:
        """Get the transcript version."""
        return self._version

    def parent(self) -> AIConversationParent | None:
        """Get the parent delegation reference."""
        return self._parent

    def add_attachment(self, content: str, filename: str, attachment_type: str) -> str:
        """
        Store an attachment and return its GUID.

        The content is frozen at call time. The returned GUID can be stored
        on an AIMessage to reference this attachment.

        Args:
            content: Full text content of the attachment
            filename: Original filename for display and language detection
            attachment_type: Type identifier e.g. "file" or "blueprint"

        Returns:
            GUID string identifying this attachment
        """
        guid = str(uuid.uuid4())
        self._attachments[guid] = {
            "filename": filename,
            "type": attachment_type,
            "content": content
        }
        return guid

    def get_attachment(self, guid: str) -> Dict | None:
        """
        Retrieve an attachment by GUID.

        Args:
            guid: GUID returned by add_attachment

        Returns:
            Dict with filename, type, and content keys, or None if not found
        """
        return self._attachments.get(guid)

    def attachments(self) -> Dict[str, Dict]:
        """
        Return a copy of the full attachments store.

        Returns:
            Dict mapping GUID to attachment data
        """
        return dict(self._attachments)

    def set_parent(self, parent: AIConversationParent) -> None:
        """
        Set the parent delegation reference.

        Args:
            parent: Parent delegation reference to set
        """
        self._parent = parent

    def clear(self) -> None:
        """Clear the conversation history."""
        self._messages.clear()
        self._attachments.clear()
        self._last_response_tokens = {"input": 0, "output": 0, "input_total": 0, "output_total": 0}

    def restore_attachments(self, attachments: Dict[str, Dict]) -> None:
        """
        Replace the attachment store with the given dict.

        Used when rebuilding history after a truncation, where only a subset of
        the original attachments should be retained.

        Args:
            attachments: Dict mapping GUID to attachment data (filename, type, content)
        """
        self._attachments.clear()
        self._attachments.update(attachments)

    def add_message(self, message: AIMessage) -> None:
        """Add a message to the history."""
        self._messages.append(message)

        # Update token counts if usage is provided
        if message.usage:
            self._last_response_tokens["input"] = message.usage.prompt_tokens
            self._last_response_tokens["output"] = message.usage.completion_tokens
            self._last_response_tokens["input_total"] += message.usage.prompt_tokens
            self._last_response_tokens["output_total"] += message.usage.completion_tokens

    def update_message(
        self,
        message_id: str,
        content: str,
        usage: AIUsage | None = None,
        completed: bool | None = None,
        signature: str | None = None,
        redacted_reasoning: str | None = None
    ) -> AIMessage | None:
        """Update an existing message and return the updated message."""
        for message in self._messages:
            if message.id == message_id:
                message.content = content
                if usage is not None:
                    old_usage = message.usage
                    message.usage = usage

                    # Only update token counts if we didn't have usage before
                    if old_usage is None:
                        self._last_response_tokens["input"] = usage.prompt_tokens
                        self._last_response_tokens["output"] = usage.completion_tokens
                        self._last_response_tokens["input_total"] += usage.prompt_tokens
                        self._last_response_tokens["output_total"] += usage.completion_tokens

                if completed is not None:
                    message.completed = completed

                message.signature = signature
                message.redacted_reasoning = redacted_reasoning

                return message

        return None

    def remove_last_message(self) -> AIMessage | None:
        """
        Remove and return the last message from the history.

        Returns:
            The removed message, or None if the history is empty.
        """
        if not self._messages:
            return None

        return self._messages.pop()

    def get_messages(self) -> List[AIMessage]:
        """
        Get a copy of all messages in the conversation history.

        Returns:
            List[AIMessage]: Copy of all messages
        """
        return self._messages.copy()

    def get_token_counts(self) -> Dict[str, int]:
        """Get token counts from last response."""
        return self._last_response_tokens

    def get_attachment_content_for_request(self, guid: str) -> Tuple[str, str] | None:
        """
        Resolve an attachment GUID to (filename, content) for API request construction.

        Args:
            guid: GUID of the attachment to resolve

        Returns:
            Tuple of (filename, content) or None if not found
        """
        attachment = self._attachments.get(guid)
        if attachment is None:
            return None

        return attachment["filename"], attachment["content"]
