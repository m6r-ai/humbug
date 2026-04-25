"""AI conversation state management."""

from dataclasses import dataclass
from typing import Dict, List

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
        parent: AIConversationParent | None = None
    ) -> None:
        """Initialize conversation history with optional metadata."""
        self._messages: List[AIMessage] = messages if messages is not None else []
        self._version = version
        self._parent = parent
        self._last_response_tokens = {"input": 0, "output": 0, "input_total": 0, "output_total": 0}

    def version(self) -> str:
        """Get the transcript version."""
        return self._version

    def parent(self) -> AIConversationParent | None:
        """Get the parent delegation reference."""
        return self._parent

    def set_parent(self, parent: AIConversationParent) -> None:
        """Set the parent delegation reference.

        Args:
            parent: Parent delegation reference to set
        """
        self._parent = parent

    def clear(self) -> None:
        """Clear the conversation history."""
        self._messages.clear()
        self._last_response_tokens = {"input": 0, "output": 0, "input_total": 0, "output_total": 0}

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
