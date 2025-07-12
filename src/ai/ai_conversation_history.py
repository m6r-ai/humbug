"""AI conversation state management."""

from typing import Dict, List

from ai.ai_message import AIMessage
from ai.ai_usage import AIUsage


class AIConversationHistory:
    """Manages the conversation history and state."""

    def __init__(self) -> None:
        """Initialize empty conversation history."""
        self._messages: List[AIMessage] = []
        self._last_response_tokens = {"input": 0, "output": 0, "input_total": 0, "output_total": 0}

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
        readacted_reasoning: str | None = None
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
                message.readacted_reasoning = readacted_reasoning

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
