"""AI conversation state management."""

from typing import Dict, List

from humbug.ai.ai_message import AIMessage
from humbug.ai.ai_usage import AIUsage


class AIConversationHistory:
    """Manages the conversation history and state."""

    def __init__(self) -> None:
        """Initialize empty conversation history."""
        self._messages: List[AIMessage] = []
        self._last_response_tokens = {"input": 0, "output": 0}

    def clear(self) -> None:
        """Clear the conversation history."""
        self._messages.clear()
        self._last_response_tokens = {"input": 0, "output": 0}

    def add_message(self, message: AIMessage) -> None:
        """Add a message to the history."""
        self._messages.append(message)

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

    def get_visible_messages(self) -> List[AIMessage]:
        """Get messages that should be visible to the user (excludes tool calls/results)."""
        return [msg for msg in self._messages if not msg.is_hidden_from_user()]

    def get_token_counts(self) -> Dict[str, int]:
        """Get token counts from last response."""
        return self._last_response_tokens

    def update_last_tokens(self, input_tokens: int, output_tokens: int) -> None:
        """
        Update token counts for the last response.

        Args:
            input_tokens: Number of input tokens
            output_tokens: Number of output tokens
        """
        self._last_response_tokens = {
            "input": input_tokens,
            "output": output_tokens
        }
