"""AI conversation state management."""

from typing import Dict, List, Optional

from humbug.ai.ai_message import AIMessage
from humbug.ai.ai_message_source import AIMessageSource
from humbug.ai.ai_usage import AIUsage

class AIConversationHistory:
    """Manages the conversation history and state."""

    def __init__(self, conversation_id: str):
        """Initialize empty conversation history."""
        self._conversation_id = conversation_id
        self._messages: List[AIMessage] = []
        self._last_response_tokens = {"input": 0, "output": 0}

    def add_message(self, message: AIMessage) -> None:
        """Add a message to the history."""
        self._messages.append(message)

    def update_message(
        self,
        message_id: str,
        content: str,
        usage: Optional[AIUsage] = None,
        completed: bool = None
    ) -> Optional[AIMessage]:
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

                return message

        return None

    def get_messages(self) -> List[AIMessage]:
        """Get a copy of all messages in the conversation history.

        Returns:
            List[AIMessage]: Copy of all messages
        """
        return self._messages.copy()

    def get_messages_for_context(self) -> List[Dict[str, str]]:
        """
        Get messages formatted for AI context.

        We only return pairs of messages where we saw a user message and the matching completed
        AI response.

        Returns:
            List of message dictionaries with role and content.
        """
        result = []
        i = 0

        # We must see at least 2 messages so there's no point starting with the last one.
        while i < (len(self._messages) - 1):
            user_msg = self._messages[i]
            i += 1

            if user_msg.source != AIMessageSource.USER:
                continue

            # Found a user message, look for corresponding AI response
            ai_msg = self._messages[i]
            if ai_msg.source == AIMessageSource.REASONING:
                # We're always safe to pick up this message because we always have one left
                # after we complete this loop, so the worst that will happen is we'll pick
                # up that user message.
                i += 1
                ai_msg = self._messages[i]

            if ai_msg.source != AIMessageSource.AI:
                continue

            # If we didn't complete or there were errors then we skip this
            if not ai_msg.completed or ai_msg.error:
                continue

            result.append({
                "role": "user",
                "content": user_msg.content
            })
            result.append({
                "role": "assistant",
                "content": ai_msg.content
            })

            i += 1

        # The last message should be our user's message.
        user_msg = self._messages[-1]
        result.append({
            "role": "user",
            "content": user_msg.content
        })

        return result

    def get_token_counts(self) -> Dict[str, int]:
        """Get token counts from last response."""
        return self._last_response_tokens

    def update_last_tokens(self, input_tokens: int, output_tokens: int) -> None:
        """Update token counts for the last response.

        Args:
            input_tokens: Number of input tokens
            output_tokens: Number of output tokens
        """
        self._last_response_tokens = {
            "input": input_tokens,
            "output": output_tokens
        }
