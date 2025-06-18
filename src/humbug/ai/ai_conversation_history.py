"""AI conversation state management."""

import json
from typing import Dict, List

from humbug.ai.ai_message import AIMessage
from humbug.ai.ai_message_source import AIMessageSource
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
        completed: bool | None = None
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

    def get_messages_for_context(self) -> List[Dict[str, str]]:
        """
        Get messages formatted for AI context.

        Returns:
            Sets of user messages with completed AI responses, including
            any tool interactions that occurred during the AI's processing.
        """
        result = []
        i = 0

        print("==============================================")
        print(f"self._messages: {self._messages}")
        print("==============================================")
        # We must see at least 2 messages so there's no point starting with the last one.
        while i < (len(self._messages) - 1):
            # Look for user message
            user_msg = self._messages[i]
            i += 1

            if user_msg.source != AIMessageSource.USER:
                continue

            # Collect all messages until we find a completed AI response or reach the end
            message_sequence = []

            while i < len(self._messages):
                current_msg = self._messages[i]

                if current_msg.source != AIMessageSource.AI:
                    print(f"Skipping non-AI message: {current_msg.source}")
                    message_sequence.append(current_msg)
                    i += 1
                    continue

                # Found AI response - check if it's completed and error-free
                if not current_msg.completed or current_msg.error:
                    print(f"Skipping incomplete or errored AI message: {current_msg.id}")
                    break

                # Add user message
                result.append({
                    "role": "user",
                    "content": user_msg.content
                })

                # Add any tool calls/results that happened during this exchange
                for seq_msg in message_sequence:
                    print(f"Processing message: {seq_msg.id} from {seq_msg.source}")
                    if seq_msg.source == AIMessageSource.TOOL_CALL and seq_msg.tool_calls:
                        # Add tool calls to context (format depends on provider)
                        result.append({
                            "role": "assistant",
                            "content": "",
                            "tool_calls": [
                                {
                                    "id": call.id,
                                    "type": "function",
                                    "function": {
                                        "name": call.name,
                                        "arguments": json.dumps(call.arguments)
                                    }
                                }
                                for call in seq_msg.tool_calls
                            ]
                        })

                    elif seq_msg.source == AIMessageSource.TOOL_RESULT and seq_msg.tool_results:
                        # Add tool results
                        for tool_result in seq_msg.tool_results:
                            result.append({
                                "role": "tool",
                                "content": tool_result.content,
                                "tool_call_id": tool_result.tool_call_id
                            })

                # Add final AI response
                result.append({
                    "role": "assistant",
                    "content": current_msg.content
                })

                i += 1
                break

        # Add the final user message if we're at the end
        if self._messages and self._messages[-1].source == AIMessageSource.USER:
            result.append({
                "role": "user",
                "content": self._messages[-1].content
            })

        return result

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
