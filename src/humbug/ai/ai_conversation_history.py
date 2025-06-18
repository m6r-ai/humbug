"""AI conversation state management."""

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
            Sets of user and assistant messages, excluding hidden tool audit messages.
            Tool calls are included with the AI message that made them.
            Tool results are included with the user message that contains them.
        """
        result = []

        for message in self._messages:
            # Skip hidden audit trail messages (TOOL_CALL and TOOL_RESULT)
            if message.source in (AIMessageSource.TOOL_CALL, AIMessageSource.TOOL_RESULT):
                continue

            # Handle user messages
            if message.source == AIMessageSource.USER:
                msg_dict = {
                    "role": "user",
                    "content": message.content
                }

                # Add tool results if this user message contains them
                if message.tool_results:
                    # For Anthropic, tool results are structured content
                    tool_result_content = []
                    if message.content:
                        tool_result_content.append({"type": "text", "text": message.content})

                    for tool_result in message.tool_results:
                        tool_result_content.append({
                            "type": "tool_result",
                            "tool_use_id": tool_result.tool_call_id,
                            "content": tool_result.content
                        })

                    msg_dict["content"] = tool_result_content

                result.append(msg_dict)

            # Handle AI messages
            elif message.source == AIMessageSource.AI:
                # Only include completed AI messages in context
                if not message.completed or message.error:
                    continue

                msg_dict = {
                    "role": "assistant",
                    "content": message.content
                }

                # Add tool calls if this AI message made them
                if message.tool_calls:
                    # For Anthropic, tool calls are structured content
                    tool_call_content = []
                    if message.content:
                        tool_call_content.append({"type": "text", "text": message.content})

                    for tool_call in message.tool_calls:
                        tool_call_content.append({
                            "type": "tool_use",
                            "id": tool_call.id,
                            "name": tool_call.name,
                            "input": tool_call.arguments
                        })

                    msg_dict["content"] = tool_call_content

                result.append(msg_dict)

            # Skip reasoning messages for now (they're not part of the conversation context)
            # Skip system messages (they're handled separately)

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
