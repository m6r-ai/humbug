"""xAI backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage, AIMessageSource
from humbug.ai.ai_tool_manager import AIToolCall, AIToolResult
from humbug.ai.xai.xai_stream_response import XAIStreamResponse


class XAIBackend(AIBackend):
    """xAI API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.x.ai/v1/chat/completions"

    def _build_user_message(self, content: str, tool_results: List[AIToolResult] | None = None) -> List[Dict[str, Any]]:
        """
        Build user message(s) for xAI format.

        Args:
            content: User message content
            tool_results: Optional tool results to include

        Returns:
            List of message dictionaries (may include separate tool result messages)
        """
        messages = []

        # Add user message if there's content
        if content:
            messages.append({
                "role": "user",
                "content": content
            })

        # Add separate tool result messages
        if tool_results:
            for tool_result in tool_results:
                messages.append({
                    "role": "tool",
                    "name": tool_result.name,
                    "tool_call_id": tool_result.tool_call_id,
                    "content": tool_result.content
                })

        return messages

    def _build_assistant_message(
        self,
        content: str,
        tool_calls: List[AIToolCall] | None = None,
        reasoning_content: str = ""
    ) -> Dict[str, Any]:
        """
        Build assistant message for xAI format.

        Args:
            content: Assistant message content
            tool_calls: Optional tool calls made by the assistant
            reasoning_content: Optional reasoning content for xAI

        Returns:
            Assistant message dictionary
        """
        message: Dict[str, Any] = {
            "role": "assistant",
            "content": content
        }

        # Add reasoning content if present
        if reasoning_content:
            message["reasoning_content"] = reasoning_content

        if tool_calls:
            message["tool_calls"] = [
                {
                    "id": call.id,
                    "type": "function",
                    "function": {
                        "name": call.name,
                        "arguments": call.arguments
                    }
                }
                for call in tool_calls
            ]

        return message

    def _format_messages_for_provider(self, conversation_history: List[AIMessage]) -> List[Dict[str, Any]]:
        """
        Format conversation history for xAI's API format in a single pass.

        Args:
            conversation_history: List of AIMessage objects

        Returns:
            List of messages formatted for xAI API
        """
        result = []
        last_reasoning_message: AIMessage | None = None

        for message in conversation_history:
            if message.source == AIMessageSource.TOOL_CALL:
                # If we had a reasoning message, combine it with the tool call
                if last_reasoning_message is None:
                    continue

                # Create assistant message with reasoning content and tool calls
                assistant_msg = self._build_assistant_message(
                    content=message.content,
                    tool_calls=last_reasoning_message.tool_calls,
                    reasoning_content=last_reasoning_message.content
                )
                result.append(assistant_msg)
                last_reasoning_message = None
                continue

            # Clear any pending reasoning message when we encounter other message types
            last_reasoning_message = None

            if message.source == AIMessageSource.USER:
                user_messages = self._build_user_message(
                    content=message.content,
                    tool_results=message.tool_results
                )
                result.extend(user_messages)
                continue

            if message.source == AIMessageSource.AI:
                # Only include completed AI messages without errors
                if not message.completed or message.error:
                    continue

                assistant_msg = self._build_assistant_message(
                    content=message.content,
                    tool_calls=message.tool_calls
                )
                result.append(assistant_msg)
                continue

            if message.source == AIMessageSource.REASONING:
                last_reasoning_message = message
                continue

        return result

    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for xAI."""
        messages = self._format_messages_for_provider(conversation_history)

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True,
            "stream_options": {"include_usage": True}
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        # Add tools if supported
        if self._supports_tools(settings) and self._tool_manager.has_tools():
            tool_definitions = self._tool_manager.get_tool_definitions_for_provider("xai")
            if tool_definitions:
                data["tools"] = tool_definitions
                data["tool_choice"] = "auto"
                self._logger.debug("Added %d tool definitions for xai", len(tool_definitions))

        self._logger.debug("stream message %r", data)

        # Build headers
        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }

        return RequestConfig(
            url=self._api_url,
            headers=headers,
            data=data
        )

    def _create_stream_response_handler(self) -> XAIStreamResponse:
        """Create an xAI-specific stream response handler."""
        return XAIStreamResponse()
