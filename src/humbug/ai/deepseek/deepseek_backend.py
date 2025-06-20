"""Deepseek backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage, AIMessageSource
from humbug.ai.deepseek.deepseek_stream_response import DeepseekStreamResponse


class DeepseekBackend(AIBackend):
    """Deepseek API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.deepseek.com/chat/completions"

    def _get_messages_for_context(self, messages: List[AIMessage]) -> List[Dict[str, Any]]:
        """
        Get messages formatted for AI context - raw format for backend processing.

        Returns:
            Raw message format that backends can transform as needed.
            Tool calls and results are preserved as separate fields for backend processing.
        """
        result = []
        last_reasoning_message: AIMessage | None = None

        for message in messages:
            # Skip hidden audit trail messages (TOOL_CALL and TOOL_RESULT)
            if message.source == AIMessageSource.TOOL_CALL:
                if last_reasoning_message is not None:
                    # If we had a reasoning message, we can include it
                    msg_dict = {
                        "role": "assistant",
                        "reasoning_content": message.content
                    }

                    if last_reasoning_message.tool_calls:
                        msg_dict["tool_calls"] = [
                            {
                                "id": call.id,
                                "name": call.name,
                                "arguments": call.arguments
                            }
                            for call in last_reasoning_message.tool_calls
                        ]

                    result.append(msg_dict)
                    last_reasoning_message = None

                continue

            # We're only interested in the last reasoning message for tool calls
            last_reasoning_message = None

            if message.source == AIMessageSource.TOOL_RESULT:
                continue

            # Handle user messages
            if message.source == AIMessageSource.USER:
                msg_dict: Dict[str, Any] = {
                    "role": "user",
                    "content": message.content
                }

                # Add tool results if this user message contains them (raw format)
                if message.tool_results:
                    msg_dict["tool_results"] = [
                        {
                            "tool_call_id": result.tool_call_id,
                            "name": result.name,
                            "content": result.content,
                            "error": result.error
                        }
                        for result in message.tool_results
                    ]

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

                # Add tool calls if this AI message made them (raw format)
                if message.tool_calls:
                    msg_dict["tool_calls"] = [
                        {
                            "id": call.id,
                            "name": call.name,
                            "arguments": call.arguments
                        }
                        for call in message.tool_calls
                    ]

                result.append(msg_dict)

            elif message.source == AIMessageSource.REASONING:
                last_reasoning_message = message

            # Skip system messages (they're handled separately)

        return result

    def _format_messages_for_provider(self, conversation_history: List[AIMessage]) -> List[Dict[str, Any]]:
        """Format conversation history for xAI's API format."""
        messages = self._get_messages_for_context(conversation_history)

        result = []
        for message in messages:
            role = message["role"]
            content = message["content"]

            msg_dict = {
                "role": role,
                "content": content
            }

            # Handle assistant messages with tool calls
            if role == "assistant" and "tool_calls" in message:
                msg_dict["tool_calls"] = [
                    {
                        "id": tool_call["id"],
                        "type": "function",
                        "function": {
                            "name": tool_call["name"],
                            "arguments": tool_call["arguments"]
                        }
                    }
                    for tool_call in message["tool_calls"]
                ]

            # Handle user messages with tool results
            elif role == "user" and "tool_results" in message:
                if content:
                    result.append(msg_dict)

                for tool_result in message["tool_results"]:
                    result.append({
                        "role": "tool",
                        "name": tool_result["name"],
                        "tool_call_id": tool_result["tool_call_id"],
                        "content": tool_result["content"]
                    })

                continue

            result.append(msg_dict)

        return result

    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Deepseek."""
        # Use the pre-formatted messages directly
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
            tool_definitions = self._tool_manager.get_tool_definitions_for_provider("deepseek")
            if tool_definitions:
                data["tools"] = tool_definitions
                data["tool_choice"] = "auto"
                self._logger.debug("Added %d tool definitions for deepseek", len(tool_definitions))

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

    def _create_stream_response_handler(self) -> DeepseekStreamResponse:
        """Create an Deepseek-specific stream response handler."""
        return DeepseekStreamResponse()
