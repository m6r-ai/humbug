"""Mistral backend implementation."""
import json
import re
from typing import Dict, List, Any

from ai.ai_backend import AIBackend, RequestConfig
from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_message import AIMessage, AIMessageSource
from ai.mistral.mistral_stream_response import MistralStreamResponse
from ai_tool.ai_tool_manager import AIToolCall, AIToolResult, AIToolDefinition


class MistralBackend(AIBackend):
    """Mistral API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.mistral.ai/v1/chat/completions"

    def _format_tool_definition(self, tool_def: AIToolDefinition) -> Dict[str, Any]:
        """
        Convert tool definition to Mistral format.

        Args:
            tool_def: Generic tool definition

        Returns:
            Tool definition in Mistral format
        """
        properties: Dict[str, Any] = {}
        required = []

        for param in tool_def.parameters:
            properties[param.name] = {
                "type": param.type,
                "description": param.description
            }
            if param.enum:
                properties[param.name]["enum"] = param.enum

            if param.properties:
                properties[param.name]["properties"] = {
                    prop_name: {
                        "type": prop.type,
                        "description": prop.description
                    }
                    for prop_name, prop in param.properties.items()
                }

            if param.required:
                required.append(param.name)

        return {
            "type": "function",
            "function": {
                "name": tool_def.name,
                "description": tool_def.description,
                "parameters": {
                    "type": "object",
                    "properties": properties,
                    "required": required
                }
            }
        }

    def _build_user_message(self, content: str, tool_results: List[AIToolResult] | None = None) -> List[Dict[str, Any]]:
        """
        Build user message(s) for Mistral format.

        Args:
            content: User message content
            tool_results: Optional tool results to include

        Returns:
            List of message dictionaries (may include separate tool result messages)
        """
        messages = []

        # Add tool result messages
        if tool_results:
            for tool_result in tool_results:
                tool_content = tool_result.content if not tool_result.error else f"Error: {tool_result.error}"

                # Mistral can't handle some special characters in tool IDs, so we sanitize them.
                # This only occurs if we switch AI backends mid-conversation.
                messages.append({
                    "role": "tool",
                    "tool_call_id": re.sub(r"[^a-zA-Z0-9]", "", tool_result.id)[-9:],
                    "content": tool_content
                })

        # Add user message if there's content
        if content:
            messages.append({
                "role": "user",
                "content": content
            })

        return messages

    def _build_assistant_message(self, content: str, tool_calls: List[AIToolCall] | None = None) -> Dict[str, Any]:
        """
        Build assistant message for Mistral format.

        Args:
            content: Assistant message content
            tool_calls: Optional tool calls made by the assistant

        Returns:
            Assistant message dictionary
        """
        message: Dict[str, Any] = {
            "role": "assistant",
            "content": content if content else "...",  # Never send empty content
        }

        if tool_calls:
            # Mistral can't handle some special characters in tool IDs, so we sanitize them.
            # This only occurs if we switch AI backends mid-conversation.
            message["tool_calls"] = [
                {
                    "id": re.sub(r"[^a-zA-Z0-9]", "", call.id)[-9:],
                    "type": "function",
                    "function": {
                        "name": call.name,
                        "arguments": json.dumps(call.arguments)
                    }
                }
                for call in tool_calls
            ]

        return message

    def _format_messages_for_provider(self, conversation_history: List[AIMessage]) -> List[Dict[str, Any]]:
        """
        Format conversation history for Mistral's API format in a single pass.

        Args:
            conversation_history: List of AIMessage objects

        Returns:
            List of messages formatted for Mistral API
        """
        result: List[Dict[str, Any]] = []
        last_user_message_index = -1
        current_turn_message_index = -1

        for message in conversation_history:
            if message.source == AIMessageSource.USER:
                # If we have tool results this was an auto-generated user message - it doesn't count as a turn
                if not message.tool_results:
                    # If we never finished the last turn then we need to remove it
                    if current_turn_message_index >= 0:
                        self._logger.debug("Removing unfinished turn at index %d", current_turn_message_index)
                        result = result[:current_turn_message_index]

                    current_turn_message_index = len(result)

                user_messages = self._build_user_message(
                    content=message.content,
                    tool_results=message.tool_results
                )
                result.extend(user_messages)

                last_user_message_index = len(result) - 1
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

                # If we have an AI message that has no tool calls then we've finished this turn
                if not message.tool_calls:
                    current_turn_message_index = -1
                    last_user_message_index = -1

                continue

        # Remove anything after the last user message - we'll start with that last one
        assert last_user_message_index >= 0, "Last user message index should be valid"
        self._logger.debug("Removing unfinished user message at index %d", last_user_message_index)
        result = result[:last_user_message_index+1]

        return result

    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Mistral."""
        # Use the unified message formatting
        messages = self._format_messages_for_provider(conversation_history)

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "max_tokens": settings.max_output_tokens,
            "stream": True
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        # Add tools if supported
        if self._supports_tools(settings):
            tool_definitions = self._tool_manager.get_tool_definitions()
            if tool_definitions:
                data["tools"] = [self._format_tool_definition(tool_def) for tool_def in tool_definitions]
                data["tool_choice"] = "auto"
                self._logger.debug("Added %d tool definitions for mistral", len(tool_definitions))

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

    def _create_stream_response_handler(self) -> MistralStreamResponse:
        """Create an Mistral-specific stream response handler."""
        return MistralStreamResponse()
