"""Anthropic backend implementation."""
import json
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
from humbug.ai.ai_message import AIMessage, AIMessageSource
from humbug.ai.anthropic.anthropic_stream_response import AnthropicStreamResponse


class AnthropicBackend(AIBackend):
    """Anthropic API backend implementation."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.anthropic.com/v1/messages"

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

            # Handle user messages with tool results
            if role == "user" and "tool_results" in message:
                # For Anthropic, tool results are structured content
                tool_result_content = []
                if content:
                    tool_result_content.append({"type": "text", "text": content})

                for tool_result in message["tool_results"]:
                    tool_result_content.append({
                        "type": "tool_result",
                        "tool_use_id": tool_result["tool_call_id"],
                        "content": tool_result["content"]
                    })

                result.append({
                    "role": role,
                    "content": tool_result_content
                })

            # Handle assistant messages with tool calls
            elif role == "assistant" and "tool_calls" in message:
                # For Anthropic, tool calls are structured content
                tool_call_content = []
                if content:
                    tool_call_content.append({"type": "text", "text": content})

                tool_call: Dict[str, Any]
                for tool_call in message["tool_calls"]:
                    try:
                        json_args = json.loads(tool_call["arguments"])

                    except json.JSONDecodeError as e:
                        self._logger.warning("Failed to parse tool arguments: %s", e)
                        raise

                    tool_call_content.append({
                        "type": "tool_use",
                        "id": tool_call["id"],
                        "name": tool_call["name"],
                        "input": json_args
                    })

                result.append({
                    "role": role,
                    "content": tool_call_content
                })

            # Handle regular messages
            else:
                result.append({
                    "role": role,
                    "content": content
                })

        return result

    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Anthropic."""
        # Use the pre-formatted messages directly
        messages = self._format_messages_for_provider(conversation_history)

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "max_tokens": settings.max_output_tokens,
            "stream": True
        }

        # Add thinking configuration if VISIBLE_REASONING is enabled.  Set budget at 90% of the maximum token count.
        thinking = False
        if (settings.reasoning & ReasoningCapability.VISIBLE_REASONING) == ReasoningCapability.VISIBLE_REASONING:
            thinking = True
            data["thinking"] = {
                "type": "enabled",
                "budget_tokens": int(settings.max_output_tokens * 0.9)
            }

        # Only include temperature if supported by model
        if not thinking and AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        # Add tools if supported
        if self._supports_tools(settings) and self._tool_manager.has_tools():
            tool_definitions = self._tool_manager.get_tool_definitions_for_provider("anthropic")
            if tool_definitions:
                data["tools"] = tool_definitions
                self._logger.debug("Added %d tool definitions for anthropic", len(tool_definitions))

        # Build headers
        headers = {
            "content-type": "application/json",
            "x-api-key": self._api_key,
            "anthropic-version": "2023-06-01"
        }

        return RequestConfig(
            url=self._api_url,
            headers=headers,
            data=data
        )

    def _create_stream_response_handler(self) -> AnthropicStreamResponse:
        """Create an Anthropic-specific stream response handler."""
        return AnthropicStreamResponse()
