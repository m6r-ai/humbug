"""Anthropic backend implementation."""
import json
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
from humbug.ai.ai_message import AIMessage, AIMessageSource
from humbug.ai.ai_tool_manager import AIToolCall, AIToolResult
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

    def _build_user_message(self, content: str, tool_results: List[AIToolResult] | None = None) -> Dict[str, Any]:
        """
        Build user message for Anthropic format.

        Args:
            content: User message content
            tool_results: Optional tool results to include

        Returns:
            User message dictionary with structured content
        """
        # For Anthropic, tool results are structured content within the user message
        if tool_results:
            content_parts = []

            # Add text content if present
            if content:
                content_parts.append({"type": "text", "text": content})

            # Add tool results as structured content
            for tool_result in tool_results:
                content_parts.append({
                    "type": "tool_result",
                    "tool_use_id": tool_result.tool_call_id,
                    "content": tool_result.content
                })

            return {
                "role": "user",
                "content": content_parts
            }

        # Simple text message
        return {
            "role": "user",
            "content": content
        }

    def _build_assistant_message(self, content: str, tool_calls: List[AIToolCall] | None = None) -> Dict[str, Any]:
        """
        Build assistant message for Anthropic format.

        Args:
            content: Assistant message content
            tool_calls: Optional tool calls made by the assistant

        Returns:
            Assistant message dictionary with structured content
        """
        # For Anthropic, tool calls are structured content within the assistant message
        if tool_calls:
            content_parts = []

            # Add text content if present
            if content:
                content_parts.append({"type": "text", "text": content})

            # Add tool calls as structured content
            for tool_call in tool_calls:
                try:
                    json_args = json.loads(tool_call.arguments)
                except json.JSONDecodeError as e:
                    self._logger.warning("Failed to parse tool arguments: %s", e)
                    raise

                content_parts.append({
                    "type": "tool_use",
                    "id": tool_call.id,
                    "name": tool_call.name,
                    "input": json_args
                })

            return {
                "role": "assistant",
                "content": content_parts
            }

        # Simple text message
        return {
            "role": "assistant",
            "content": content
        }

    def _format_messages_for_provider(self, conversation_history: List[AIMessage]) -> List[Dict[str, Any]]:
        """
        Format conversation history for Anthropic's API format in a single pass.

        Args:
            conversation_history: List of AIMessage objects

        Returns:
            List of messages formatted for Anthropic API
        """
        result = []
        last_reasoning_message: AIMessage | None = None

        for message in conversation_history:
            if message.source == AIMessageSource.TOOL_CALL:
                # If we don't have a pending reasoning message, we don't need to do anything
                if last_reasoning_message is None:
                    continue

                # Create assistant message with reasoning content and tool calls
                # TODO: reasoning messages should be handled differently!
                assistant_msg = self._build_assistant_message(
                    content=last_reasoning_message.content,
                    tool_calls=last_reasoning_message.tool_calls
                )
                result.append(assistant_msg)
                last_reasoning_message = None
                continue

            # Clear any pending reasoning message when we encounter other message types
            last_reasoning_message = None

            if message.source == AIMessageSource.USER:
                user_msg = self._build_user_message(
                    content=message.content,
                    tool_results=message.tool_results
                )
                result.append(user_msg)
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
        """Build complete request configuration for Anthropic."""
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
