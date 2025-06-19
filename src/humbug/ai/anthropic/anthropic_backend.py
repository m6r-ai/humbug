"""Anthropic backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
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

    def _format_messages_for_provider(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Format conversation history for Anthropic's structured content format."""
        result = []

        for message in conversation_history:
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
                    tool_call_content.append({
                        "type": "tool_use",
                        "id": tool_call["id"],
                        "name": tool_call["name"],
                        "input": tool_call["arguments"]
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
        conversation_history: List[Dict[str, Any]],
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
