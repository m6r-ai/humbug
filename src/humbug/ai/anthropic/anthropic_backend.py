"""Anthropic backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
from humbug.ai.ai_message import AIMessage, AIMessageSource
from humbug.ai.ai_tool_manager import AIToolCall, AIToolResult, AIToolDefinition
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

    def _format_tool_definition(self, tool_def: AIToolDefinition) -> Dict[str, Any]:
        """
        Convert tool definition to Anthropic format.

        Args:
            tool_def: Generic tool definition

        Returns:
            Tool definition in Anthropic format
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

            if param.required:
                required.append(param.name)

        return {
            "name": tool_def.name,
            "description": tool_def.description,
            "input_schema": {
                "type": "object",
                "properties": properties,
                "required": required
            }
        }

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
        if not tool_results:
            return {
                "role": "user",
                "content": content
            }

        # Process tool results
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

    def _build_assistant_message(
            self,
            reasoning: str,
            signature: str,
            content: str,
            tool_calls: List[AIToolCall] | None = None
        ) -> Dict[str, Any]:
        """
        Build assistant message for Anthropic format.

        Args:
            reasoning: Reasoning content if applicable
            signature: Signature of the assistant on any reasoning content
            content: Assistant message content
            tool_calls: Optional tool calls made by the assistant

        Returns:
            Assistant message dictionary with structured content
        """
        # For Anthropic, tool calls are structured content within the assistant message
        if tool_calls:
            content_parts: List[Dict[str, Any]] = []

            # Add reasoning content if present
            if reasoning:
                content_parts.append({"type": "thinking", "thinking": reasoning, "signature": signature})

            # Add text content if present
            if content:
                content_parts.append({"type": "text", "text": content})

            # Add tool calls as structured content
            for tool_call in tool_calls:
                content_parts.append({
                    "type": "tool_use",
                    "id": tool_call.id,
                    "name": tool_call.name,
                    "input": tool_call.arguments,
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
        result: List[Dict[str, Any]] = []
        last_user_message_index = -1
        last_reasoning_message: AIMessage | None = None

        for message in conversation_history:
            # Check for problematic messages that should trigger cleanup
            is_problematic = (
                message.source == AIMessageSource.SYSTEM or
                (message.source == AIMessageSource.AI and (not message.completed or message.error)) or
                (message.source == AIMessageSource.REASONING and (not message.completed or message.error))
            )

            if is_problematic and last_user_message_index >= 0:
                self._logger.debug("Removing user message and subsequent messages due to %s", message.source)
                result = result[:last_user_message_index]
                last_user_message_index = -1
                last_reasoning_message = None
                continue

            if message.source == AIMessageSource.TOOL_CALL:
                # If we don't have a pending reasoning message, we don't need to do anything
                if last_reasoning_message is None:
                    continue

                # Create assistant message with reasoning content and tool calls
                signature = ""
                if last_reasoning_message and last_reasoning_message.signature:
                    signature = last_reasoning_message.signature

                assistant_msg = self._build_assistant_message(
                    reasoning=last_reasoning_message.content,
                    signature=signature,
                    content="",
                    tool_calls=last_reasoning_message.tool_calls
                )
                result.append(assistant_msg)
                last_reasoning_message = None
                continue

            if message.source == AIMessageSource.AI:
                # Only include completed AI messages without errors
                if not message.completed or message.error:
                    last_reasoning_message = None
                    continue

                signature = ""
                if last_reasoning_message and last_reasoning_message.signature:
                    signature = last_reasoning_message.signature

                assistant_msg = self._build_assistant_message(
                    reasoning=last_reasoning_message.content if last_reasoning_message else "",
                    signature=signature,
                    content=message.content if message.content else "...",  # Never send empty content
                    tool_calls=message.tool_calls
                )
                result.append(assistant_msg)
                last_reasoning_message = None
                continue

            # Clear any pending reasoning message when we encounter other message types
            last_reasoning_message = None

            if message.source == AIMessageSource.USER:
                last_user_message_index = len(result)
                user_msg = self._build_user_message(
                    content=message.content,
                    tool_results=message.tool_results
                )
                result.append(user_msg)
                continue

            if message.source == AIMessageSource.REASONING:
                if not message.completed or message.error:
                    continue

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
        if self._supports_tools(settings):
            tool_definitions = self._tool_manager.get_tool_definitions()
            if tool_definitions:
                data["tools"] = [self._format_tool_definition(tool_def) for tool_def in tool_definitions]
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
