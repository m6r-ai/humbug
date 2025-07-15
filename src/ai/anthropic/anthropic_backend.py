"""Anthropic backend implementation."""
from typing import Dict, List, Any

from ai.ai_backend import AIBackend, RequestConfig
from ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
from ai.ai_message import AIMessage, AIMessageSource
from ai.anthropic.anthropic_stream_response import AnthropicStreamResponse
from ai_tool import AIToolCall, AIToolResult, AIToolDefinition


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
        content_parts = []
        if content:
            content_parts.append({
                "type": "text",
                "text": content
            })

        if tool_results:
            # Add tool results as structured content
            for tool_result in tool_results:
                tool_content = tool_result.content if not tool_result.error else f"Error: {tool_result.error}"

                result: Dict[str, Any] = {
                    "type": "tool_result",
                    "tool_use_id": tool_result.id,
                    "content": tool_content
                }

                if tool_result.error:
                    result["is_error"] = True

                content_parts.append(result)

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
        current_turn_message_index = -1
        last_reasoning_message: AIMessage | None = None

        for message in conversation_history:
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

                # If we have an AI message that has no tool calls then we've finished this turn
                if not message.tool_calls:
                    current_turn_message_index = -1
                    last_user_message_index = -1

                continue

            # Clear any pending reasoning message when we encounter other message types
            last_reasoning_message = None

            if message.source == AIMessageSource.USER:
                # If we have tool results this was an auto-generated user message - it doesn't count as a turn
                if not message.tool_results:
                    # If we never finished the last turn then we need to remove it
                    if current_turn_message_index >= 0:
                        self._logger.debug("Removing unfinished turn at index %d", current_turn_message_index)
                        result = result[:current_turn_message_index]

                    current_turn_message_index = len(result)

                user_msg = self._build_user_message(
                    content=message.content,
                    tool_results=message.tool_results
                )
                result.append(user_msg)

                last_user_message_index = len(result) - 1
                continue

            if message.source == AIMessageSource.REASONING:
                if not message.completed or message.error:
                    continue

                last_reasoning_message = message
                continue

        # Remove anything after the last user message - we'll start with that last one
        assert last_user_message_index >= 0, "Last user message index should be valid"
        result = result[:last_user_message_index+1]

        # Find the last user message and ensure it has cache control set
        last_user_message = result[-1]
        last_user_content = last_user_message["content"][0]
        last_user_content["cache_control"] = {"type": "ephemeral"}

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
