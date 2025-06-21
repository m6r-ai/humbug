"""Ollama backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage, AIMessageSource
from humbug.ai.ai_tool_manager import AIToolCall, AIToolResult, AIToolDefinition
from humbug.ai.ollama.ollama_stream_response import OllamaStreamResponse


class OllamaBackend(AIBackend):
    """Ollama API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "http://localhost:11434/api/chat"

    def __init__(self, api_key: str, api_url: str | None = None) -> None:
        """Initialize the Ollama backend.

        Args:
            api_key: API key for authentication (not used in this case)
            api_url: Custom API base URL (optional)
        """
        super().__init__(api_key, api_url)

        # Llama doesn't use normal SSE encoding!
        self._uses_data = False

    def _format_tool_definition(self, tool_def: AIToolDefinition) -> Dict[str, Any]:
        """
        Convert tool definition to Ollama format.

        Args:
            tool_def: Generic tool definition

        Returns:
            Tool definition in Ollama format
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
        Build user message(s) for Ollama format.

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
                    "content": tool_result.content
                })

        return messages

    def _build_assistant_message(self, content: str, tool_calls: List[AIToolCall] | None = None) -> Dict[str, Any]:
        """
        Build assistant message for Ollama format.

        Args:
            content: Assistant message content
            tool_calls: Optional tool calls made by the assistant

        Returns:
            Assistant message dictionary
        """
        message: Dict[str, Any] = {
            "role": "assistant",
            "content": content
        }

        if tool_calls:
            message["tool_calls"] = [
                {
                    "function": {
                        "name": call.name,
                        "arguments": call.arguments
                    }
                }
                for call in tool_calls
            ]

        return message

    def _build_message(self, content: str, role: str) -> Dict[str, Any]:
        """
        Build message for Ollama format.

        Args:
            content: Message content
            role: Message role ("user" or "assistant")

        Returns:
            Message dictionary formatted for Ollama API
        """
        return {
            "role": role,
            "content": content
        }

    def _format_messages_for_provider(self, conversation_history: List[AIMessage]) -> List[Dict[str, Any]]:
        """
        Format conversation history for Ollama's API format in a single pass.

        Args:
            conversation_history: List of AIMessage objects

        Returns:
            List of messages formatted for Ollama API
        """
        result = []

        for message in conversation_history:
            if message.source == AIMessageSource.USER:
                user_messages = self._build_user_message(
                    content=message.content,
                    tool_results=message.tool_results
                )
                result.extend(user_messages)
                continue

            # Handle AI assistant messages
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

        return result

    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Ollama."""
        # Use the unified message formatting
        messages = self._format_messages_for_provider(conversation_history)

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True,
            "options": {
                "temperature": settings.temperature if settings.temperature else 0.7,
                "server_sent_events": True
            }
        }

        # Add tools if supported
        if self._supports_tools(settings) and self._tool_manager.has_tools():
            tool_definitions = self._tool_manager.get_tool_definitions()
            if tool_definitions:
                data["tools"] = [self._format_tool_definition(tool_def) for tool_def in tool_definitions]
                self._logger.debug("Added %d tool definitions for ollama", len(tool_definitions))

        self._logger.debug("stream message %r", data)

        # Build headers
        headers = {
            "Content-Type": "application/json"
        }

        return RequestConfig(
            url=self._api_url,
            headers=headers,
            data=data
        )

    def _create_stream_response_handler(self) -> OllamaStreamResponse:
        """Create an Ollama-specific stream response handler."""
        return OllamaStreamResponse()
