"""Ollama backend implementation."""
import json
import socket
from typing import Callable, Dict, List, Any
from urllib.parse import urlsplit, urlunsplit

import aiohttp

from ai.ai_backend import AIBackend, RequestConfig
from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_message import AIMessage, AIMessageSource
from ai.ai_model import AIReasoningEffort
from ai.ai_conversation_history import AIConversationHistory
from ai.ollama.ollama_stream_response import OllamaStreamResponse
from ai_tool import AIToolCall, AIToolResult, AIToolDefinition


class OllamaBackend(AIBackend):
    """Ollama API backend implementation with streaming support."""

    def _get_api_endpoint_url(self, endpoint: str) -> str:
        """
        Build an Ollama API endpoint URL from either a base URL or API URL.

        Users commonly enter either "http://localhost:11434" or a full endpoint
        such as "http://localhost:11434/api/chat".  Normalize both forms so
        fetch and pull do not depend on platform-specific shell commands.
        """
        split = urlsplit(self._api_url)
        path = split.path.rstrip("/")
        if "/api/" in path:
            base_path = path.split("/api/", 1)[0]

        elif path.endswith("/api"):
            base_path = path[:-4]

        else:
            base_path = path

        endpoint_path = f"{base_path}/api/{endpoint.lstrip('/')}"
        return urlunsplit((
            split.scheme,
            split.netloc,
            endpoint_path,
            "",
            ""
        ))

    async def fetch_models(self) -> List[str]:
        """Fetch installed model names from the Ollama API."""
        url = self._get_api_endpoint_url("tags")
        headers = {"Authorization": f"Bearer {self._api_key}"} if self._api_key else {}
        # Force IPv4 — on macOS 'localhost' resolves to ::1 (IPv6) but Ollama
        # listens only on 127.0.0.1, causing EINVAL with the default connector.
        connector = aiohttp.TCPConnector(family=socket.AF_INET)
        async with aiohttp.ClientSession(connector=connector) as session:
            async with session.get(url, headers=headers, ssl=False) as response:
                response.raise_for_status()
                data = await response.json()
                return [m["name"] for m in data.get("models", [])]

    async def pull_model(
        self, model_name: str, on_progress: Callable[[str], None]
    ) -> None:
        """
        Pull a model from the Ollama registry.

        Streams NDJSON progress lines and calls on_progress with each status
        string.  Raises ValueError if the model name is not found, or
        aiohttp errors for network/server problems.

        Args:
            model_name: Name of the model to pull (e.g. "llama3.2").
            on_progress: Callback invoked with each status string from Ollama.
        """
        url = self._get_api_endpoint_url("pull")
        data = json.dumps({"name": model_name, "stream": True}).encode()
        headers = {"Content-Type": "application/json"}
        if self._api_key:
            headers["Authorization"] = f"Bearer {self._api_key}"

        connector = aiohttp.TCPConnector(family=socket.AF_INET)
        async with aiohttp.ClientSession(connector=connector) as session:
            async with session.post(url, data=data, headers=headers, ssl=False) as response:
                response.raise_for_status()
                async for raw_line in response.content:
                    line = raw_line.decode().strip()
                    if not line:
                        continue

                    try:
                        obj = json.loads(line)

                    except json.JSONDecodeError:
                        continue

                    if "error" in obj:
                        raise ValueError(obj["error"])

                    if "status" in obj:
                        on_progress(obj["status"])

    def __init__(self, api_key: str, api_url: str | None) -> None:
        """
        Initialize the Ollama backend.

        Args:
            api_key: API key for authentication (not used in this case)
            api_url: Custom API base URL (optional)
        """
        super().__init__(api_key, api_url or "http://localhost:11434")

        # Llama doesn't use normal SSE encoding!
        self._uses_data = False

    def _reasoning_model_matches(self, message: AIMessage, settings: AIConversationSettings) -> bool:
        """
        Require an exact model key match for reasoning compatibility.

        Ollama hosts many structurally different models under one API, so provider-level
        matching is insufficient — a reasoning block from one Ollama model is not
        compatible with a different Ollama model.
        """
        return message.model == settings.model and message.provider == settings.provider

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
            if param.type == "array":
                properties[param.name] = {
                    "type": "array",
                    "description": param.description,
                    "items": {
                        "type": "string"  # Assuming array of strings for simplicity
                    }
                }

            else:
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

        # Add tool result messages
        if tool_results:
            for tool_result in tool_results:
                tool_content = tool_result.content if not tool_result.error else f"Error: {tool_result.error}"

                messages.append({
                    "role": "tool",
                    "tool_call_id": tool_result.id,
                    "content": tool_content
                })

        # Add user message if there's content
        if content:
            messages.append({
                "role": "user",
                "content": content
            })

        return messages

    def _build_assistant_message(
        self,
        content: str,
        tool_calls: List[AIToolCall] | None = None,
        reasoning_content: str = ""
    ) -> Dict[str, Any]:
        """
        Build assistant message for Ollama format.

        Args:
            content: Assistant message content
            tool_calls: Optional tool calls made by the assistant
            reasoning_content: Optional reasoning content

        Returns:
            Assistant message dictionary
        """
        message: Dict[str, Any] = {
            "role": "assistant",
            "content": content
        }

        # Add reasoning content if present
        if reasoning_content:
            message["thinking"] = reasoning_content

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

    def _format_messages_for_provider(
        self,
        conversation_history: AIConversationHistory,
        settings: AIConversationSettings
    ) -> List[Dict[str, Any]]:
        """
        Format conversation history for Ollama's API format in a single pass.

        Args:
            conversation_history: List of AIMessage objects
            settings: Current conversation settings

        Returns:
            List of messages formatted for Ollama API
        """
        result: List[Dict[str, Any]] = []
        last_user_message_index = -1
        current_turn_message_index = -1
        last_reasoning_message: AIMessage | None = None

        for message in conversation_history.get_messages():
            if message.source == AIMessageSource.TOOL_CALL:
                # If we don't have a pending reasoning message, we don't need to do anything
                if last_reasoning_message is None:
                    continue

                # Create assistant message with reasoning content and tool calls
                assistant_msg = self._build_assistant_message(
                    content="",
                    tool_calls=last_reasoning_message.tool_calls,
                    reasoning_content=last_reasoning_message.content
                )
                result.append(assistant_msg)
                last_reasoning_message = None
                continue

            # Handle AI assistant messages
            if message.source == AIMessageSource.AI:
                # Only include completed AI messages without errors
                if not message.completed or message.error:
                    last_reasoning_message = None
                    continue

                assistant_msg = self._build_assistant_message(
                    content=message.content if message.content else "...",  # Never send empty content,
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

                user_messages = self._build_user_message(
                    content=self._resolve_message_content(message, conversation_history),
                    tool_results=message.tool_results
                )
                result.extend(user_messages)

                last_user_message_index = len(result) - 1
                continue

            if message.source == AIMessageSource.REASONING:
                if not message.completed or message.error:
                    continue

                if not self._reasoning_model_matches(message, settings):
                    continue

                last_reasoning_message = message
                continue

        # Remove anything after the last user message - we'll start with that last one
        assert last_user_message_index >= 0, "Last user message index should be valid"
        self._logger.debug("Removing unfinished user message at index %d", last_user_message_index)
        result = result[:last_user_message_index+1]

        return result

    def _build_request_config(
        self,
        conversation_history: AIConversationHistory,
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Ollama."""
        # Use the unified message formatting
        messages = self._format_messages_for_provider(conversation_history, settings)

        # Prepend system message if configured
        if self._system_prompt:
            messages.insert(0, {
                "role": "system",
                "content": self._system_prompt
            })

        # Build request data
        data = {
            "model": settings.model,
            "messages": messages,
            "stream": True,
            "options": {
                "temperature": settings.temperature if settings.temperature else 0.7,
                "server_sent_events": True
            }
        }

        thinking: bool = settings.reasoning_effort not in (None, AIReasoningEffort.NONE)
        data["think"] = thinking

        # Add tools if supported
        if self._supports_tools(settings):
            tool_definitions = self._tool_manager.get_tool_definitions()
            if tool_definitions:
                data["tools"] = [self._format_tool_definition(tool_def) for tool_def in tool_definitions]
                self._logger.debug("Added %d tool definitions for ollama", len(tool_definitions))

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

    def _create_stream_response_handler(self) -> OllamaStreamResponse:
        """Create an Ollama-specific stream response handler."""
        return OllamaStreamResponse()
