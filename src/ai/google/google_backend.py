"""Google Google backend implementation."""
from typing import Dict, List, Any

from ai.ai_backend import AIBackend, RequestConfig
from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_message import AIMessage, AIMessageSource
from ai.google.google_stream_response import GoogleStreamResponse
from ai_tool import AIToolCall, AIToolResult, AIToolDefinition


class GoogleBackend(AIBackend):
    """Google Gemini API backend implementation."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://generativelanguage.googleapis.com/v1beta/models"

    def _format_tool_definition(self, tool_def: AIToolDefinition) -> Dict[str, Any]:
        """
        Convert tool definition to Google format.

        Args:
            tool_def: Generic tool definition

        Returns:
            Tool definition in Google format
        """
        properties: Dict[str, Any] = {}
        required = []

        for param in tool_def.parameters:
            properties[param.name] = {
                "type": param.type.upper(),  # Google uses uppercase types
                "description": param.description
            }
            if param.enum:
                properties[param.name]["enum"] = param.enum

            if param.properties:
                properties[param.name]["properties"] = {
                    prop_name: {
                        "type": prop.type.upper(),
                        "description": prop.description
                    }
                    for prop_name, prop in param.properties.items()
                }

            if param.required:
                required.append(param.name)

        return {
            "name": tool_def.name,
            "description": tool_def.description,
            "parameters": {
                "type": "OBJECT",
                "properties": properties,
                "required": required
            }
        }

    def _build_user_message(self, content: str, tool_results: List[AIToolResult] | None = None) -> Dict[str, Any]:
        """
        Build user message for Google format.

        Args:
            content: User message content
            tool_results: Optional tool results to include

        Returns:
            User message dictionary with structured content
        """
        parts: List[Dict[str, Any]] = []

        # Add text content if present
        if content:
            parts.append({
                "text": content
            })

        # Add tool results as function response parts
        if tool_results:
            for tool_result in tool_results:
                response: Dict[str, Any] = {
                }

                if tool_result.error:
                    response["error"] = tool_result.error

                else:
                    response["content"] = tool_result.content

                parts.append({
                    "functionResponse": {
                        "name": tool_result.name,
                        "response": response
                    }
                })

        return {
            "role": "user",
            "parts": parts
        }

    def _build_assistant_message(self, content: str, tool_calls: List[AIToolCall] | None = None) -> Dict[str, Any]:
        """
        Build assistant message for Google format.

        Args:
            content: Assistant message content
            tool_calls: Optional tool calls made by the assistant

        Returns:
            Assistant message dictionary with structured content
        """
        parts: List[Dict[str, Any]] = []

        # Add text content if present
        if content:
            parts.append({
                "text": content
            })

        # Add tool calls as function call parts
        if tool_calls:
            for tool_call in tool_calls:
                parts.append({
                    "functionCall": {
                        "name": tool_call.name,
                        "args": tool_call.arguments
                    }
                })

        return {
            "role": "model",
            "parts": parts
        }

    def _build_message(self, content: str, role: str) -> Dict[str, Any]:
        """
        Build message for Google format.

        Args:
            content: Message content
            role: Message role ("user" or "model")

        Returns:
            Message dictionary formatted for Google API
        """
        return {
            "role": role,
            "parts": [{
                "text": content
            }]
        }

    def _format_messages_for_provider(self, conversation_history: List[AIMessage]) -> List[Dict[str, Any]]:
        """
        Format conversation history for Google's API format in a single pass.

        Args:
            conversation_history: List of AIMessage objects

        Returns:
            List of messages formatted for Google API
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

                user_msg = self._build_user_message(
                    content=message.content,
                    tool_results=message.tool_results
                )
                result.append(user_msg)

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
        """Build complete request configuration for Google."""
        messages = self._format_messages_for_provider(conversation_history)

        # Build generation config
        generation_config = {
            "topP": 0.8,
            "topK": 10
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            generation_config["temperature"] = settings.temperature

        # Build request data
        data = {
            "contents": messages,
            "safetySettings": [
                {
                    "category": "HARM_CATEGORY_DANGEROUS_CONTENT",
                    "threshold": "BLOCK_ONLY_HIGH"
                }
            ],
            "generationConfig": generation_config
        }

        # Add tools if supported
        if self._supports_tools(settings):
            tool_definitions = self._tool_manager.get_tool_definitions()
            if tool_definitions:
                data["tools"] = [{
                    "function_declarations": [self._format_tool_definition(tool_def) for tool_def in tool_definitions]
                }]
                self._logger.debug("Added %d tool definitions for google", len(tool_definitions))

        # Build URL with model and API key
        model_path = AIConversationSettings.get_name(settings.model)
        url = f"{self._api_url}/{model_path}:streamGenerateContent?alt=sse&key={self._api_key}"

        # Build headers
        headers = {
            "Content-Type": "application/json"
        }

        return RequestConfig(
            url=url,
            headers=headers,
            data=data
        )

    def _create_stream_response_handler(self) -> GoogleStreamResponse:
        """Create a Google-specific stream response handler."""
        return GoogleStreamResponse()
