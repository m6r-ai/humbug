"""Google Google backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage, AIMessageSource
from humbug.ai.google.google_stream_response import GoogleStreamResponse


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
        result = []

        for message in conversation_history:
            # Handle user messages
            if message.source == AIMessageSource.USER:
                user_msg = self._build_message(message.content, "user")
                result.append(user_msg)
                continue

            if message.source == AIMessageSource.AI:
                # Only include completed AI messages without errors
                if not message.completed or message.error:
                    continue

                assistant_msg = self._build_message(message.content, "model")
                result.append(assistant_msg)
                continue

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
