"""Google Google backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
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

    def _build_request_config(
        self,
        conversation_history: List[Dict[str, str]],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Google."""
        # Format messages for Google format
        contents = self._format_messages_for_google(conversation_history)

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
            "contents": contents,
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

    def _format_messages_for_google(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Convert history format to Google format."""
        contents = []

        for msg in conversation_history:
            role = "model" if msg["role"] == "assistant" else "user"
            contents.append({
                "role": role,
                "parts": [{
                    "text": msg["content"]
                }]
            })

        return contents

    def _create_stream_response_handler(self) -> GoogleStreamResponse:
        """Create a Google-specific stream response handler."""
        return GoogleStreamResponse()
