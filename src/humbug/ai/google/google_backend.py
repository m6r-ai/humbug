"""Google Google backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.google.google_stream_response import GoogleStreamResponse


class GoogleBackend(AIBackend):
    """Google Gemini API backend implementation."""

    def __init__(self, api_key: str):
        """Initialize the Google backend."""
        super().__init__()
        self._api_key = api_key
        self._api_base = "https://generativelanguage.googleapis.com/v1beta/models"

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: AIConversationSettings) -> dict:
        """Build Google-specific request data."""
        contents = []

        # Convert history format to Google format
        for msg in conversation_history:
            role = "model" if msg["role"] == "assistant" else "user"
            contents.append({
                "role": role,
                "parts": [{
                    "text": msg["content"]
                }]
            })

        generation_config = {
            "topP": 0.8,
            "topK": 10
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            generation_config["temperature"] = settings.temperature

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

        return data

    def _create_stream_response_handler(self) -> GoogleStreamResponse:
        """Create a Google-specific stream response handler."""
        return GoogleStreamResponse()

    def _get_api_url(self, settings: AIConversationSettings) -> str:
        """Get the Google API URL."""
        model_path = AIConversationSettings.get_name(settings.model)
        return f"{self._api_base}/{model_path}:streamGenerateContent?alt=sse&key={self._api_key}"

    def _get_headers(self) -> dict:
        """Get the Google API headers."""
        return {
            "Content-Type": "application/json"
        }
