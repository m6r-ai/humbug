"""Google Gemini backend implementation."""
from typing import List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.gemini_stream_response import GeminiStreamResponse


class GeminiBackend(AIBackend):
    """Google Gemini API backend implementation."""

    def __init__(self, api_key: str):
        """Initialize the Gemini backend."""
        super().__init__()
        self._api_key = api_key
        self._api_base = "https://generativelanguage.googleapis.com/v1beta/models"
        self._default_settings = ConversationSettings("gemini-1.5-flash")

    def _build_request_data(self, message: str, conversation_history: List[str], settings: ConversationSettings) -> dict:
        """Build Gemini-specific request data."""
        # Combine history and current message into context
        messages = [{"text": msg} for msg in conversation_history]

        data = {
            "contents": [{
                "parts": messages
            }],
            "safetySettings": [
                {
                    "category": "HARM_CATEGORY_DANGEROUS_CONTENT",
                    "threshold": "BLOCK_ONLY_HIGH"
                }
            ],
            "generationConfig": {
                "topP": 0.8,
                "topK": 10
            }
        }

        # Only include temperature if supported by model
        if ConversationSettings.supports_temperature(settings.model):
            data["generationConfig"]["temperature"] = settings.temperature

        return data

    def _create_stream_response_handler(self):
        """Create a Gemini-specific stream response handler."""
        return GeminiStreamResponse()

    def _get_api_url(self, settings: ConversationSettings) -> str:
        """Get the Gemini API URL."""
        return f"{self._api_base}/{settings.model}:streamGenerateContent?alt=sse"

    def _get_headers(self) -> dict:
        """Get the Gemini API headers."""
        return {
            "Content-Type": "application/json",
            "x-goog-api-key": self._api_key
        }
