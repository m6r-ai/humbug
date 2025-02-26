"""Google Gemini backend implementation."""
from typing import Dict, List

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

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: ConversationSettings) -> dict:
        """Build Gemini-specific request data."""
        contents = []

        # Convert history format to Gemini format
        for msg in conversation_history:
            role = "model" if msg["role"] == "assistant" else "user"
            contents.append({
                "role": role,
                "parts": [{
                    "text": msg["content"]
                }]
            })

        data = {
            "contents": contents,
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
        model_path = ConversationSettings.get_name(settings.model)
        return f"{self._api_base}/{model_path}:streamGenerateContent?alt=sse&key={self._api_key}"

    def _get_headers(self) -> dict:
        """Get the Gemini API headers."""
        return {
            "Content-Type": "application/json"
        }
