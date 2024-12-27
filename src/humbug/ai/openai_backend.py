"""OpenAI backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.openai_stream_response import OpenAIStreamResponse


class OpenAIBackend(AIBackend):
    """OpenAI API backend implementation with streaming support."""

    def __init__(self, api_key: str):
        """Initialize the OpenAI backend."""
        super().__init__()
        self._api_key = api_key
        self._api_url = "https://api.openai.com/v1/chat/completions"

    def _build_request_data(self, message: str, conversation_history: List[Dict[str, str]], settings: ConversationSettings) -> dict:
        """Build OpenAI-specific request data."""
        # conversation_history already contains properly formatted messages
        messages = conversation_history.copy()

        data = {
            "model": settings.model,
            "messages": messages,
            "stream": True,
            "stream_options": {"include_usage": True}
        }

        # Only include temperature if supported by model
        if ConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        self._logger.debug("stream message %s", data)
        return data

    def _create_stream_response_handler(self):
        """Create an OpenAI-specific stream response handler."""
        return OpenAIStreamResponse()

    def _get_api_url(self, settings: ConversationSettings) -> str:
        """Get the OpenAI API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the OpenAI API headers."""
        return {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }
