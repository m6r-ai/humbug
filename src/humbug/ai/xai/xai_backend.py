"""xAI backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.xai.xai_stream_response import XAIStreamResponse


class XAIBackend(AIBackend):
    """xAI API backend implementation with streaming support."""

    def __init__(self, api_key: str):
        """Initialize the xAI backend."""
        super().__init__()
        self._api_key = api_key
        self._api_url = "https://api.x.ai/v1/chat/completions"

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: AIConversationSettings) -> dict:
        """Build xAI-specific request data."""
        # conversation_history already contains properly formatted messages
        messages = conversation_history.copy()

        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True,
            "stream_options": {"include_usage": True}
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        self._logger.debug("stream message %r", data)
        return data

    def _create_stream_response_handler(self) -> XAIStreamResponse:
        """Create an xAI-specific stream response handler."""
        return XAIStreamResponse()

    def _get_api_url(self, settings: AIConversationSettings) -> str:
        """Get the xAI API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the xAI API headers."""
        return {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }
