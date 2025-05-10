"""Deepseek backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.deepseek.deepseek_stream_response import DeepseekStreamResponse


class DeepseekBackend(AIBackend):
    """Deepseek API backend implementation with streaming support."""

    def __init__(self, api_key: str, base_url: str | None = None):
        """Initialize the Deepseek backend.

        Args:
            api_key: API key for Deepseek
            base_url: Custom API base URL (optional)
        """
        super().__init__()
        self._api_key = api_key
        self._api_url = base_url or "https://api.deepseek.com/chat/completions"

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: AIConversationSettings) -> dict:
        """Build Deepseek-specific request data."""
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

    def _create_stream_response_handler(self) -> DeepseekStreamResponse:
        """Create an Deepseek-specific stream response handler."""
        return DeepseekStreamResponse()

    def _get_api_url(self, settings: AIConversationSettings) -> str:
        """Get the Deepseek API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the Deepseek API headers."""
        return {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }
