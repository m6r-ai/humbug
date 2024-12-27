"""Anthropic backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.anthropic_stream_response import AnthropicStreamResponse


class AnthropicBackend(AIBackend):
    """Anthropic API backend implementation."""

    def __init__(self, api_key: str):
        """Initialize the Anthropic backend."""
        super().__init__()
        self._api_key = api_key
        self._api_url = "https://api.anthropic.com/v1/messages"

    def _build_request_data(self, message: str, conversation_history: List[Dict[str, str]], settings: ConversationSettings) -> dict:
        """Build Anthropic-specific request data."""
        # Take existing messages in correct format
        messages = conversation_history.copy()

        data = {
            "model": settings.model,
            "messages": messages,
            "max_tokens": 4096,
            "stream": True
        }

        # Only include temperature if supported by model
        if ConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        return data

    def _create_stream_response_handler(self):
        """Create an Anthropic-specific stream response handler."""
        return AnthropicStreamResponse()

    def _get_api_url(self, settings: ConversationSettings) -> str:
        """Get the Anthropic API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the Anthropic API headers."""
        return {
            "content-type": "application/json",
            "x-api-key": self._api_key,
            "anthropic-version": "2023-06-01"
        }
