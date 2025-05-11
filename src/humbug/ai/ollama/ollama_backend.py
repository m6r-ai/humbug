"""Ollama backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ollama.ollama_stream_response import OllamaStreamResponse # Import the stream response handler


class OllamaBackend(AIBackend):
    """Ollama API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "http://localhost:11434/api/chat"

    def __init__(self, api_key: str, api_url: str | None = None) -> None:
        """Initialize the Ollama backend.

        Args:
            api_key: API key for authentication (not used in this case)
            api_url: Custom API base URL (optional)
        """
        super().__init__(api_key, api_url)

        # Llama doesn't use normal SSE encoding!
        self._uses_data = False

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: AIConversationSettings) -> dict:
        """Build Ollama-specific request data."""
        messages = conversation_history.copy()

        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True,
            "options": {
                "temperature": settings.temperature if settings.temperature else 0.7,
                "server_sent_events": True
            }
        }
        return data

    def _create_stream_response_handler(self) -> OllamaStreamResponse:
        """Create an Ollama-specific stream response handler."""
        return OllamaStreamResponse()

    def _get_api_url(self, _settings: AIConversationSettings) -> str:
        """Get the Ollama API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the Ollama API headers."""
        return {
            "Content-Type": "application/json"
        }
