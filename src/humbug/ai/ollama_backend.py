"""Ollama backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.ollama_stream_response import OllamaStreamResponse # Import the stream response handler


class OllamaBackend(AIBackend):
    """Ollama API backend implementation with streaming support."""

    def __init__(self):
        """Initialize the Ollama backend."""
        super().__init__()
        self._api_url = "http://localhost:11434/api/chat"
        self._default_settings = ConversationSettings("llama3.2")

        # Llama doesn't use normal SSE encoding!
        self._uses_data = False

    def _build_request_data(self, message: str, conversation_history: List[Dict[str, str]], settings: ConversationSettings) -> dict:
        """Build Ollama-specific request data."""
        messages = conversation_history.copy()
        messages.append({"role": "user", "content": message}) # Append user message

        data = {
            "model": settings.model,
            "messages": messages,
            "stream": True,
            "options": {
                "temperature": settings.temperature if settings.temperature else 0.7,
                "server_sent_events": True
            }
        }
        return data

    def _create_stream_response_handler(self):
        """Create an Ollama-specific stream response handler."""
        return OllamaStreamResponse()

    def _get_api_url(self, settings: ConversationSettings) -> str:
        """Get the Ollama API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the Ollama API headers."""
        return {
            "Content-Type": "application/json"
        }
