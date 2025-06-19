"""Ollama backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ollama.ollama_stream_response import OllamaStreamResponse


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

    def _format_messages_for_provider(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Format conversation history for Ollama's API format."""
        result = []

        for message in conversation_history:
            # Skip tool-related messages since Ollama doesn't support them in this implementation
            if "tool_calls" in message or "tool_results" in message:
                continue

            result.append({
                "role": message["role"],
                "content": message["content"]
            })

        return result

    def _build_request_config(
        self,
        conversation_history: List[Dict[str, Any]],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Ollama."""
        # Use the pre-formatted messages directly
        messages = self._format_messages_for_provider(conversation_history)

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True,
            "options": {
                "temperature": settings.temperature if settings.temperature else 0.7,
                "server_sent_events": True
            }
        }

        # Build headers
        headers = {
            "Content-Type": "application/json"
        }

        return RequestConfig(
            url=self._api_url,
            headers=headers,
            data=data
        )

    def _create_stream_response_handler(self) -> OllamaStreamResponse:
        """Create an Ollama-specific stream response handler."""
        return OllamaStreamResponse()
