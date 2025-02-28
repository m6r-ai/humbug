"""M6R backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.m6r.m6r_stream_response import M6RStreamResponse


class M6RBackend(AIBackend):
    """M6R API backend implementation."""

    def __init__(self, api_key: str):
        """Initialize the M6R backend."""
        super().__init__()
        self._api_key = api_key
        self._api_url = "http://localhost:8080/v1/chat"  # Default to localhost

        # M6R uses standard SSE encoding
        self._uses_data = True

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: AIConversationSettings) -> dict:
        """Build M6R-specific request data."""
        # Take existing messages and include current message
        messages = conversation_history.copy()

        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True
        }

        return data

    def _create_stream_response_handler(self):
        """Create a M6R-specific stream response handler."""
        return M6RStreamResponse()

    def _get_api_url(self, settings: AIConversationSettings) -> str:
        """Get the M6R API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the M6R API headers."""
        return {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }
