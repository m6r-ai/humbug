"""M6R backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.m6r.m6r_stream_response import M6RStreamResponse


class M6RBackend(AIBackend):
    """M6R API backend implementation."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "http://localhost:8080/v1/chat"

    def _build_request_config(
        self,
        conversation_history: List[Dict[str, str]],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for M6R."""
        # Format messages for M6R (no special formatting needed)
        messages = conversation_history.copy()

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "stream": True
        }

        # Build headers
        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self._api_key}"
        }

        return RequestConfig(
            url=self._api_url,
            headers=headers,
            data=data
        )

    def _create_stream_response_handler(self) -> M6RStreamResponse:
        """Create a M6R-specific stream response handler."""
        return M6RStreamResponse()
