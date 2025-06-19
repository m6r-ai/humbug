"""M6R backend implementation."""
from typing import Dict, List, Any

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

    def _format_messages_for_provider(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Format conversation history for M6R's API format."""
        result = []

        for message in conversation_history:
            # Skip tool-related messages since M6R doesn't support them in this implementation
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
        """Build complete request configuration for M6R."""
        # Use the pre-formatted messages directly
        messages = self._format_messages_for_provider(conversation_history)

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
