"""xAI backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.xai.xai_stream_response import XAIStreamResponse


class XAIBackend(AIBackend):
    """xAI API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.x.ai/v1/chat/completions"

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

    def _format_messages_for_context(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Format conversation history."""
        return conversation_history

    def _add_tools_to_request_data(self, data: dict, settings: AIConversationSettings) -> None:
        """Add tool definitions to request data."""
