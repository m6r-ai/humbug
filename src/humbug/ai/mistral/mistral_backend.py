"""Mistral backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.mistral.mistral_stream_response import MistralStreamResponse


class MistralBackend(AIBackend):
    """Mistral API backend implementation with streaming support."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL.

        Returns:
            The default URL
        """
        return "https://api.mistral.ai/v1/chat/completions"

    def _build_request_config(
        self,
        conversation_history: List[Dict[str, str]],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for Mistral."""
        # Format messages for Mistral (no special formatting needed)
        messages = conversation_history.copy()

        # Build request data
        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "max_tokens": settings.max_output_tokens,
            "stream": True
        }

        # Only include temperature if supported by model
        if AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        self._logger.debug("stream message %r", data)

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

    def _create_stream_response_handler(self) -> MistralStreamResponse:
        """Create an Mistral-specific stream response handler."""
        return MistralStreamResponse()
