"""Mistral backend implementation."""
from typing import Dict, List, Any

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

    def _format_messages_for_provider(self, conversation_history: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Format conversation history for Mistral's API format."""
        result = []

        for message in conversation_history:
            # Skip tool-related messages since Mistral doesn't support them in this implementation
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
        """Build complete request configuration for Mistral."""
        # Use the pre-formatted messages directly
        messages = self._format_messages_for_provider(conversation_history)

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
