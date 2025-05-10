"""Anthropic backend implementation."""
from typing import Dict, List

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_conversation_settings import AIConversationSettings, ReasoningCapability
from humbug.ai.anthropic.anthropic_stream_response import AnthropicStreamResponse


class AnthropicBackend(AIBackend):
    """Anthropic API backend implementation."""

    def __init__(self, api_key: str, base_url: str | None = None):
        """Initialize the Anthropic backend.

        Args:
            api_key: API key for Anthropic
            base_url: Custom API base URL (optional)
        """
        super().__init__()
        self._api_key = api_key
        self._api_url = base_url or "https://api.anthropic.com/v1/messages"

    def _build_request_data(self, conversation_history: List[Dict[str, str]], settings: AIConversationSettings) -> dict:
        """Build Anthropic-specific request data."""
        # Take existing messages in correct format
        messages = conversation_history.copy()

        data = {
            "model": AIConversationSettings.get_name(settings.model),
            "messages": messages,
            "max_tokens": settings.max_output_tokens,
            "stream": True
        }

        # Add thinking configuration if VISIBLE_REASONING is enabled.  Set budget at 90% of the maximum token count.
        thinking = False
        if (settings.reasoning & ReasoningCapability.VISIBLE_REASONING) == ReasoningCapability.VISIBLE_REASONING:
            thinking = True
            data["thinking"] = {
                "type": "enabled",
                "budget_tokens": int(settings.max_output_tokens * 0.9)
            }

        # Only include temperature if supported by model
        if not thinking and AIConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        return data

    def _create_stream_response_handler(self) -> AnthropicStreamResponse:
        """Create an Anthropic-specific stream response handler."""
        return AnthropicStreamResponse()

    def _get_api_url(self, settings: AIConversationSettings) -> str:
        """Get the Anthropic API URL."""
        return self._api_url

    def _get_headers(self) -> dict:
        """Get the Anthropic API headers."""
        return {
            "content-type": "application/json",
            "x-api-key": self._api_key,
            "anthropic-version": "2023-06-01"
        }
