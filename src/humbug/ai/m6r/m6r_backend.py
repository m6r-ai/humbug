"""M6R backend implementation."""
from typing import Dict, List, Any

from humbug.ai.ai_backend import AIBackend, RequestConfig
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage, AIMessageSource
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

    def _build_message(self, content: str, role: str) -> Dict[str, Any]:
        """
        Build message for M6R format.

        Args:
            content: Message content
            role: Message role ("user" or "assistant")

        Returns:
            Message dictionary formatted for M6R API
        """
        return {
            "role": role,
            "content": content
        }

    def _format_messages_for_provider(self, conversation_history: List[AIMessage]) -> List[Dict[str, Any]]:
        """
        Format conversation history for M6R's API format in a single pass.

        Args:
            conversation_history: List of AIMessage objects

        Returns:
            List of messages formatted for M6R API
        """
        result: List[Dict[str, Any]] = []
        last_user_message_index = -1

        for message in conversation_history:
            if message.source == AIMessageSource.USER:
                last_user_message_index = len(result)
                user_msg = self._build_message(message.content, "user")
                result.append(user_msg)
                continue

            # Check for problematic messages that should trigger cleanup
            is_problematic = (
                message.source == AIMessageSource.SYSTEM or
                (message.source == AIMessageSource.AI and (not message.completed or message.error)) or
                (message.source == AIMessageSource.REASONING and (not message.completed or message.error))
            )

            if is_problematic and last_user_message_index >= 0:
                self._logger.debug("Removing user message and subsequent messages due to %s", message.source)
                result = result[:last_user_message_index]
                last_user_message_index = -1
                continue

            if message.source == AIMessageSource.AI:
                # Only include completed AI messages without errors
                if not message.completed or message.error:
                    continue

                assistant_msg = self._build_message(message.content, "assistant")
                result.append(assistant_msg)
                continue

        return result

    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """Build complete request configuration for M6R."""
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
