"""Base class for AI backends."""

from abc import ABC, abstractmethod
import asyncio
from collections.abc import AsyncGenerator
from dataclasses import dataclass
import json
import logging
import os
import ssl
import sys
from typing import Any

import certifi

from http_client import ClientConnectorError, HttpClientError, HttpClient
from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_message import AIMessage
from ai.ai_conversation_history import AIConversationHistory
from ai.ai_response import AIResponse, AIError
from ai.ai_stream_response import AIStreamResponse
from ai_tool import AIToolManager
from syntax.programming_language_utils import ProgrammingLanguageUtils


@dataclass
class RequestConfig:
    """Complete configuration for an API request."""
    url: str
    headers: dict[str, str]
    data: dict[str, Any]


class AIBackend(ABC):
    """Abstract base class for AI backends."""

    # Class-level system prompt (shared across all backend instances)
    _system_prompt: str | None = None

    @classmethod
    def set_system_prompt(cls, prompt: str | None) -> None:
        """
        Set the global system prompt for all AI backends.

        Args:
            prompt: The system prompt to use, or None to clear it
        """
        cls._system_prompt = prompt

    def __init__(self, api_key: str, api_url: str) -> None:
        """Initialize common attributes."""
        self._api_key = api_key
        self._api_url = api_url
        self._uses_data = True  # Indicates that we default to normal SSE encoding
        self._max_retries = 6
        self._base_delay = 2
        self._logger = logging.getLogger(self.__class__.__name__)
        self._tool_manager = AIToolManager()

        if getattr(sys, "frozen", False) and hasattr(sys, '_MEIPASS'):
            cert_path = os.path.join(sys._MEIPASS, "certifi", "cacert.pem")  # type: ignore

        else:
            cert_path = certifi.where()

        self._ssl_context = ssl.create_default_context(cafile=cert_path)

    async def fetch_models(self) -> list[str]:
        """Fetch available model IDs from the provider API. Returns empty list by default."""
        return []

    @abstractmethod
    def _build_request_config(
        self,
        conversation_history: AIConversationHistory,
        settings: AIConversationSettings
    ) -> RequestConfig:
        """
        Build complete request configuration for this backend.

        Args:
            conversation_history: Conversation history including messages and attachments
            settings: Conversation settings

        Returns:
            RequestConfig containing URL, headers, and request data
        """

    @abstractmethod
    def _create_stream_response_handler(self) -> AIStreamResponse:
        """Abstract method to create a backend-specific stream response handler."""

    def _reasoning_model_matches(self, message: AIMessage, settings: AIConversationSettings) -> bool:
        """
        Determine whether a reasoning message from history is compatible with the current model.

        The default implementation matches on provider, which is correct for backends
        that serve a single well-defined API (Anthropic, DeepSeek, xAI, Z.ai).  Backends
        that host many structurally different models under one API (Ollama, vLLM) should
        override this method and require an exact model match instead.

        Args:
            message: The reasoning message from conversation history
            settings: Current conversation settings

        Returns:
            True if the reasoning message is compatible with the current model
        """
        return (message.provider or "") == settings.provider

    def _resolve_message_content(
        self,
        message: AIMessage,
        conversation_history: AIConversationHistory
    ) -> str:
        """
        Resolve a user message's content, prepending any attachments as fenced code blocks.

        Args:
            message: The user message whose content to resolve
            conversation_history: History containing the attachment store

        Returns:
            Message content with attachment contents prepended
        """
        if not message.attachments:
            return message.content

        parts = []
        for guid in message.attachments:
            resolved = conversation_history.get_attachment_content_for_request(guid)
            if resolved is not None:
                filename, attachment_content = resolved
                language = ProgrammingLanguageUtils.get_name(
                    ProgrammingLanguageUtils.from_file_extension(filename)
                )
                parts.append(f"`{filename}`:\n```{language}\n{attachment_content}\n```")

        if not parts:
            return message.content

        parts.append(message.content)
        return "\n\n".join(parts)

    def _supports_tools(self, settings: AIConversationSettings) -> bool:
        """
        Check if the current model supports tool calling.

        Args:
            settings: Current conversation settings

        Returns:
            True if the model supports tools
        """
        return AIConversationSettings.supports_tools(settings.model, settings.provider)

    async def stream_message(
        self,
        conversation_history: AIConversationHistory,
        conversation_settings: AIConversationSettings
    ) -> AsyncGenerator[AIResponse, None]:
        """Send a message to the AI backend and stream the response."""
        # Format messages for this specific provider
        config = self._build_request_config(conversation_history, conversation_settings)

        self._logger.debug(config.data)

        attempt = 0

        # Use explicit IPv4 for local connections as localhost can cause SSL issues!
        url = config.url.replace("localhost", "127.0.0.1")

        async with HttpClient(
            ssl_context=self._ssl_context,
            connect_timeout=20,
            read_timeout=300,
        ) as client:
            while attempt < self._max_retries:
                try:
                    response = await client.post(
                        url,
                        headers=config.headers,
                        json=config.data,
                    )

                    # Did we get an error response?  If yes, then deal with it.
                    if response.status() != 200:
                        response_message = await response.text()

                        try:
                            error_data = json.loads(response_message)

                        except json.JSONDecodeError as e:
                            self._logger.warning("Unable to parse: %s (%s)", response_message, str(e))
                            error_data = {}

                        self._logger.debug("API error: %d: %s", response.status(), error_data)

                        # If we get a 429 error, this is a rate limit error and we should retry
                        if response.status() == 429:
                            if attempt < self._max_retries - 1:
                                delay = self._base_delay * (2 ** attempt)
                                yield AIResponse(
                                    reasoning="",
                                    content="",
                                    error=AIError(
                                        code="rate_limit",
                                        message=f"Rate limit exceeded.  Retrying in {delay} seconds...",
                                        retries_exhausted=False,
                                        details=error_data
                                    )
                                )
                                await asyncio.sleep(delay)
                                attempt += 1
                                continue

                        # If we get one of many types of 500 series error, something went wrong on the server.  Some of these
                        # are non-standard errors, but are seen with some LLMs.
                        if response.status() in {500, 501, 502, 503, 504, 508, 509, 529}:
                            if attempt < self._max_retries - 1:
                                delay = self._base_delay * (2 ** attempt)
                                yield AIResponse(
                                    reasoning="",
                                    content="",
                                    error=AIError(
                                        code="server_error",
                                        message=f"Server error: {response.status()}.  Retrying in {delay} seconds...",
                                        retries_exhausted=False,
                                        details=error_data
                                    )
                                )
                                await asyncio.sleep(delay)
                                attempt += 1
                                continue

                        yield AIResponse(
                            reasoning="",
                            content="",
                            error=AIError(
                                code=str(response.status()),
                                message=f"API error {response.status()}: {error_data}",
                                retries_exhausted=True,
                                details=error_data
                            )
                        )
                        return

                    # Signal that AI is connected
                    yield AIResponse(
                        reasoning="",
                        content="",
                        connected=True
                    )

                    # We got a success code.  Create a response handler and start generating AIResponse
                    # updates for each server-sent event we see.
                    response_handler = self._create_stream_response_handler()
                    async for line in response.content_lines():
                        try:
                            decoded_line = line.decode('utf-8').strip()
                            if not decoded_line:
                                continue

                            if self._uses_data:
                                if not decoded_line.startswith("data: "):
                                    continue

                                decoded_line = decoded_line[6:]

                                if decoded_line == "[DONE]":
                                    break

                            chunk = json.loads(decoded_line)
                            response_handler.update_from_chunk(chunk)

                            if response_handler.error:
                                yield AIResponse(
                                    reasoning="",
                                    content="",
                                    error=response_handler.error
                                )
                                return

                            yield AIResponse(
                                reasoning=response_handler.reasoning,
                                content=response_handler.content,
                                usage=response_handler.usage,
                                tool_calls=response_handler.tool_calls,
                                signature=response_handler.signature,
                                redacted_reasoning=response_handler.redacted_reasoning
                            )

                        except json.JSONDecodeError as e:
                            self._logger.exception("JSON exception: %s", e)
                            break

                        except Exception as e:
                            self._logger.exception("Unexpected exception: %s", e)
                            break

                    # Successfully processed response, exit retry loop
                    break

                except (ClientConnectorError, HttpClientError, asyncio.TimeoutError, OSError) as e:
                    # Handle network-related errors that should be retried
                    self._logger.warning("Network error (attempt %d/%d): %s", attempt + 1, self._max_retries, str(e))
                    delay = self._base_delay * (2 ** attempt)

                    if attempt < self._max_retries - 1:
                        yield AIResponse(
                            reasoning="",
                            content="",
                            error=AIError(
                                code="network_error",
                                message=f"Network error: {str(e)}. Retrying in {delay} seconds...",
                                retries_exhausted=False,
                                details={"type": type(e).__name__, "attempt": attempt + 1}
                            )
                        )
                        await asyncio.sleep(delay)
                        attempt += 1
                        continue

                    yield AIResponse(
                        reasoning="",
                        content="",
                        error=AIError(
                            code="network_error",
                            message=f"Network error: {str(e)}",
                            retries_exhausted=True,
                            details={"type": type(e).__name__}
                        )
                    )
                    return

                except Exception as e:
                    # Handle non-retryable errors
                    self._logger.exception("Error processing AI response: %s", str(e))
                    yield AIResponse(
                        reasoning="",
                        content="",
                        error=AIError(
                            code="error",
                            message=f"Error: {str(e)}",
                            retries_exhausted=True,
                            details={"type": type(e).__name__}
                        )
                    )
                    return
