"""Base class for AI backends."""

from abc import ABC, abstractmethod
import asyncio
from dataclasses import dataclass
import json
import logging
import os
import ssl
import sys
from typing import List, AsyncGenerator, Dict, Any

import aiohttp
from aiohttp import ClientConnectorError, ClientError
import certifi

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage
from humbug.ai.ai_rate_limiter import AIRateLimiter
from humbug.ai.ai_response import AIResponse, AIError
from humbug.ai.ai_stream_response import AIStreamResponse
from humbug.ai.ai_tool_manager import AIToolManager


@dataclass
class RequestConfig:
    """Complete configuration for an API request."""
    url: str
    headers: Dict[str, str]
    data: Dict[str, Any]


class AIBackend(ABC):
    """Abstract base class for AI backends."""

    @classmethod
    def get_default_url(cls) -> str:
        """
        Get the default API URL for this backend.

        Returns:
            The default URL for this backend.
        """
        return ""

    def __init__(self, api_key: str, api_url: str | None) -> None:
        """Initialize common attributes."""
        self._api_key = api_key
        self._api_url = api_url or self.get_default_url()
        self._uses_data = True  # Indicates that we default to normal SSE encoding
        self._max_retries = 6
        self._base_delay = 2
        self._rate_limiter = AIRateLimiter()
        self._logger = logging.getLogger(self.__class__.__name__)
        self._tool_manager = AIToolManager()

        if getattr(sys, "frozen", False) and hasattr(sys, '_MEIPASS'):
            cert_path = os.path.join(sys._MEIPASS, "certifi", "cacert.pem")

        else:
            cert_path = certifi.where()

        self._ssl_context = ssl.create_default_context(cafile=cert_path)

    @abstractmethod
    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        """
        Build complete request configuration for this backend.

        Args:
            conversation_history: List of conversation messages
            settings: Conversation settings

        Returns:
            RequestConfig containing URL, headers, and request data
        """

    @abstractmethod
    def _create_stream_response_handler(self) -> AIStreamResponse:
        """Abstract method to create a backend-specific stream response handler."""

    def _supports_tools(self, settings: AIConversationSettings) -> bool:
        """
        Check if the current model supports tool calling.

        Args:
            settings: Current conversation settings

        Returns:
            True if the model supports tools
        """
        model_config = AIConversationSettings.MODELS.get(settings.model)
        if not model_config:
            return False

        return model_config.supports_tools()

    async def stream_message(
        self,
        conversation_history: List[AIMessage],
        conversation_settings: AIConversationSettings
    ) -> AsyncGenerator[AIResponse, None]:
        """Send a message to the AI backend and stream the response."""
        # Format messages for this specific provider
        config = self._build_request_config(conversation_history, conversation_settings)

        attempt = 0
        while attempt < self._max_retries:
            try:
                await self._rate_limiter.acquire()

                post_timeout = aiohttp.ClientTimeout(
                    total=None,
                    sock_connect=20,
                    sock_read=120
                )

                # Use explicit IPv4 for local connections as localhost can cause SSL issues!
                url = config.url.replace("localhost", "127.0.0.1")

                async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=self._ssl_context)) as session:
                    async with session.post(
                        url,
                        headers=config.headers,
                        json=config.data,
                        timeout=post_timeout
                    ) as response:
                        # Did we get an error response?  If yes, then deal with it.
                        if response.status != 200:
                            response_message = await response.text()

                            try:
                                error_data = json.loads(response_message)
                                error_msg = error_data.get("error", {})
                                if not isinstance(error_msg, str):
                                    error_msg = error_msg.get("message", "Unknown error")

                            except json.JSONDecodeError as e:
                                self._logger.warning("Unable to parse: %s (%s)", response_message, str(e))
                                error_data = {}
                                error_msg = "Unknown error"

                            self._logger.debug("API error: %d: %s", response.status, error_data)

                            # If we get a 429 error, this is a rate limit error and we should retry
                            if response.status == 429:
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

                            # If we get a 503 error, the server is overloaded and we should retry
                            if response.status == 503:
                                if attempt < self._max_retries - 1:
                                    delay = self._base_delay * (2 ** attempt)
                                    yield AIResponse(
                                        reasoning="",
                                        content="",
                                        error=AIError(
                                            code="overloaded",
                                            message=f"Server is overloaded.  Retrying in {delay} seconds...",
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
                                    code=str(response.status),
                                    message=f"API error {response.status}: {error_msg}",
                                    retries_exhausted=True,
                                    details=error_data
                                )
                            )
                            return

                        # We got a success code.  Create a response handler and start generating AIResponse
                        # updates for each server-sent event we see.
                        response_handler = self._create_stream_response_handler()
                        async for line in response.content:
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
                                    tool_calls=response_handler.tool_calls
                                )

                            except json.JSONDecodeError as e:
                                self._logger.exception("JSON exception: %s", e)
                                continue

                            except Exception as e:
                                self._logger.exception("Unexpected exception: %s", e)
                                break

                        # Successfully processed response, exit retry loop
                        break

            except (ClientConnectorError, ClientError, asyncio.TimeoutError) as e:
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
