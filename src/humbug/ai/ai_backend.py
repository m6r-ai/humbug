"""Base class for AI backends."""

from abc import ABC, abstractmethod
import asyncio
import json
import logging
import os
import ssl
import sys
from typing import List, AsyncGenerator, Dict

import aiohttp
from aiohttp import ClientConnectorError, ClientError
import certifi

from humbug.ai.ai_response import AIResponse, AIError
from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.rate_limiter import RateLimiter


class AIBackend(ABC):
    """Abstract base class for AI backends."""

    def __init__(self):
        """Initialize common attributes."""
        self._conversation_settings: Dict[str, ConversationSettings] = {}
        self._default_settings = ConversationSettings()
        self._uses_data = True  # Indicates that we default to normal SSE encoding
        self._max_retries = 3
        self._base_delay = 2
        self._rate_limiter = RateLimiter()
        self._logger = logging.getLogger(self.__class__.__name__) # Logger based on class name

        if getattr(sys, "frozen", False):  # Check if running as a bundled app
            cert_path = os.path.join(sys._MEIPASS, "certifi", "cacert.pem")
        else:
            cert_path = certifi.where()

        self._ssl_context = ssl.create_default_context(cafile=cert_path)

    def update_conversation_settings(self, conversation_id: str, settings: ConversationSettings):
        """Update settings for a specific conversation."""
        self._conversation_settings[conversation_id] = settings

    def get_conversation_settings(self, conversation_id: str) -> ConversationSettings:
        """Get settings for a specific conversation."""
        return self._conversation_settings.get(conversation_id, self._default_settings)

    @abstractmethod
    def _build_request_data(self, message: str, conversation_history: List[str], settings: ConversationSettings) -> dict:
        """Abstract method to build backend-specific request data."""

    @abstractmethod
    def _create_stream_response_handler(self):
        """Abstract method to create a backend-specific stream response handler."""

    @abstractmethod
    def _get_api_url(self, settings: ConversationSettings) -> str:
        """Abstract method to get the API URL."""

    @abstractmethod
    def _get_headers(self) -> dict:
        """Abstract method to get the API headers."""

    async def stream_message(
        self,
        message: str,
        conversation_history: List[str],
        conversation_id: str = None
    ) -> AsyncGenerator[AIResponse, None]:
        """Send a message to the AI backend and stream the response."""
        settings = self.get_conversation_settings(conversation_id) if conversation_id else self._default_settings
        url = self._get_api_url(settings)
        data = self._build_request_data(message, conversation_history, settings)
        headers = self._get_headers()

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
                url = url.replace("localhost", "127.0.0.1")

                async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=self._ssl_context)) as session:
                    async with session.post(
                        url,
                        headers=headers,
                        json=data,
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

                            # Map Gemini error codes to consistent format
                            if "error" in error_data and "status" in error_data["error"]:
                                error_code = error_data["error"]["status"]
                                if error_code == "RESOURCE_EXHAUSTED":
                                    error_msg = "Rate limit exceeded. Retrying..."
                                elif error_code == "PERMISSION_DENIED":
                                    error_msg = "Invalid API key or permissions"
                                elif error_code == "UNAVAILABLE":
                                    error_msg = "Service temporarily unavailable"

                            if response.status == 429 or "RESOURCE_EXHAUSTED" in error_msg:
                                if attempt < self._max_retries - 1:
                                    delay = self._base_delay * (2 ** attempt)
                                    yield AIResponse(
                                        reasoning="",
                                        content="",
                                        error=AIError(
                                            code="rate_limit",
                                            message=f"Rate limit exceeded. Retrying in {delay} seconds...",
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
                                line = line.decode('utf-8').strip()
                                if not line:
                                    continue

                                if self._uses_data:
                                    if not line.startswith("data: "):
                                        continue

                                    line = line[6:]

                                    if line == "[DONE]":
                                        break

                                chunk = json.loads(line)
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
                                    usage=response_handler.usage
                                )

                            except json.JSONDecodeError as e:
                                self._logger.exception("JSON exception: %s", e)
                                continue

                            except Exception as e:
                                self._logger.exception("unexpected exception: %s", e)
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
