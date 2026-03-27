"""Humbug AI backend — connects to a local wrapper API at localhost:8000."""
import asyncio
import json
from typing import List, AsyncGenerator

import aiohttp
from aiohttp import ClientConnectorError, ClientError

from ai.ai_backend import AIBackend, RequestConfig
from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_message import AIMessage, AIMessageSource
from ai.ai_response import AIResponse, AIError
from ai.ai_stream_response import AIStreamResponse
from ai.ai_usage import AIUsage


class HumbugBackend(AIBackend):
    """Backend for the local Humbug AI wrapper API.

    Sends POST /api/chat with {"message": "<last user message>"}
    and reads back {"answer": "...", "conversation_id": "...", "loops": N}.
    """

    @classmethod
    def get_default_url(cls) -> str:
        return "http://localhost:8000/api/chat"

    # ------------------------------------------------------------------ #
    # Abstract method stubs (stream_message is fully overridden below)    #
    # ------------------------------------------------------------------ #

    def _build_request_config(
        self,
        conversation_history: List[AIMessage],
        settings: AIConversationSettings
    ) -> RequestConfig:
        # Extract the last user message text
        last_user_message = ""
        for msg in reversed(conversation_history):
            if msg.source == AIMessageSource.USER:
                last_user_message = msg.content or ""
                break

        return RequestConfig(
            url=self._api_url,
            headers={"Content-Type": "application/json"},
            data={"message": last_user_message}
        )

    def _create_stream_response_handler(self) -> AIStreamResponse:
        raise NotImplementedError("HumbugBackend uses a non-streaming response")

    # ------------------------------------------------------------------ #
    # Core: single-shot POST, no streaming                                #
    # ------------------------------------------------------------------ #

    async def stream_message(
        self,
        conversation_history: List[AIMessage],
        conversation_settings: AIConversationSettings
    ) -> AsyncGenerator[AIResponse, None]:
        """POST the last user message and yield the answer as a single response."""
        config = self._build_request_config(conversation_history, conversation_settings)
        url = config.url.replace("localhost", "127.0.0.1")

        attempt = 0
        while attempt < self._max_retries:
            try:
                timeout = aiohttp.ClientTimeout(total=None, sock_connect=20, sock_read=300)

                async with aiohttp.ClientSession(
                    connector=aiohttp.TCPConnector(ssl=self._ssl_context)
                ) as session:
                    async with session.post(
                        url,
                        headers=config.headers,
                        json=config.data,
                        timeout=timeout
                    ) as response:
                        if response.status != 200:
                            body = await response.text()
                            try:
                                error_data = json.loads(body)
                            except json.JSONDecodeError:
                                error_data = {"raw": body}

                            if response.status in {429, 500, 501, 502, 503, 504} and attempt < self._max_retries - 1:
                                delay = self._base_delay * (2 ** attempt)
                                yield AIResponse(
                                    reasoning="", content="",
                                    error=AIError(
                                        code=str(response.status),
                                        message=f"Error {response.status}. Retrying in {delay}s...",
                                        retries_exhausted=False,
                                        details=error_data
                                    )
                                )
                                await asyncio.sleep(delay)
                                attempt += 1
                                continue

                            yield AIResponse(
                                reasoning="", content="",
                                error=AIError(
                                    code=str(response.status),
                                    message=f"API error {response.status}: {error_data}",
                                    retries_exhausted=True,
                                    details=error_data
                                )
                            )
                            return

                        # Signal connected
                        yield AIResponse(reasoning="", content="", connected=True)

                        body = await response.text()
                        data = json.loads(body)
                        answer = data.get("answer", "")

                        yield AIResponse(
                            reasoning="",
                            content=answer,
                            usage=AIUsage(prompt_tokens=0, completion_tokens=0, total_tokens=0)
                        )
                        return

            except (ClientConnectorError, ClientError, asyncio.TimeoutError) as e:
                self._logger.warning("Network error (attempt %d/%d): %s", attempt + 1, self._max_retries, str(e))
                delay = self._base_delay * (2 ** attempt)

                if attempt < self._max_retries - 1:
                    yield AIResponse(
                        reasoning="", content="",
                        error=AIError(
                            code="network_error",
                            message=f"Network error: {str(e)}. Retrying in {delay}s...",
                            retries_exhausted=False,
                            details={"type": type(e).__name__}
                        )
                    )
                    await asyncio.sleep(delay)
                    attempt += 1
                    continue

                yield AIResponse(
                    reasoning="", content="",
                    error=AIError(
                        code="network_error",
                        message=f"Network error: {str(e)}",
                        retries_exhausted=True,
                        details={"type": type(e).__name__}
                    )
                )
                return

            except Exception as e:
                self._logger.exception("Unexpected error: %s", str(e))
                yield AIResponse(
                    reasoning="", content="",
                    error=AIError(
                        code="error",
                        message=f"Error: {str(e)}",
                        retries_exhausted=True,
                        details={"type": type(e).__name__}
                    )
                )
                return
