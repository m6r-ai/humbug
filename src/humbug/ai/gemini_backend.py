"""Google Gemini backend implementation."""

import asyncio
import json
import logging
from typing import AsyncGenerator, List, Dict

import aiohttp

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_response import AIResponse
from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.gemini_stream_response import GeminiStreamResponse
from humbug.ai.rate_limiter import RateLimiter


class GeminiBackend(AIBackend):
    """Google Gemini API backend implementation."""

    def __init__(self, api_key: str):
        """Initialize the Gemini backend."""
        super().__init__()
        self._api_key = api_key
        self._api_base = "https://generativelanguage.googleapis.com/v1beta/models"
        self._conversation_settings: Dict[str, ConversationSettings] = {}
        self._default_settings = ConversationSettings("gemini-1.5-flash")
        self._max_retries = 3
        self._base_delay = 2
        self._rate_limiter = RateLimiter()
        self._logger = logging.getLogger("GeminiBackend")

    def update_conversation_settings(self, conversation_id: str, settings: ConversationSettings):
        """Update settings for a specific conversation."""
        self._conversation_settings[conversation_id] = settings

    def get_conversation_settings(self, conversation_id: str) -> ConversationSettings:
        """Get settings for a specific conversation."""
        return self._conversation_settings.get(conversation_id, self._default_settings)

    def _build_request_data(self, message: str, conversation_history: List[str], settings: ConversationSettings) -> dict:
        """Build Gemini-specific request data."""
        # Combine history and current message into context
        messages = [{"text": msg} for msg in conversation_history]
        messages.append({"text": message})

        data = {
            "contents": [{
                "parts": messages
            }],
            "safetySettings": [
                {
                    "category": "HARM_CATEGORY_DANGEROUS_CONTENT",
                    "threshold": "BLOCK_ONLY_HIGH"
                }
            ],
            "generationConfig": {
                "maxOutputTokens": 800,
                "topP": 0.8,
                "topK": 10
            }
        }

        # Only include temperature if supported by model
        if ConversationSettings.supports_temperature(settings.model):
            data["generationConfig"]["temperature"] = settings.temperature

        return data

    async def stream_message(
        self,
        message: str,
        conversation_history: List[str],
        conversation_id: str = None
    ) -> AsyncGenerator[AIResponse, None]:
        """Send a message to the Gemini API and stream the response."""
        settings = self.get_conversation_settings(conversation_id) if conversation_id else self._default_settings
        model = settings.model

        url = f"{self._api_base}/{model}:streamGenerateContent?alt=sse"
        data = self._build_request_data(message, conversation_history, settings)

        try:
            for attempt in range(self._max_retries):
                try:
                    await self._rate_limiter.acquire()

                    # Configure timeouts to match OpenAI backend
                    post_timeout = aiohttp.ClientTimeout(
                        total=None,  # No total timeout
                        sock_connect=20,  # 20 second connection timeout
                        sock_read=10  # 10 second read timeout
                    )

                    async with aiohttp.ClientSession() as session:
                        async with session.post(
                            url,
                            headers={
                                "Content-Type": "application/json",
                                "x-goog-api-key": self._api_key
                            },
                            json=data,
                            timeout=post_timeout
                        ) as response:
                            # Handle HTTP-level errors
                            if response.status != 200:
                                error_data = await response.json()
                                error_msg = "Unknown error"
                                if "error" in error_data:
                                    error_msg = error_data["error"].get("message", error_msg)
                                    # Map Gemini error codes to consistent format
                                    if "status" in error_data["error"]:
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
                                            content="",
                                            error={
                                                "code": "rate_limit",
                                                "message": f"Rate limit exceeded. Retrying in {delay} seconds...",
                                                "details": error_data
                                            }
                                        )
                                        await asyncio.sleep(delay)
                                        continue

                                yield AIResponse(
                                    content="",
                                    error={
                                        "code": str(response.status),
                                        "message": f"API error: {error_msg}",
                                        "details": error_data
                                    }
                                )
                                return

                            response_handler = GeminiStreamResponse()
                            try:
                                async for line in response.content:
                                    try:
                                        line = line.decode('utf-8').strip()
                                        if not line:
                                            continue

                                        if line.startswith("data: "):
                                            line = line[6:]

                                        if line == "[DONE]":
                                            break

                                        chunk = json.loads(line)
                                        response_handler.update_from_chunk(chunk)
                                        if response_handler.error:
                                            yield AIResponse(
                                                content="",
                                                error=response_handler.error
                                            )
                                            return

                                        yield AIResponse(
                                            content=response_handler.content,
                                            usage=response_handler.usage
                                        )

                                    except asyncio.CancelledError:
                                        self._logger.exception("CancelledError")
                                        yield AIResponse(
                                            content=response_handler.content,
                                            error={
                                                "code": "cancelled",
                                                "message": "Request cancelled by user"
                                            }
                                        )
                                        return

                                    except json.JSONDecodeError as e:
                                        self._logger.exception("JSON exception: %s", e)
                                        continue

                                    except Exception as e:
                                        self._logger.exception("unexpected exception: %s", e)
                                        break

                                # Successfully processed response, exit retry loop
                                break

                            except asyncio.TimeoutError:
                                delay = self._base_delay * (2 ** attempt)
                                self._logger.debug("Timeout on attempt %d/%d", attempt + 1, self._max_retries)
                                if attempt < self._max_retries - 1:
                                    yield AIResponse(
                                        content="",
                                        error={
                                            "code": "timeout",
                                            "message": f"Request timed out (attempt {attempt + 1}/{self._max_retries}). Retrying in {delay} seconds...",
                                            "details": {"attempt": attempt + 1}
                                        }
                                    )
                                    await asyncio.sleep(delay)
                                    self._logger.debug("Retrying after timeout (attempt %d/%d)", attempt + 2, self._max_retries)
                                    continue

                                yield AIResponse(
                                    content="",
                                    error={
                                        "code": "timeout",
                                        "message": f"Request timed out after {self._max_retries} attempts",
                                        "details": {"attempt": attempt + 1}
                                    }
                                )
                                return

                            except aiohttp.ClientError as e:
                                delay = self._base_delay * (2 ** attempt)
                                self._logger.debug("Network error on attempt %d/%d: %s", attempt + 1, self._max_retries, str(e))
                                if attempt < self._max_retries - 1:
                                    yield AIResponse(
                                        content="",
                                        error={
                                            "code": "network_error",
                                            "message": f"Network error (attempt {attempt + 1}/{self._max_retries}): {str(e)}. Retrying in {delay} seconds...",
                                            "details": {"type": type(e).__name__, "attempt": attempt + 1}
                                        }
                                    )
                                    await asyncio.sleep(delay)
                                    self._logger.debug("Retrying after network error (attempt %d/%d)", attempt + 2, self._max_retries)
                                    continue

                                yield AIResponse(
                                    content="",
                                    error={
                                        "code": "network_error",
                                        "message": f"Network error after {self._max_retries} attempts: {str(e)}",
                                        "details": {"type": type(e).__name__, "attempt": attempt + 1}
                                    }
                                )
                                return

                except Exception as e:
                    self._logger.exception("Unexpected error: %s", str(e))
                    yield AIResponse(
                        content="",
                        error={
                            "code": "error",
                            "message": f"Error: {str(e)}",
                            "details": {"type": type(e).__name__}
                        }
                    )

        except (GeneratorExit, asyncio.CancelledError):
            self._logger.debug("Request cancelled for conversation %s", conversation_id)
            raise
