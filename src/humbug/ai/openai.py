"""OpenAI backend implementation."""

import asyncio
from collections import deque
import contextlib
import json
import logging
import time
from typing import AsyncGenerator, List, Dict, Optional

import aiohttp

from humbug.ai.base import AIBackend, AIResponse, AIUsage
from humbug.ai.conversation_settings import ConversationSettings


class RateLimiter:
    """Implements a sliding window rate limiter."""

    def __init__(self, window_size: int = 60, max_requests: int = 50):
        """Initialize rate limiter with window size in seconds."""
        self.window_size = window_size
        self.max_requests = max_requests
        self.requests = deque()

    async def acquire(self):
        """Wait until a request can be made within rate limits."""
        now = time.time()

        # Remove expired timestamps
        while self.requests and self.requests[0] <= now - self.window_size:
            self.requests.popleft()

        if len(self.requests) >= self.max_requests:
            # Calculate sleep time
            sleep_time = self.requests[0] + self.window_size - now
            if sleep_time > 0:
                await asyncio.sleep(sleep_time)

        self.requests.append(now)


class OpenAIStreamResponse:
    """Handles streaming response from OpenAI API."""

    def __init__(self):
        """Initialize stream response handler."""
        self.content = ""
        self.usage: Optional[AIUsage] = None
        self.error = None
        self.logger = logging.getLogger("OpenAIStreamResponse")

    def update_from_chunk(self, chunk: dict) -> None:
        """Update from a response chunk and return new content if any."""
        if "error" in chunk:
            self.error = {
                "code": "stream_error",
                "message": chunk["error"].get("message", "Unknown error"),
                "details": chunk["error"]
            }
            return

        if "usage" in chunk:
            usage = chunk["usage"]
            if usage:
                self.usage = AIUsage(
                    prompt_tokens=usage.get("prompt_tokens", 0),
                    completion_tokens=usage.get("completion_tokens", 0),
                    total_tokens=usage.get("total_tokens", 0)
                )

        if "choices" not in chunk:
            return

        choices = chunk["choices"]
        if not choices:
            return

        delta = choices[0].get("delta", {})
        if "content" in delta:
            new_content = delta["content"]
            self.content += new_content


@contextlib.asynccontextmanager
async def managed_stream(session, url, headers, data, timeout):
    """Manage the lifecycle of a streaming response."""
    async with session.post(
        url,
        headers=headers,
        json=data,
        timeout=timeout
    ) as response:
        yield response


class OpenAIBackend(AIBackend):
    """OpenAI API backend implementation with streaming support."""

    def __init__(self, api_key: str):
        """Initialize the OpenAI backend."""
        super().__init__()
        self.api_key = api_key
        self.api_url = "https://api.openai.com/v1/chat/completions"
        self.conversation_settings: Dict[str, ConversationSettings] = {}
        self.default_settings = ConversationSettings()
        self.max_retries = 3
        self.base_delay = 2
        self.rate_limiter = RateLimiter()
        self.logger = logging.getLogger("OpenAIBackend")

    def update_conversation_settings(self, conversation_id: str, settings: ConversationSettings):
        """Update settings for a specific conversation."""
        self.conversation_settings[conversation_id] = settings

    def get_conversation_settings(self, conversation_id: str) -> ConversationSettings:
        """Get settings for a specific conversation."""
        return self.conversation_settings.get(conversation_id, self.default_settings)

    async def stream_message(
        self,
        message: str,
        conversation_history: List[str],
        conversation_id: str = None
    ) -> AsyncGenerator[AIResponse, None]:
        """Send a message to the AI backend and stream the response."""
        settings = self.get_conversation_settings(conversation_id) if conversation_id else self.default_settings

        messages = [{"role": "user", "content": msg} for msg in conversation_history]
        messages.append({"role": "user", "content": message})

        data = {
            "model": settings.model,
            "messages": messages,
            "temperature": settings.temperature,
            "stream": True,
            "stream_options": {"include_usage": True}
        }

        self.logger.debug(f"stream message {data}")

        for attempt in range(self.max_retries):
            try:
                await self.rate_limiter.acquire()

                async with aiohttp.ClientSession() as session:
                    async with managed_stream(
                        session,
                        self.api_url,
                        headers={
                            "Content-Type": "application/json",
                            "Authorization": f"Bearer {self.api_key}"
                        },
                        data=data,
                        timeout=20
                    ) as response:
                        if response.status != 200:
                            error_data = await response.json()
                            error_msg = error_data.get("error", {}).get("message", "Unknown error")
                            yield AIResponse(
                                content="",
                                error={
                                    "code": str(response.status),
                                    "message": f"API error: {error_msg}",
                                    "details": error_data
                                }
                            )
                            return

                        response_handler = OpenAIStreamResponse()
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
                                    yield AIResponse(
                                        content=response_handler.content,
                                        error={
                                            "code": "cancelled",
                                            "message": "Request cancelled by user"
                                        }
                                    )
                                    return
                                except json.JSONDecodeError:
                                    continue

                        except Exception as e:
                            self.logger.debug(f"exception: {e}")
                            yield AIResponse(
                                content=response_handler.content,
                                error={
                                    "code": "stream_error",
                                    "message": f"Error processing stream: {str(e)}"
                                }
                            )
                            return

                        # Successfully processed response, exit retry loop
                        break

            except asyncio.TimeoutError:
                delay = self.base_delay * (2 ** attempt)
                self.logger.debug(f"Timeout on attempt {attempt + 1}/{self.max_retries}")
                if attempt < self.max_retries - 1:
                    yield AIResponse(
                        content="",
                        error={
                            "code": "timeout",
                            "message": f"Request timed out (attempt {attempt + 1}/{self.max_retries}). Retrying in {delay} seconds...",
                            "details": {"attempt": attempt + 1}
                        }
                    )
                    await asyncio.sleep(delay)
                    self.logger.debug(f"Retrying after timeout (attempt {attempt + 2}/{self.max_retries})")
                    continue

                yield AIResponse(
                    content="",
                    error={
                        "code": "timeout",
                        "message": f"Request timed out after {self.max_retries} attempts",
                        "details": {"attempt": attempt + 1}
                    }
                )
                return

            except aiohttp.ClientError as e:
                delay = self.base_delay * (2 ** attempt)
                self.logger.debug(f"Network error on attempt {attempt + 1}/{self.max_retries}: {str(e)}")
                if attempt < self.max_retries - 1:
                    yield AIResponse(
                        content="",
                        error={
                            "code": "network_error",
                            "message": f"Network error (attempt {attempt + 1}/{self.max_retries}): {str(e)}. Retrying in {delay} seconds...",
                            "details": {"type": type(e).__name__, "attempt": attempt + 1}
                        }
                    )
                    await asyncio.sleep(delay)
                    self.logger.debug(f"Retrying after network error (attempt {attempt + 2}/{self.max_retries})")
                    continue

                yield AIResponse(
                    content="",
                    error={
                        "code": "network_error",
                        "message": f"Network error after {self.max_retries} attempts: {str(e)}",
                        "details": {"type": type(e).__name__, "attempt": attempt + 1}
                    }
                )
                return

            except Exception as e:
                self.logger.debug(f"Unexpected error: {e}")
                yield AIResponse(
                    content="",
                    error={
                        "code": "error",
                        "message": f"Error: {str(e)}",
                        "details": {"type": type(e).__name__}
                    }
                )
                return
