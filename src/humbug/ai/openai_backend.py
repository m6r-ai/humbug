"""OpenAI backend implementation."""

import asyncio
import json
import logging
from typing import AsyncGenerator, List, Dict

import aiohttp

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_response import AIResponse
from humbug.ai.conversation_settings import ConversationSettings
from humbug.ai.openai_stream_response import OpenAIStreamResponse
from humbug.ai.rate_limiter import RateLimiter


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
            "stream": True,
            "stream_options": {"include_usage": True}
        }

        # Only include temperature if supported by model
        if ConversationSettings.supports_temperature(settings.model):
            data["temperature"] = settings.temperature

        self.logger.debug("stream message %s", data)

        try:
            for attempt in range(self.max_retries):
                try:
                    await self.rate_limiter.acquire()

                    post_timeout = aiohttp.ClientTimeout(
                        total=None,
                        sock_connect=20,
                        sock_read=10
                    )
                    async with aiohttp.ClientSession() as session:
                        async with session.post(
                            self.api_url,
                            headers={
                                "Content-Type": "application/json",
                                "Authorization": f"Bearer {self.api_key}"
                            },
                            json=data,
                            timeout=post_timeout
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
                                        self.logger.exception("CancelledError")
                                        yield AIResponse(
                                            content=response_handler.content,
                                            error={
                                                "code": "cancelled",
                                                "message": "Request cancelled by user"
                                            }
                                        )
                                        return

                                    except json.JSONDecodeError as e:
                                        self.logger.exception("JSON exception: %s", e)
                                        continue

                                    except Exception as e:
                                        self.logger.exception("unexpected exception: %s", e)
                                        break

                                # Successfully processed response, exit retry loop
                                break

                            except asyncio.TimeoutError:
                                delay = self.base_delay * (2 ** attempt)
                                self.logger.debug("Timeout on attempt %d/%d", attempt + 1, self.max_retries)
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
                                    self.logger.debug("Retrying after timeout (attempt %d/%d)", attempt + 2, self.max_retries)
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
                                self.logger.debug("Network error on attempt %d/%d: %s", attempt + 1, self.max_retries, str(e))
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
                                    self.logger.debug("Retrying after network error (attempt %d/%d)", attempt + 2, self.max_retries)
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
                    self.logger.debug("Unexpected error: %s", str(e))
                    yield AIResponse(
                        content="",
                        error={
                            "code": "error",
                            "message": f"Error: {str(e)}",
                            "details": {"type": type(e).__name__}
                        }
                    )
                    return

        except (GeneratorExit, asyncio.CancelledError):
            self.logger.debug("Stream cancelled or generator closed")
            return
