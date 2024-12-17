"""Exceptions for AI-related operations."""


class NetworkError(Exception):
    """Network-related errors during AI operations.

    Raised when network operations fail during AI interactions, including:
    - Connection failures (aiohttp.ClientConnectionError)
    - Timeouts (asyncio.TimeoutError)
    - DNS failures (aiohttp.ClientConnectorError)
    """


class APIError(Exception):
    """API-specific errors from the AI service.

    Raised when the AI service returns an error, including:
    - Non-200 status codes
    - Error responses in the body
    - Rate limit exceeded responses
    """


class StreamError(Exception):
    """Errors during stream processing.

    Raised when handling streaming responses fails, including:
    - JSON parsing failures (json.JSONDecodeError)
    - Invalid text encoding (UnicodeDecodeError)
    """
