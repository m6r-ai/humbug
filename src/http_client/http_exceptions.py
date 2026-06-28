"""Exception hierarchy for the async HTTP client."""


class HttpClientError(Exception):
    """Base exception for all HTTP client errors."""


class ClientConnectorError(HttpClientError):
    """Raised when a connection cannot be established (DNS failure, refused, etc.)."""


class ClientResponseError(HttpClientError):
    """Raised when the server returns an HTTP error status (>= 400)."""

    def __init__(self, status: int, message: str) -> None:
        """
        Initialize the response error.

        Args:
            status: HTTP status code
            message: Error message from the response or a description
        """
        self._status = status
        self._message = message
        super().__init__(f"HTTP {status}: {message}")

    def status(self) -> int:
        """Return the HTTP status code."""
        return self._status

    def message(self) -> str:
        """Return the error message."""
        return self._message


class ServerTimeoutError(HttpClientError):
    """Raised when a connect or read timeout occurs."""
