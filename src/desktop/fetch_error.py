"""User-friendly, localised error messages for provider model-fetch and pull failures."""


from desktop.language.language_manager import LanguageManager
from http_client import ClientConnectorError, ClientResponseError, ServerTimeoutError

_OLLAMA_NOT_FOUND_HINTS = ("file does not exist", "not found", "no such")


def pull_error_message(exc: Exception) -> str:
    """Return a localised error string for an Ollama pull_model() failure."""
    strings = LanguageManager().strings()

    if isinstance(exc, ValueError):
        msg = str(exc).lower()
        if any(hint in msg for hint in _OLLAMA_NOT_FOUND_HINTS):
            return strings.ollama_pull_model_not_found

        return strings.ollama_pull_error.format(str(exc)[:80])

    if isinstance(exc, ClientConnectorError):
        return strings.ollama_pull_not_running

    if isinstance(exc, ClientResponseError):
        if exc.status() == 404:
            return strings.ollama_pull_model_not_found

        return strings.ollama_pull_error.format(f"HTTP {exc.status()}")

    if isinstance(exc, ServerTimeoutError):
        return strings.ollama_pull_error.format("timeout")

    return strings.ollama_pull_error.format(str(exc)[:80])


def fetch_error_message(exc: Exception, backend_id: str = "") -> str:
    """
    Return a localised one-line message for a fetch_models() exception.

    Args:
        exc: The exception raised by fetch_models().
        backend_id: Optional provider name (e.g. "ollama") for provider-specific wording.
    """
    strings = LanguageManager().strings()

    if isinstance(exc, ClientConnectorError):
        # Ollama runs locally — give the more actionable "is it running?" message.
        if backend_id == "ollama":
            return strings.ollama_pull_not_running

        return strings.fetch_error_connection

    if isinstance(exc, ClientResponseError):
        if exc.status() == 401:
            return strings.fetch_error_invalid_key

        if exc.status() == 403:
            return strings.fetch_error_access_denied.format(exc.status())

        if exc.status() == 404:
            return strings.fetch_error_not_found.format(exc.status())

        if exc.status() == 429:
            return strings.fetch_error_rate_limited.format(exc.status())

        if exc.status() >= 500:
            return strings.fetch_error_server_error.format(exc.status())

        return strings.fetch_error_generic.format(f"HTTP {exc.status()}: {exc.message()}")

    if isinstance(exc, ServerTimeoutError):
        return strings.fetch_error_timeout

    return strings.fetch_error_generic.format(str(exc)[:80])
