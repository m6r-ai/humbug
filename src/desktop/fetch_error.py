"""User-friendly, localised error messages for provider model-fetch, pull, and streaming failures."""


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
        return f"{strings.ollama_pull_not_running} ({str(exc)[:80]})"

    if isinstance(exc, ClientResponseError):
        if exc.status() == 404:
            return f"{strings.ollama_pull_model_not_found} ({exc.status()})"

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
            return f"{strings.ollama_pull_not_running} ({str(exc)[:80]})"

        return f"{strings.fetch_error_connection} ({str(exc)[:80]})"

    if isinstance(exc, ClientResponseError):
        if exc.status() == 401:
            return f"{strings.fetch_error_invalid_key} ({exc.status()})"

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


def stream_error_message(error: dict | None) -> str:
    """
    Return a localised, human-friendly one-line message for an AI streaming error.

    ``error`` is the raw {"code", "message", "details"} dict AIConversation attaches
    to a SYSTEM error message. This only controls what's rendered in the chat bubble —
    the original technical message stays untouched on the message and in the saved
    transcript, so it's still available for logs or support.
    """
    strings = LanguageManager().strings()

    if error is None:
        return ""

    code = error.get("code", "")
    message = error.get("message", "")

    if code == "cancelled":
        return strings.stream_error_interrupted

    if code == "network_error":
        return f"{strings.fetch_error_connection} ({message[:80]})"

    if code == "backend_error":
        return strings.stream_error_no_backend

    if code == "process_error":
        return strings.fetch_error_generic.format(message[:80])

    try:
        status = int(code)

    except (TypeError, ValueError):
        return strings.fetch_error_generic.format(message[:80])

    if status == 401:
        return f"{strings.fetch_error_invalid_key} ({status})"

    if status == 403:
        return strings.stream_error_access_denied.format(status)

    if status == 404:
        return strings.fetch_error_not_found.format(status)

    if status == 429:
        return strings.fetch_error_rate_limited.format(status)

    if status >= 500:
        return strings.fetch_error_server_error.format(status)

    return strings.fetch_error_generic.format(f"HTTP {status}")
