"""User-friendly, localised error messages for provider model-fetch failures."""

import aiohttp

from humbug.language.language_manager import LanguageManager


def fetch_error_message(exc: Exception) -> str:
    """Return a localised one-line message for a fetch_models() exception."""
    strings = LanguageManager().strings()

    if isinstance(exc, aiohttp.ClientResponseError):
        if exc.status == 401:
            return strings.fetch_error_invalid_key
        if exc.status == 403:
            return strings.fetch_error_access_denied.format(exc.status)
        if exc.status == 404:
            return strings.fetch_error_not_found.format(exc.status)
        if exc.status == 429:
            return strings.fetch_error_rate_limited.format(exc.status)
        if exc.status >= 500:
            return strings.fetch_error_server_error.format(exc.status)
        return strings.fetch_error_generic.format(f"HTTP {exc.status}: {exc.message}")

    if isinstance(exc, aiohttp.ClientConnectorError):
        return strings.fetch_error_connection

    if isinstance(exc, aiohttp.ServerTimeoutError):
        return strings.fetch_error_timeout

    return strings.fetch_error_generic.format(str(exc)[:80])
