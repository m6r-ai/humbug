"""Tests for the friendly error-message mappers used by fetch, pull, and streaming failures."""

from desktop.fetch_error import stream_error_message
from desktop.language.language_manager import LanguageManager


class TestStreamErrorMessage:
    def test_none_returns_empty_string(self):
        assert stream_error_message(None) == ""

    def test_cancelled_is_friendly(self):
        error = {"code": "cancelled", "message": "Server failed to complete response"}
        assert stream_error_message(error) == LanguageManager().strings().stream_error_interrupted

    def test_network_error_reuses_connection_message(self):
        error = {"code": "network_error", "message": "Network error: [Errno 61] Connection refused"}
        assert stream_error_message(error) == (
            f"{LanguageManager().strings().fetch_error_connection} "
            "(Network error: [Errno 61] Connection refused)"
        )

    def test_backend_error_is_actionable(self):
        error = {"code": "backend_error", "message": "No backend available for provider: foo"}
        assert stream_error_message(error) == LanguageManager().strings().stream_error_no_backend

    def test_process_error_truncates_raw_exception_text(self):
        error = {"code": "process_error", "message": "boom", "details": {"type": "ValueError"}}
        assert stream_error_message(error) == LanguageManager().strings().fetch_error_generic.format("boom")

    def test_http_401_maps_to_invalid_key(self):
        error = {"code": "401", "message": "API error 401: {...}"}
        assert stream_error_message(error) == f"{LanguageManager().strings().fetch_error_invalid_key} (401)"

    def test_http_403_uses_streaming_specific_wording_not_fetch_wording(self):
        error = {"code": "403", "message": "API error 403: {...}"}
        result = stream_error_message(error)
        assert result == LanguageManager().strings().stream_error_access_denied.format(403)
        assert "model-list" not in result

    def test_http_404_maps_to_not_found(self):
        error = {"code": "404", "message": "API error 404: {...}"}
        assert stream_error_message(error) == LanguageManager().strings().fetch_error_not_found.format(404)

    def test_http_429_maps_to_rate_limited(self):
        error = {"code": "429", "message": "API error 429: {...}"}
        assert stream_error_message(error) == LanguageManager().strings().fetch_error_rate_limited.format(429)

    def test_http_5xx_maps_to_server_error(self):
        error = {"code": "529", "message": "API error 529: {...}"}
        assert stream_error_message(error) == LanguageManager().strings().fetch_error_server_error.format(529)

    def test_unmapped_http_status_falls_back_to_generic(self):
        error = {"code": "418", "message": "API error 418: {...}"}
        assert stream_error_message(error) == LanguageManager().strings().fetch_error_generic.format("HTTP 418")

    def test_non_numeric_unknown_code_falls_back_to_generic_with_message(self):
        error = {"code": "stream_error", "message": "Unknown error"}
        assert stream_error_message(error) == LanguageManager().strings().fetch_error_generic.format("Unknown error")

    def test_raw_technical_text_is_never_the_displayed_string(self):
        """The whole point: the raw dump must not leak into the friendly text."""
        raw = "API error 401: {'error': {'type': 'authentication_error', 'message': 'invalid x-api-key'}}"
        error = {"code": "401", "message": raw, "details": {"error": {"message": "invalid x-api-key"}}}
        assert stream_error_message(error) != raw
