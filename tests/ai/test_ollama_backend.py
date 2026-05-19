"""Tests for Ollama backend URL handling."""

from ai.ollama.ollama_backend import OllamaBackend


def test_ollama_endpoint_urls_from_default_api_url() -> None:
    """Default chat endpoint should normalize to other Ollama API endpoints."""
    backend = OllamaBackend(api_key="", api_url=None)

    assert backend._get_api_endpoint_url("tags") == "http://localhost:11434/api/tags"
    assert backend._get_api_endpoint_url("pull") == "http://localhost:11434/api/pull"


def test_ollama_endpoint_urls_from_base_url() -> None:
    """Base Ollama URLs should work for fetch and pull."""
    backend = OllamaBackend(api_key="", api_url="http://localhost:11434")

    assert backend._get_api_endpoint_url("tags") == "http://localhost:11434/api/tags"
    assert backend._get_api_endpoint_url("pull") == "http://localhost:11434/api/pull"


def test_ollama_endpoint_urls_from_trailing_slash_base_url() -> None:
    """Trailing slashes should not produce malformed endpoint URLs."""
    backend = OllamaBackend(api_key="", api_url="http://localhost:11434/")

    assert backend._get_api_endpoint_url("tags") == "http://localhost:11434/api/tags"


def test_ollama_endpoint_urls_from_custom_api_url() -> None:
    """Full custom API URLs should keep their host and prefix."""
    backend = OllamaBackend(api_key="", api_url="http://example.test/ollama/api/chat")

    assert backend._get_api_endpoint_url("tags") == "http://example.test/ollama/api/tags"
    assert backend._get_api_endpoint_url("pull") == "http://example.test/ollama/api/pull"
