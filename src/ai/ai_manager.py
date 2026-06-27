"""AI backend management singleton."""

import logging
from dataclasses import dataclass

from ai.ai_backend import AIBackend
from ai.ai_backend_settings import AIBackendSettings
from ai.anthropic.anthropic_backend import AnthropicBackend
from ai.deepseek.deepseek_backend import DeepseekBackend
from ai.google.google_backend import GoogleBackend
from ai.mistral.mistral_backend import MistralBackend
from ai.ollama.ollama_backend import OllamaBackend
from ai.openai.openai_backend import OpenAIBackend
from ai.vllm.vllm_backend import VLLMBackend
from ai.xai.xai_backend import XAIBackend
from ai.zai.zai_backend import ZaiBackend


@dataclass
class _BackendRegistration:
    """Registration entry for an AI backend."""
    backend_class: type[AIBackend]
    default_url: str


class AIManager:
    """
    Singleton manager for AI backends.

    Handles the creation and management of AI backend instances based on settings.
    Provides a clean interface for accessing AI backends without depending on user management.
    """

    _instance: 'AIManager | None' = None
    _logger = logging.getLogger("AIManager")

    _BACKEND_REGISTRY: dict[str, _BackendRegistration] = {
        "anthropic": _BackendRegistration(AnthropicBackend, "https://api.anthropic.com/v1/messages"),
        "deepseek": _BackendRegistration(DeepseekBackend, "https://api.deepseek.com/chat/completions"),
        "google": _BackendRegistration(GoogleBackend, "https://generativelanguage.googleapis.com/v1beta/models"),
        "mistral": _BackendRegistration(MistralBackend, "https://api.mistral.ai/v1/chat/completions"),
        "ollama": _BackendRegistration(OllamaBackend, "http://127.0.0.1:11434/api/chat"),
        "ollama-cloud": _BackendRegistration(OllamaBackend, "https://ollama.com/api/chat"),
        "openai": _BackendRegistration(OpenAIBackend, "https://api.openai.com/v1/chat/completions"),
        "vllm": _BackendRegistration(VLLMBackend, "http://localhost:8000/v1/chat/completions"),
        "xai": _BackendRegistration(XAIBackend, "https://api.x.ai/v1/chat/completions"),
        "zai": _BackendRegistration(ZaiBackend, "https://api.z.ai/api/paas/v4/chat/completions"),
    }

    def __new__(cls) -> 'AIManager':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialize the AIManager if not already initialized."""
        if not hasattr(self, '_initialized'):
            self._ai_backends: dict[str, AIBackend] = {}
            self._initialized = True

    def get_backends(self) -> dict[str, AIBackend]:
        """
        Get the current AI backends.

        Returns:
            Dictionary mapping provider names to backend instances
        """
        return self._ai_backends

    def get_default_url(self, provider: str) -> str:
        """
        Get the default API URL for a provider.

        Args:
            provider: The provider name

        Returns:
            The default URL for the specified provider, or empty string if not found
        """
        registration = self._BACKEND_REGISTRY.get(provider)
        if registration:
            return registration.default_url

        return ""

    def get_backend_class(self, provider: str) -> type[AIBackend] | None:
        """
        Get the backend class for a provider.

        Args:
            provider: The provider name

        Returns:
            The backend class for the specified provider, or None if not found
        """
        registration = self._BACKEND_REGISTRY.get(provider)
        if registration:
            return registration.backend_class

        return None

    def _create_backends(self, backend_settings: dict[str, AIBackendSettings]) -> dict[str, AIBackend]:
        """
        Create AI backends based on enabled backends and their settings.

        Args:
            backend_settings: Dictionary mapping provider names to their settings

        Returns:
            Dictionary mapping provider names to backend instances
        """
        backends: dict[str, AIBackend] = {}

        for provider, registration in self._BACKEND_REGISTRY.items():
            provider_settings = backend_settings.get(provider)

            if not provider_settings or not provider_settings.enabled:
                continue

            backends[provider] = registration.backend_class(
                api_key=provider_settings.api_key,
                api_url=provider_settings.url or registration.default_url
            )

        return backends

    def initialize_from_settings(self, ai_backend_settings: dict[str, AIBackendSettings]) -> None:
        """
        Initialize AI backends from settings.

        Args:
            ai_backend_settings: Dictionary mapping provider names to their settings
        """
        self._ai_backends = self._create_backends(ai_backend_settings)
        self._logger.info("Initialized AI backends from settings")

    def update_backend_settings(self, ai_backend_settings: dict[str, AIBackendSettings]) -> None:
        """
        Update AI backend settings and reinitialize backends.

        Args:
            ai_backend_settings: Dictionary mapping provider names to their updated settings
        """
        self._ai_backends = self._create_backends(ai_backend_settings)
        self._logger.info("Updated AI backends with new settings")
