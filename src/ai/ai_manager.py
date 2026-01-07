"""AI backend management singleton."""

import logging
from typing import Dict, Type

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


class AIManager:
    """
    Singleton manager for AI backends.

    Handles the creation and management of AI backend instances based on settings.
    Provides a clean interface for accessing AI backends without depending on user management.
    """

    _instance: 'AIManager | None' = None
    _logger = logging.getLogger("AIManager")

    # Mapping of provider names to backend classes
    _BACKEND_CLASSES: Dict[str, Type[AIBackend]] = {
        "anthropic": AnthropicBackend,
        "deepseek": DeepseekBackend,
        "google": GoogleBackend,
        "mistral": MistralBackend,
        "ollama": OllamaBackend,
        "openai": OpenAIBackend,
        "vllm": VLLMBackend,
        "xai": XAIBackend,
        "zai": ZaiBackend
    }

    def __new__(cls) -> 'AIManager':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialize the AIManager if not already initialized."""
        if not hasattr(self, '_initialized'):
            self._ai_backends: Dict[str, AIBackend] = {}
            self._initialized = True

    def get_backends(self) -> Dict[str, AIBackend]:
        """
        Get the current AI backends.

        Returns:
            Dictionary mapping provider names to backend instances
        """
        return self._ai_backends

    def get_default_url(self, provider: str) -> str:
        """
        Get the default API URL for a provider by asking the backend class.

        Args:
            provider: The provider name

        Returns:
            The default URL for the specified provider, or empty string if not found
        """
        backend_class = self._BACKEND_CLASSES.get(provider)
        if backend_class:
            return backend_class.get_default_url()

        return ""

    def _create_backends(self, backend_settings: Dict[str, AIBackendSettings]) -> Dict[str, AIBackend]:
        """
        Create AI backends based on enabled backends and their settings.

        Args:
            backend_settings: Dictionary mapping provider names to their settings

        Returns:
            Dictionary mapping provider names to backend instances
        """
        backends: Dict[str, AIBackend] = {}

        # Iterate through all provider names
        for provider, backend_class in self._BACKEND_CLASSES.items():
            # Get settings for this provider
            provider_settings = backend_settings.get(provider)

            # Skip if settings don't exist or backend is disabled
            if not provider_settings or not provider_settings.enabled:
                continue

            # Create backend instance with consistent parameter passing
            backends[provider] = backend_class(
                api_key=provider_settings.api_key,
                api_url=provider_settings.url if provider_settings.url else None
            )

        return backends

    def initialize_from_settings(self, ai_backend_settings: Dict[str, AIBackendSettings]) -> None:
        """
        Initialize AI backends from settings.

        Args:
            ai_backend_settings: Dictionary mapping provider names to their settings
        """
        self._ai_backends = self._create_backends(ai_backend_settings)
        self._logger.info("Initialized AI backends from settings")

    def update_backend_settings(self, ai_backend_settings: Dict[str, AIBackendSettings]) -> None:
        """
        Update AI backend settings and reinitialize backends.

        Args:
            ai_backend_settings: Dictionary mapping provider names to their updated settings
        """
        self._ai_backends = self._create_backends(ai_backend_settings)
        self._logger.info("Updated AI backends with new settings")
