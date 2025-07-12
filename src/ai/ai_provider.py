"""Provider selection and initialization for AI backends."""

from typing import Dict, Type

from ai.ai_backend import AIBackend
from ai.ai_backend_settings import AIBackendSettings
from ai.anthropic.anthropic_backend import AnthropicBackend
from ai.deepseek.deepseek_backend import DeepseekBackend
from ai.google.google_backend import GoogleBackend
from ai.m6r.m6r_backend import M6RBackend
from ai.mistral.mistral_backend import MistralBackend
from ai.ollama.ollama_backend import OllamaBackend
from ai.openai.openai_backend import OpenAIBackend
from ai.xai.xai_backend import XAIBackend


class AIProvider:
    """Factory and manager for AI backends."""

    # Mapping of provider names to backend classes
    _BACKEND_CLASSES: Dict[str, Type[AIBackend]] = {
        "anthropic": AnthropicBackend,
        "deepseek": DeepseekBackend,
        "google": GoogleBackend,
        "m6r": M6RBackend,
        "mistral": MistralBackend,
        "ollama": OllamaBackend,
        "openai": OpenAIBackend,
        "xai": XAIBackend
    }

    @staticmethod
    def get_default_url(provider: str) -> str:
        """
        Get the default API URL for a provider by asking the backend class.

        Args:
            provider: The provider name

        Returns:
            The default URL for the specified provider, or empty string if not found
        """
        backend_class = AIProvider._BACKEND_CLASSES.get(provider)
        if backend_class:
            return backend_class.get_default_url()

        return ""

    @staticmethod
    def create_backends(backend_settings: Dict[str, AIBackendSettings]) -> Dict[str, AIBackend]:
        """
        Create AI backends based on enabled backends and their settings.

        Args:
            backend_settings: Dictionary mapping provider names to their settings

        Returns:
            Dictionary mapping provider names to backend instances
        """
        backends: Dict[str, AIBackend] = {}

        # Iterate through all provider names
        for provider, backend_class in AIProvider._BACKEND_CLASSES.items():
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
