"""Provider selection and initialization for AI backends."""

from typing import Dict

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_backend_settings import AIBackendSettings
from humbug.ai.anthropic.anthropic_backend import AnthropicBackend
from humbug.ai.deepseek.deepseek_backend import DeepseekBackend
from humbug.ai.google.google_backend import GoogleBackend
from humbug.ai.m6r.m6r_backend import M6RBackend
from humbug.ai.mistral.mistral_backend import MistralBackend
from humbug.ai.ollama.ollama_backend import OllamaBackend
from humbug.ai.openai.openai_backend import OpenAIBackend
from humbug.ai.xai.xai_backend import XAIBackend


class AIProvider:
    """Factory and manager for AI backends."""

    @staticmethod
    def create_backends(backend_settings: Dict[str, AIBackendSettings]) -> Dict[str, AIBackend]:
        """Create AI backends based on enabled backends and their settings.

        Args:
            backend_settings: Dictionary mapping provider names to their settings

        Returns:
            Dictionary mapping provider names to backend instances
        """
        backends: Dict[str, AIBackend] = {}

        # Check if Anthropic is enabled
        anthropic_settings = backend_settings.get("anthropic")
        if anthropic_settings and anthropic_settings.enabled:
            backends["anthropic"] = AnthropicBackend(
                api_key=anthropic_settings.api_key,
                base_url=anthropic_settings.url if anthropic_settings.url else None
            )

        # Check if DeepSeek is enabled
        deepseek_settings = backend_settings.get("deepseek")
        if deepseek_settings and deepseek_settings.enabled:
            backends["deepseek"] = DeepseekBackend(
                api_key=deepseek_settings.api_key,
                base_url=deepseek_settings.url if deepseek_settings.url else None
            )

        # Check if Google is enabled
        google_settings = backend_settings.get("google")
        if google_settings and google_settings.enabled:
            backends["google"] = GoogleBackend(
                api_key=google_settings.api_key,
                base_url=google_settings.url if google_settings.url else None
            )

        # Check if M6R is enabled
        m6r_settings = backend_settings.get("m6r")
        if m6r_settings and m6r_settings.enabled:
            backends["m6r"] = M6RBackend(
                api_key=m6r_settings.api_key,
                base_url=m6r_settings.url if m6r_settings.url else None
            )

        # Check if Mistral is enabled
        mistral_settings = backend_settings.get("mistral")
        if mistral_settings and mistral_settings.enabled:
            backends["mistral"] = MistralBackend(
                api_key=mistral_settings.api_key,
                base_url=mistral_settings.url if mistral_settings.url else None
            )

        # Check if OpenAI is enabled
        openai_settings = backend_settings.get("openai")
        if openai_settings and openai_settings.enabled:
            backends["openai"] = OpenAIBackend(
                api_key=openai_settings.api_key,
                base_url=openai_settings.url if openai_settings.url else None
            )

        # Check if xAI is enabled
        xai_settings = backend_settings.get("xai")
        if xai_settings and xai_settings.enabled:
            backends["xai"] = XAIBackend(
                api_key=xai_settings.api_key,
                base_url=xai_settings.url if xai_settings.url else None
            )

        # Ollama is special - it's a local service
        ollama_settings = backend_settings.get("ollama")
        if ollama_settings and ollama_settings.enabled:
            backends["ollama"] = OllamaBackend(
                base_url=ollama_settings.url if ollama_settings.url else None
            )

        return backends
