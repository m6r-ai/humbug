"""Provider selection and initialization for AI backends."""

from typing import Dict, Optional

from humbug.ai.ai_backend import AIBackend
from humbug.ai.anthropic_backend import AnthropicBackend
from humbug.ai.gemini_backend import GeminiBackend
from humbug.ai.m6r_backend import M6RBackend
from humbug.ai.ollama_backend import OllamaBackend
from humbug.ai.openai_backend import OpenAIBackend


class AIProvider:
    """Factory and manager for AI backends."""

    @staticmethod
    def create_backends(
        openai_key: Optional[str] = None,
        google_key: Optional[str] = None,
        anthropic_key: Optional[str] = None,
        m6r_key: Optional[str] = None
    ) -> Dict[str, AIBackend]:
        """Create AI backends based on available API keys.

        Args:
            openai_key: Optional OpenAI API key
            google_key: Optional Google API key
            anthropic_key: Optional Anthropic API key
            m6r_key: Optional M6R API key

        Returns:
            Dictionary mapping provider names to backend instances
        """
        backends = {}
        if openai_key:
            backends["openai"] = OpenAIBackend(openai_key)

        if google_key:
            backends["google"] = GeminiBackend(google_key)

        if anthropic_key:
            backends["anthropic"] = AnthropicBackend(anthropic_key)

        if m6r_key:
            backends["m6r"] = M6RBackend(m6r_key)

        backends["ollama"] = OllamaBackend()

        return backends
