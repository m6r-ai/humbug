"""AI backend management singleton."""

import logging
from typing import Dict

from ai.ai_backend import AIBackend
from ai.ai_backend_settings import AIBackendSettings
from ai.ai_provider import AIProvider


class AIManager:
    """
    Singleton manager for AI backends.
    
    Handles the creation and management of AI backend instances based on settings.
    Provides a clean interface for accessing AI backends without depending on user management.
    """

    _instance: 'AIManager | None' = None
    _logger = logging.getLogger("AIManager")

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

    def initialize_from_settings(self, ai_backend_settings: Dict[str, AIBackendSettings]) -> None:
        """
        Initialize AI backends from settings.

        Args:
            ai_backend_settings: Dictionary mapping provider names to their settings
        """
        self._ai_backends = AIProvider.create_backends(ai_backend_settings)
        self._logger.info("Initialized AI backends from settings")

    def update_backend_settings(self, ai_backend_settings: Dict[str, AIBackendSettings]) -> None:
        """
        Update AI backend settings and reinitialize backends.

        Args:
            ai_backend_settings: Dictionary mapping provider names to their updated settings
        """
        self._ai_backends = AIProvider.create_backends(ai_backend_settings)
        self._logger.info("Updated AI backends with new settings")
