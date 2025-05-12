"""
Manages Humbug application user settings, primarily API keys.
"""

import logging
import os
from typing import Dict, cast

from PySide6.QtCore import QObject, Signal

from humbug.ai.ai_backend import AIBackend
from humbug.ai.ai_backend_settings import AIBackendSettings
from humbug.ai.ai_provider import AIProvider
from humbug.user.user_settings import UserSettings


class UserError(Exception):
    """Base exception for user operations."""


class UserManager(QObject):
    """
    Manages Humbug application user settings.

    Implements singleton pattern for global access to user settings.
    Handles loading/saving settings and merging with environment variables.
    """
    USER_DIR = ".humbug"
    SETTINGS_FILE = "user-settings.json"
    API_KEYS_FILE = "api-keys.json"  # Legacy file, maintained for backward compatibility

    # Signal emitted when user settings change
    settings_changed = Signal()

    _instance = None
    _logger = logging.getLogger("UserManager")

    def __new__(cls) -> 'UserManager':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(UserManager, cls).__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialize user manager if not already initialized."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._user_path = os.path.expanduser(f"~/{self.USER_DIR}")
            self._settings: UserSettings | None = None
            self._ai_backends: Dict[str, AIBackend] = {}
            self._load_settings()
            self._initialize_ai_backends()
            self._initialized = True

    def _get_settings_path(self) -> str:
        """Get path to user settings file."""
        return os.path.join(self._user_path, self.SETTINGS_FILE)

    def _get_legacy_api_keys_path(self) -> str:
        """Get path to legacy API keys file."""
        return os.path.join(self._user_path, self.API_KEYS_FILE)

    def _load_settings(self) -> None:
        """
        Load user settings from config files.

        First tries to load from the new settings file format.
        Falls back to legacy api-keys.json if needed.
        Creates default settings if files don't exist.
        """
        try:
            # Ensure user directory exists
            os.makedirs(self._user_path, mode=0o700, exist_ok=True)

            settings_path = self._get_settings_path()
            legacy_path = self._get_legacy_api_keys_path()

            # Try to load from new format first
            if os.path.exists(settings_path):
                self._settings = UserSettings.load(settings_path)
                self._logger.info("Loaded user settings from %s", settings_path)
                return

            # Fall back to legacy format
            if os.path.exists(legacy_path):
                self._settings = UserSettings.load_legacy(legacy_path)
                self._logger.info("Loaded user settings from legacy file %s", legacy_path)
                # Save in new format for future use
                self._save_settings()
                return

            # Create default settings
            self._settings = UserSettings.create_default()
            self._logger.info("Created default user settings")
            self._save_settings()

        except Exception:
            self._logger.exception("Failed to load user settings")
            # Create default settings as fallback
            self._settings = UserSettings.create_default()

    def _save_settings(self) -> None:
        """Save current settings to file."""
        if not self._settings:
            return

        try:
            settings_path = self._get_settings_path()
            self._settings.save(settings_path)
            self._logger.info("Saved user settings to %s", settings_path)

        except OSError as e:
            self._logger.error("Failed to save user settings: %s", str(e))

    def _initialize_ai_backends(self) -> None:
        """Initialize AI backends using current settings."""
        # Check environment variables and insert them where there's no saved setting
        env_keys = {
            "anthropic": os.environ.get("ANTHROPIC_API_KEY"),
            "deepseek": os.environ.get("DEEPSEEK_API_KEY"),
            "google": os.environ.get("GOOGLE_API_KEY"),
            "m6r": os.environ.get("M6R_API_KEY"),
            "mistral": os.environ.get("MISTRAL_API_KEY"),
            "openai": os.environ.get("OPENAI_API_KEY"),
            "xai": os.environ.get("XAI_API_KEY")
        }

        for backend_id, api_key in env_keys.items():
            settings = cast(UserSettings, self._settings)
            if api_key is not None and not settings.ai_backends[backend_id].enabled:
                settings.ai_backends[backend_id] = AIBackendSettings(
                    enabled=True,
                    api_key=api_key,
                    url=""
                )

        self._ai_backends = AIProvider.create_backends(settings.ai_backends)
        self._logger.info("Initialized AI backends with available settings")

    def update_settings(self, new_settings: UserSettings) -> None:
        """
        Update user settings, save to file, and refresh AI backends.

        Args:
            new_settings: UserSettings object with updated settings

        Raises:
            UserError: If settings cannot be saved
        """
        try:
            self._settings = new_settings
            self._save_settings()

            # Re-initialize backends with new settings
            self._initialize_ai_backends()

            # Emit signal to notify listeners
            self.settings_changed.emit()

        except OSError as e:
            raise UserError(f"Failed to save user settings: {str(e)}") from e

    def settings(self) -> UserSettings:
        """
        Get the current user settings.

        Returns:
            The current UserSettings object
        """
        return cast(UserSettings, self._settings)

    def get_ai_backends(self) -> Dict[str, AIBackend]:
        """
        Get the current AI backends.

        Returns:
            Dictionary mapping provider names to backend instances
        """
        return self._ai_backends
