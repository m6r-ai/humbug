"""
Manages Humbug application user settings, primarily API keys.
"""

import logging
import os
from typing import Dict, Optional

from PySide6.QtCore import QObject, Signal

from humbug.ai.ai_backend import AIBackend
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

    def __new__(cls):
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(UserManager, cls).__new__(cls)
        return cls._instance

    def __init__(self) -> None:
        """Initialize user manager if not already initialized."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._user_path = os.path.expanduser(f"~/{self.USER_DIR}")
            self._settings: Optional[UserSettings] = None
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
            # Fall back to legacy format
            elif os.path.exists(legacy_path):
                self._settings = UserSettings.load_legacy(legacy_path)
                self._logger.info("Loaded user settings from legacy file %s", legacy_path)
                # Save in new format for future use
                self._save_settings()
            else:
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
        """Initialize AI backends using current API keys."""
        api_keys = self.get_api_keys()

        self._ai_backends = AIProvider.create_backends(
            anthropic_key=api_keys.get("ANTHROPIC_API_KEY"),
            deepseek_key=api_keys.get("DEEPSEEK_API_KEY"),
            google_key=api_keys.get("GOOGLE_API_KEY"),
            m6r_key=api_keys.get("M6R_API_KEY"),
            mistral_key=api_keys.get("MISTRAL_API_KEY"),
            openai_key=api_keys.get("OPENAI_API_KEY")
        )

        self._logger.info("Initialized AI backends with available API keys")

    def update_settings(self, new_settings: UserSettings) -> None:
        """
        Update user settings, save to file, and refresh AI backends.

        Args:
            new_settings: UserSettings object with updated settings

        Raises:
            UserError: If settings cannot be saved
        """
        if not self._settings:
            self._load_settings()

        try:
            self._settings = new_settings
            self._save_settings()

            # Re-initialize backends with new API keys
            self._initialize_ai_backends()

            # Emit signal to notify listeners
            self.settings_changed.emit()

        except OSError as e:
            raise UserError(f"Failed to save user settings: {str(e)}") from e

    @property
    def settings(self) -> UserSettings:
        """
        Get the current user settings.

        Returns:
            The current UserSettings object
        """
        if not self._settings:
            self._load_settings()
        return self._settings

    def get_api_keys(self, include_env_vars: bool = True) -> Dict[str, str]:
        """
        Get current API keys, optionally including environment variables.

        Environment variables take precedence over stored keys when included.

        Args:
            include_env_vars: Whether to include environment variables

        Returns:
            Dictionary of API keys
        """
        if not self._settings:
            self._load_settings()

        api_keys = self._settings.api_keys.copy()

        if include_env_vars:
            # Check environment variables and override stored keys if present
            env_keys = {
                "ANTHROPIC_API_KEY": os.environ.get("ANTHROPIC_API_KEY"),
                "DEEPSEEK_API_KEY": os.environ.get("DEEPSEEK_API_KEY"),
                "GOOGLE_API_KEY": os.environ.get("GOOGLE_API_KEY"),
                "M6R_API_KEY": os.environ.get("M6R_API_KEY"),
                "MISTRAL_API_KEY": os.environ.get("MISTRAL_API_KEY"),
                "OPENAI_API_KEY": os.environ.get("OPENAI_API_KEY")
            }

            # Only override if environment variable is not None
            for key, value in env_keys.items():
                if value is not None:
                    api_keys[key] = value

        return api_keys

    def get_ai_backends(self) -> Dict[str, AIBackend]:
        """
        Get the current AI backends.

        Returns:
            Dictionary mapping provider names to backend instances
        """
        if not self._ai_backends:
            self._initialize_ai_backends()

        return self._ai_backends
