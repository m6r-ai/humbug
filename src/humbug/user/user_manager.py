"""
Manages Humbug application user settings, primarily API keys.
"""

import json
import logging
import os
from typing import Dict, Optional

from PySide6.QtCore import QObject, Signal

from humbug.user.user_settings import UserSettings


class UserError(Exception):
    """Base exception for user operations."""


class UserManager(QObject):
    """
    Manages Humbug application user settings.
    
    Implements singleton pattern for global access to user settings.
    Handles loading/saving settings and merging with environment variables.
    """
    # Signal emitted when settings change
    settings_changed = Signal()

    USER_DIR = ".humbug"
    SETTINGS_FILE = "user-settings.json"
    API_KEYS_FILE = "api-keys.json"  # Legacy file, maintained for backward compatibility

    _instance = None
    _logger = logging.getLogger("UserManager")

    def __new__(cls):
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(UserManager, cls).__new__(cls)
        return cls._instance

    def __init__(self):
        """Initialize user manager if not already initialized."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._user_path = os.path.expanduser(f"~/{self.USER_DIR}")
            self._settings: Optional[UserSettings] = None
            self._load_settings()
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
                self._settings = UserSettings.load(legacy_path)
                self._logger.info("Loaded user settings from legacy file %s", legacy_path)
                # Save in new format for future use
                self._save_settings()
            else:
                # Create default settings
                self._settings = UserSettings.create_default()
                self._logger.info("Created default user settings")
                self._save_settings()

        except Exception as e:
            self._logger.error("Failed to load user settings: %s", str(e))
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

            # Also update the legacy file for backward compatibility
            legacy_path = self._get_legacy_api_keys_path()
            with open(legacy_path, 'w', encoding='utf-8') as f:
                json.dump(self._settings.api_keys, f, indent=4)
            os.chmod(legacy_path, 0o600)

        except OSError as e:
            self._logger.error("Failed to save user settings: %s", str(e))

    def update_api_keys(self, api_keys: Dict[str, str]) -> None:
        """
        Update API keys and save to file.
        
        Args:
            api_keys: Dictionary of API key name to value
            
        Raises:
            UserError: If API keys cannot be saved
        """
        if not self._settings:
            self._load_settings()

        try:
            self._settings.api_keys = api_keys
            self._save_settings()
            self.settings_changed.emit()
        except OSError as e:
            raise UserError(f"Failed to save API keys: {str(e)}") from e

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
