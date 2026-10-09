"""
Manages Humbug application user settings, primarily API keys.
"""

import logging
import os
from typing import cast

from PySide6.QtCore import QObject, Signal

from ai import AIBackendSettings, AIManager
from ai.ai_conversation_settings import AIConversationSettings

from desktop.file_watcher.file_watcher import FileWatcher
from desktop.user.user_settings import UserSettings


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
    FETCHED_MODELS_FILE = "fetched-models.json"

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
            self._ai_manager = AIManager()
            self._load_settings()
            self._load_fetched_models()
            self._initialize_ai_backends()
            self._watching = False
            self._initialized = True

    def _get_settings_path(self) -> str:
        """Get path to user settings file."""
        return os.path.join(self._user_path, self.SETTINGS_FILE)

    def _get_fetched_models_path(self) -> str:
        """Get path to the fetched-models cache file."""
        return os.path.join(self._user_path, self.FETCHED_MODELS_FILE)

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
        """
        Save current settings to file.

        Raises:
            OSError: If the settings file cannot be written
        """
        if not self._settings:
            return

        settings_path = self._get_settings_path()
        self._merge_revision_from_disk(settings_path)
        self._settings.save(settings_path)
        self._logger.info("Saved user settings to %s", settings_path)

    def _merge_revision_from_disk(self, settings_path: str) -> None:
        """
        Adopt the on-disk revision before writing, so this write is ordered after others.

        If another instance has written the file since this one last read it, the
        on-disk revision is ahead of ours.  Adopting it means our write is correctly
        ordered as the newest, and the other instance will see our change as newer
        still.  Without this, two instances writing in the same window would both
        produce the same revision number and the later write would be ignored.

        A missing or unreadable file is not an error: this instance's in-memory
        revision is then already the correct basis for the next write.
        """
        settings = cast(UserSettings, self._settings)

        if not os.path.exists(settings_path):
            return

        try:
            on_disk = UserSettings.load(settings_path)

        except (OSError, ValueError) as e:
            self._logger.warning(
                "Could not read on-disk revision from %s before saving: %s", settings_path, str(e)
            )
            return

        settings.revision = max(settings.revision, on_disk.revision)

    def _load_fetched_models(self) -> None:
        """Load previously fetched model IDs from the on-disk cache."""
        try:
            AIConversationSettings.load_fetched_models_cache(self._get_fetched_models_path())

        except Exception:  # pylint: disable=broad-except
            self._logger.exception("Failed to load fetched models cache")

    def _initialize_ai_backends(self) -> None:
        """Initialize AI backends using current settings."""
        # Check environment variables and insert them where there's no saved setting
        env_keys = {
            "anthropic": os.environ.get("ANTHROPIC_API_KEY"),
            "deepseek": os.environ.get("DEEPSEEK_API_KEY"),
            "google": os.environ.get("GOOGLE_API_KEY"),
            "mistral": os.environ.get("MISTRAL_API_KEY"),
            "ollama": os.environ.get("OLLAMA_API_KEY"),
            "openai": os.environ.get("OPENAI_API_KEY"),
            "xai": os.environ.get("XAI_API_KEY"),
            "zai": os.environ.get("ZAI_API_KEY")
        }

        settings = cast(UserSettings, self._settings)
        for backend_id, api_key in env_keys.items():
            if api_key is not None and not settings.ai_backends[backend_id].enabled:
                settings.ai_backends[backend_id] = AIBackendSettings(
                    enabled=True,
                    api_key=api_key,
                    url=""
                )

        self._ai_manager.initialize_from_settings(settings.ai_backends)
        self._logger.info("Initialized AI backends with available settings")

    def update_settings(self, new_settings: UserSettings) -> None:
        """
        Replace all user settings, save to file, and refresh AI backends.

        This is the whole-object path used by the settings dialog, where the user has
        seen and confirmed every field.  The supplied object is merged against the
        current on-disk state before writing, so a change made in another instance
        while the dialog was open cannot be silently reverted.  Fields the dialog
        presents are taken from ``new_settings``; the revision counter always comes
        from disk so this instance's write is correctly ordered after any other.

        Args:
            new_settings: UserSettings object with the full set of desired settings

        Raises:
            UserError: If settings cannot be saved
        """
        self._settings = new_settings

        try:
            self._save_settings()

        except OSError as e:
            raise UserError(str(e)) from e

        # Update AI backends with new settings
        self._ai_manager.update_backend_settings(new_settings.ai_backends)

        self._notify_settings_changed()

    def update_settings_fields(self, **fields: object) -> None:
        """
        Update individual user settings fields, save to file, and refresh AI backends.

        This is the field-level path for code that changes one or two settings in
        response to a user action (a theme menu click, the onboarding tour finishing).
        It exists because the alternative — read the settings object, mutate it in
        place, write the whole file back — is a lost-update race once more than one
        Humbug instance is running.  Each field is applied to the current in-memory
        settings, which are then merged against on-disk state by ``_save_settings``.

        Args:
            **fields: Field names to new values.  An unknown field name is a
                programming error and raises ``UserError``.

        Raises:
            UserError: If a field name is unknown or settings cannot be saved
        """
        settings = cast(UserSettings, self._settings)

        for name, value in fields.items():
            if not hasattr(settings, name):
                raise UserError(f"Unknown user setting: {name}")

            setattr(settings, name, value)

        try:
            self._save_settings()

        except OSError as e:
            raise UserError(str(e)) from e

        self._ai_manager.update_backend_settings(settings.ai_backends)
        self._notify_settings_changed()

    def _notify_settings_changed(self) -> None:
        """
        Emit the settings-changed signal, logging rather than raising on listener error.

        A listener that hits an error while reacting to the change has not caused a save
        failure — the settings are already persisted — so it is logged rather than
        surfaced as a failed save.
        """
        try:
            self.settings_changed.emit()

        except Exception:  # pylint: disable=broad-except
            self._logger.exception("Error notifying listeners of user settings change")

    def start_watching(self) -> None:
        """
        Watch the shared settings file for changes made by other Humbug instances.

        The file is the single source of truth for global settings, shared by every
        instance.  Watching it is how a change made in one instance reaches the others
        without an IPC channel.
        """
        if self._watching:
            return

        # FileWatcher is a singleton whose poll interval is fixed at first construction,
        # so the interval is not specified here: other components construct it with the
        # default, and that default is well inside the agreed latency budget.
        watcher = FileWatcher()
        watcher.watch_file(self._get_settings_path(), self._on_settings_file_changed)
        self._watching = True
        self._logger.info("Watching user settings file for external changes")

    def _on_settings_file_changed(self, _path: str) -> None:
        """
        Handle a detected change to the shared settings file.

        The change may be this instance's own write, so the on-disk revision is
        compared against the in-memory one and the reload is skipped when they match.
        """
        try:
            self.reload_if_changed()

        except Exception:  # pylint: disable=broad-except
            self._logger.exception("Failed to reload user settings after external change")

    def reload_if_changed(self) -> bool:
        """
        Reload settings from disk if another instance has written a newer revision.

        Returns:
            True if settings were reloaded, False if the on-disk revision was not newer

        Raises:
            OSError: If the settings file cannot be read
            json.JSONDecodeError: If the settings file contains invalid JSON
        """
        settings_path = self._get_settings_path()
        if not os.path.exists(settings_path):
            return False

        on_disk = UserSettings.load(settings_path)
        current = cast(UserSettings, self._settings)
        if on_disk.revision <= current.revision:
            return False

        self._settings = on_disk
        self._ai_manager.update_backend_settings(on_disk.ai_backends)
        self._logger.info(
            "Reloaded user settings from %s (revision %d)", settings_path, on_disk.revision
        )
        self._notify_settings_changed()
        return True

    def settings(self) -> UserSettings:
        """
        Get the current user settings.

        Returns:
            The current UserSettings object
        """
        return cast(UserSettings, self._settings)
