"""
Manages Humbug application mindspaces.

This module provides functionality for creating, opening, and managing Humbug mindspaces.
A mindspace contains project-specific settings, recent files, and conversation history.
"""

import json
import logging
import os
import shutil
from typing import Dict, List

from PySide6.QtCore import QObject, Signal

from humbug.mindspace.system.system_interactions import SystemInteractions
from humbug.mindspace.system.system_message import SystemMessage
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.mindspace.mindspace_directory_tracker import MindspaceDirectoryTracker
from humbug.mindspace.mindspace_error import MindspaceError, MindspaceExistsError, MindspaceNotFoundError
from humbug.mindspace.mindspace_settings import MindspaceSettings


class MindspaceManager(QObject):
    """
    Manages Humbug application mindspaces.

    A mindspace is a directory containing project-specific settings and data. Each mindspace
    has a .humbug subdirectory containing configuration files and conversation history.

    Implements singleton pattern to provide global access to mindspace state.

    Attributes:
        MINDSPACE_DIR: Name of the mindspace configuration directory
        SETTINGS_FILE: Name of the mindspace settings file
        SESSION_FILE: Name of the file storing recent tabs
    """

    # Signal emitted when mindspace settings change
    settings_changed = Signal()

    # Signal emitted when system interactions are updated
    system_interactions_updated = Signal()

    MINDSPACE_DIR = ".humbug"
    SETTINGS_FILE = "settings.json"
    SESSION_FILE = "session.json"
    SYSTEM_INTERACTIONS_FILE = "system.json"

    _instance = None
    _logger = logging.getLogger("MindspaceManager")

    def __new__(cls) -> 'MindspaceManager':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(MindspaceManager, cls).__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialize mindspace manager if not already initialized."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._mindspace_path: str = ""
            self._settings: MindspaceSettings | None = None
            self._home_config = os.path.expanduser("~/.humbug/mindspace.json")
            self._directory_tracker = MindspaceDirectoryTracker()
            self._system_interactions = SystemInteractions()
            self._initialized = True

    def mindspace_path(self) -> str:
        """
        Get the current mindspace path.

        Returns:
            The absolute path to the current mindspace, or None if no mindspace is open.
        """
        return self._mindspace_path

    def settings(self) -> MindspaceSettings | None:
        """
        Get the current mindspace settings.

        Returns:
            The MindspaceSettings object for the current mindspace, or None if no mindspace is open.
        """
        return self._settings

    def update_settings(self, new_settings: MindspaceSettings) -> None:
        """
        Update mindspace settings and notify listeners.

        Args:
            new_settings: New settings to apply

        Raises:
            MindspaceError: If settings cannot be saved
            MindspaceNotFoundError: If no mindspace is open
        """
        if not self.has_mindspace():
            raise MindspaceNotFoundError("No mindspace is currently open")

        # Save settings to file
        settings_path = os.path.join(
            self._mindspace_path,
            self.MINDSPACE_DIR,
            self.SETTINGS_FILE
        )
        try:
            new_settings.save(settings_path)
            self._settings = new_settings
            self.settings_changed.emit()

        except OSError as e:
            raise MindspaceError(f"Failed to save mindspace settings: {str(e)}") from e

    def has_mindspace(self) -> bool:
        """
        Check if a mindspace is currently open.

        Returns:
            True if a mindspace is open, False otherwise.
        """
        return bool(self._mindspace_path)

    def is_already_mindspace(self, path: str) -> bool:
        """Check if we already have a mindspace at the specified path."""
        mindspace_dir = os.path.join(path, self.MINDSPACE_DIR)
        if os.path.exists(mindspace_dir):
            return True

        return False

    def create_mindspace(self, path: str, folders: List[str]) -> None:
        """
        Create a new mindspace at the specified path.

        Args:
            path: Directory path where the mindspace should be created.
            folders: List of folder names to create within the mindspace.

        Raises:
            MindspaceExistsError: If a mindspace already exists at the specified path.
            OSError: If there are filesystem errors creating the mindspace.
        """
        mindspace_dir = os.path.join(path, self.MINDSPACE_DIR)
        if os.path.exists(mindspace_dir):
            raise MindspaceExistsError(f"Mindspace already exists at {path}")

        try:
            # Create mindspace directory structure
            os.makedirs(mindspace_dir)

            for folder in folders:
                os.makedirs(os.path.join(path, folder), exist_ok=True)

            # Create and save default settings
            settings = MindspaceSettings()
            settings.save(os.path.join(mindspace_dir, self.SETTINGS_FILE))

            # Create empty session file
            session_path = os.path.join(mindspace_dir, self.SESSION_FILE)
            with open(session_path, 'w', encoding='utf-8') as f:
                json.dump({"tabs": []}, f, indent=4)

        except OSError as e:
            self._logger.error("Failed to create mindspace at %s: %s", path, str(e))
            # Clean up any partially created mindspace
            if os.path.exists(mindspace_dir):
                try:
                    shutil.rmtree(mindspace_dir)
                except OSError:
                    pass  # Ignore cleanup errors

            # Also clean up any created folders
            if folders:
                for folder in folders:
                    folder_path = os.path.join(path, folder)
                    if os.path.exists(folder_path):
                        try:
                            shutil.rmtree(folder_path)
                        except OSError:
                            pass  # Ignore cleanup errors
            raise

    def open_mindspace(self, path: str) -> None:
        """
        Open an existing mindspace.

        Args:
            path: Path to the mindspace directory.

        Returns:
            The MindspaceSettings object for the opened mindspace.

        Raises:
            MindspaceNotFoundError: If no mindspace exists at the specified path.
            MindspaceError: If there are errors loading the mindspace settings.
        """
        mindspace_dir = os.path.join(path, self.MINDSPACE_DIR)
        if not os.path.exists(mindspace_dir):
            raise MindspaceNotFoundError(f"No mindspace found at {path}")

        try:
            settings_path = os.path.join(mindspace_dir, self.SETTINGS_FILE)
            settings = MindspaceSettings.load(settings_path)
            self._mindspace_path = path
            self._settings = settings
            self._directory_tracker.load_tracking(path)
            self._update_home_tracking()
            self._load_system_interactions()
            self.settings_changed.emit()

        except Exception as e:
            self._logger.error("Failed to open mindspace at %s: %s", path, str(e))
            raise MindspaceError(f"Failed to open mindspace: {str(e)}") from e

    def close_mindspace(self) -> None:
        """Close the current mindspace."""
        if self.has_mindspace():
            self._directory_tracker.save_tracking(self._mindspace_path)
            self._mindspace_path = ""
            self._settings = None
            self._system_interactions.clear()
            self._directory_tracker.clear_tracking()
            self._update_home_tracking()
            self.settings_changed.emit()

    def save_mindspace_state(self, state: Dict) -> None:
        """
        Save mindspace state to disk.

        Args:
            state: Dictionary containing tabs and layout state

        Raises:
            MindspaceError: If saving state fails
        """
        if not self.has_mindspace():
            raise MindspaceError("No mindspace is active")

        try:
            # Ensure .humbug directory exists
            self.ensure_mindspace_dir(".humbug")

            for tab_state in state.get('tabs', []):
                if 'path' in tab_state and os.path.isabs(tab_state['path']):
                    try:
                        tab_state['path'] = os.path.relpath(
                            tab_state['path'],
                            self._mindspace_path
                        )
                    except ValueError:
                        # Path is outside mindspace, keep as absolute
                        pass

            # Write session file
            session_file = os.path.join(self._mindspace_path, self.MINDSPACE_DIR, self.SESSION_FILE)
            with open(session_file, 'w', encoding='utf-8') as f:
                json.dump(state, f, indent=4)

        except OSError as e:
            raise MindspaceError(f"Failed to save mindspace state: {str(e)}") from e

    def load_mindspace_state(self) -> Dict:
        """
        Load mindspace state from disk.

        Returns:
            Dictionary containing tabs and layout state

        Raises:
            MindspaceError: If loading state fails
        """
        if not self.has_mindspace():
            raise MindspaceError("No mindspace is active")

        try:
            session_file = os.path.join(self._mindspace_path, self.MINDSPACE_DIR, self.SESSION_FILE)
            if not os.path.exists(session_file):
                return {}

            with open(session_file, encoding='utf-8') as f:
                state = json.load(f)

            for tab_state in state.get('tabs', []):
                if 'path' in tab_state and not os.path.isabs(tab_state['path']):
                    tab_state['path'] = os.path.join(
                        self._mindspace_path,
                        tab_state['path']
                    )

            return state

        except json.JSONDecodeError as e:
            raise MindspaceError(f"Failed to parse mindspace state: {str(e)}") from e

        except OSError as e:
            raise MindspaceError(f"Failed to load mindspace state: {str(e)}") from e

    def get_last_mindspace(self) -> str | None:
        """
        Get the path of the last opened mindspace.

        Returns:
            Path to the last mindspace that was opened, or None if no mindspace has been opened
            or the last mindspace no longer exists.
        """
        try:
            with open(self._home_config, encoding='utf-8') as f:
                data = json.load(f)
                mindspace_path = data.get("lastMindspace")
                if mindspace_path and os.path.exists(mindspace_path):
                    return mindspace_path

        except (FileNotFoundError, json.JSONDecodeError):
            pass

        return None

    def get_absolute_path(self, path: str) -> str:
        """
        Convert a mindspace-relative path to an absolute path.

        Args:
            path: Absolute path or path relative to the mindspace root.

        Returns:
            Absolute path.

        Raises:
            MindspaceNotFoundError: If no mindspace is currently open.
        """
        if not self.has_mindspace():
            raise MindspaceNotFoundError("No mindspace is currently open")

        path = os.path.expanduser(path)
        if os.path.isabs(path):
            return os.path.abspath(path)

        return os.path.abspath(os.path.join(self._mindspace_path, path))

    def get_relative_path(self, path: str) -> str:
        """
        Convert an absolute path to a mindspace-relative path if possible.

        Args:
            path: Absolute path to convert.

        Returns:
            Path relative to mindspace root, or the abosolute path if outside mindspace,
        """
        abs_path = os.path.abspath(os.path.expanduser(path))
        if not self.has_mindspace():
            return abs_path

        try:
            # Normalize both paths for comparison
            mindspace_path = os.path.abspath(self._mindspace_path)

            # Check if the path is actually within the mindspace
            # by comparing the normalized path beginnings
            common_path = os.path.commonpath([abs_path, mindspace_path])
            if common_path != mindspace_path:
                return abs_path

            # If we get here, the path is within the mindspace, so make it relative
            return os.path.relpath(abs_path, mindspace_path)

        except ValueError:
            self._logger.warning(
                "Failed to convert path '%s' to relative path within mindspace '%s': %s",
                path, self._mindspace_path, str(ValueError)
            )
            raise

    def ensure_mindspace_dir(self, dir_path: str) -> str:
        """
        Ensure a directory exists within the mindspace.

        Args:
            dir_path: Directory path relative to mindspace root.

        Returns:
            Absolute path to the created directory.

        Raises:
            MindspaceNotFoundError: If no mindspace is currently open.
            OSError: If directory cannot be created.
        """
        if not self.has_mindspace():
            raise MindspaceNotFoundError("No mindspace is currently open")

        abs_path = self.get_absolute_path(dir_path)
        try:
            os.makedirs(abs_path, exist_ok=True)
            return abs_path

        except OSError as e:
            raise MindspaceError(f"Failed to create directory '{dir_path}' in mindspace: {e}") from e

    def _update_home_tracking(self) -> None:
        """Update the home directory tracking file with current mindspace path."""
        try:
            os.makedirs(os.path.dirname(self._home_config), exist_ok=True)
            with open(self._home_config, 'w', encoding='utf-8') as f:
                json.dump({"lastMindspace": self._mindspace_path}, f, indent=4)

        except OSError as e:
            self._logger.error("Failed to update home tracking: %s", str(e))
            # Non-critical error, don't raise

    def update_file_dialog_directory(self, path: str) -> None:
        """Update the last used file dialog directory."""
        if self.has_mindspace():
            self._directory_tracker.update_file_dialog_directory(path)
            self._directory_tracker.save_tracking(self._mindspace_path)

    def update_conversations_directory(self, path: str) -> None:
        """Update the last used conversations directory."""
        if self.has_mindspace():
            self._directory_tracker.update_conversations_directory(path)
            self._directory_tracker.save_tracking(self._mindspace_path)

    def file_dialog_directory(self) -> str:
        """Get the last used file dialog directory."""
        return self._directory_tracker.file_dialog_directory()

    def conversations_directory(self) -> str:
        """Get the last used conversations directory."""
        return self._directory_tracker.conversations_directory()

    def add_system_interaction(self, source: SystemMessageSource, content: str) -> SystemMessage:
        """
        Add a new system interaction message.

        Args:
            source: Source of the message (user or system)
            content: Content of the message

        Returns:
            The created SystemMessage

        Raises:
            MindspaceNotFoundError: If no mindspace is open
        """
        if not self.has_mindspace():
            raise MindspaceNotFoundError("No mindspace is currently open")

        message = SystemMessage.create(source, content)
        self._system_interactions.add_message(message)
        self._save_system_interactions()
        self.system_interactions_updated.emit()
        return message

    def get_system_interactions(self) -> List[SystemMessage]:
        """
        Get all system interaction messages.

        Returns:
            List of SystemMessage objects

        Raises:
            MindspaceNotFoundError: If no mindspace is open
        """
        if not self.has_mindspace():
            raise MindspaceNotFoundError("No mindspace is currently open")

        return self._system_interactions.get_messages()

    def clear_system_interactions(self) -> None:
        """
        Clear all system interaction messages.

        Raises:
            MindspaceNotFoundError: If no mindspace is open
        """
        if not self.has_mindspace():
            raise MindspaceNotFoundError("No mindspace is currently open")

        self._system_interactions.clear()
        self._save_system_interactions()
        self.system_interactions_updated.emit()

    def _save_system_interactions(self) -> None:
        """Save system interactions to disk."""
        if not self.has_mindspace():
            return

        try:
            # Ensure .humbug directory exists
            mindspace_dir = os.path.join(self._mindspace_path, self.MINDSPACE_DIR)
            os.makedirs(mindspace_dir, exist_ok=True)

            # Save interactions
            interactions_path = os.path.join(mindspace_dir, self.SYSTEM_INTERACTIONS_FILE)
            self._system_interactions.save(interactions_path)

        except OSError as e:
            # Non-critical error, don't raise any exceptions
            self._logger.error("Failed to save system interactions: %s", str(e))

    def _load_system_interactions(self) -> None:
        """Load system interactions from disk."""
        if not self.has_mindspace():
            return

        try:
            interactions_path = os.path.join(
                self._mindspace_path,
                self.MINDSPACE_DIR,
                self.SYSTEM_INTERACTIONS_FILE
            )
            self._system_interactions.load(interactions_path)

        except Exception as e:
            # Non-critical error, don't raise any exceptions
            self._logger.info("Failed to load system interactions: %s", str(e))
