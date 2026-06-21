import json
import logging
import os
from typing import Dict, List

from PySide6.QtCore import QObject, Signal

from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace_message import MindspaceMessage
from mindspace.mindspace_settings import MindspaceSettings

from desktop.mindspace.mindspace_directory_tracker import MindspaceDirectoryTracker


class MindspaceManager(QObject):
    """
    Qt-facing singleton that owns the Mindspace model.

    Wraps Mindspace with Qt signals and adds the two UI-only concerns that do
    not belong in the pure model: directory tracking (last-used file dialog
    paths) and home-config tracking (last opened mindspace path).

    All mindspace logic lives in Mindspace.  This class only adds the Qt
    observer machinery and the UI-specific state.
    """

    settings_changed     = Signal()
    interactions_updated = Signal()
    usage_updated        = Signal()

    MINDSPACE_DIR = Mindspace.MINDSPACE_DIR

    _instance = None
    _logger = logging.getLogger("MindspaceManager")

    def __new__(cls) -> 'MindspaceManager':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialise if not already done."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._mindspace = Mindspace(
                on_settings_changed=self.settings_changed.emit,
                on_interactions_updated=self.interactions_updated.emit,
                on_usage_updated=self.usage_updated.emit,
            )
            self._directory_tracker = MindspaceDirectoryTracker()
            self._home_config = os.path.expanduser("~/.humbug/mindspace.json")
            self._initialized = True

    def mindspace(self) -> Mindspace:
        """Return the underlying Mindspace model for direct access."""
        return self._mindspace

    def mindspace_path(self) -> str:
        """Return the absolute path to the open mindspace, or empty string."""
        return self._mindspace.mindspace_path()

    def has_mindspace(self) -> bool:
        """Return True if a mindspace is currently open."""
        return self._mindspace.has_mindspace()

    def settings(self) -> MindspaceSettings | None:
        """Return current mindspace settings, or None if no mindspace is open."""
        return self._mindspace.settings()

    def is_already_mindspace(self, path: str) -> bool:
        """Return True if a mindspace already exists at path."""
        return self._mindspace.is_already_mindspace(path)

    def check_mindspace(self, path: str) -> bool:
        """Return True if a mindspace exists at path."""
        return self._mindspace.check_mindspace(path)

    def create_mindspace(self, path: str, folders: list) -> None:
        """Create a new mindspace at path with the given subdirectories."""
        self._mindspace.create_mindspace(path, folders)

    def open_mindspace(self, path: str) -> None:
        """Open an existing mindspace and load its state."""
        self._mindspace.open_mindspace(path)
        self._directory_tracker.load_tracking(path)
        self._update_home_tracking()

    def close_mindspace(self) -> None:
        """Close the current mindspace and persist directory tracking."""
        if self._mindspace.has_mindspace():
            self._directory_tracker.save_tracking(self._mindspace.mindspace_path())
            self._mindspace.close_mindspace()
            self._directory_tracker.clear_tracking()
            self._update_home_tracking()

    def update_settings(self, new_settings: MindspaceSettings) -> None:
        """Persist and apply updated mindspace settings."""
        self._mindspace.update_settings(new_settings)

    def get_last_mindspace(self) -> str | None:
        """Return the path of the last opened mindspace, or None."""
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
        """Convert a mindspace-relative path to an absolute path."""
        return self._mindspace.get_absolute_path(path)

    def get_relative_path(self, path: str) -> str:
        """Convert an absolute path to a mindspace-relative path if possible."""
        return self._mindspace.get_relative_path(path)

    def get_mindspace_relative_path(self, path: str) -> str | None:
        """Convert an absolute path to a mindspace-relative path, or None if outside."""
        return self._mindspace.get_mindspace_relative_path(path)

    def ensure_mindspace_dir(self, dir_path: str) -> str:
        """Ensure a directory exists within the mindspace, creating it if needed."""
        return self._mindspace.ensure_mindspace_dir(dir_path)

    def add_interaction(self, level: MindspaceLogLevel, content: str) -> MindspaceMessage:
        """Append a message to the interaction log and persist it."""
        return self._mindspace.add_interaction(level, content)

    def get_interactions(self) -> List[MindspaceMessage]:
        """Return all interaction log messages."""
        return self._mindspace.get_interactions()

    def save_mindspace_state(self, state: Dict) -> None:
        """Persist session state (open tabs, layout) to disk."""
        self._mindspace.save_mindspace_state(state)

    def load_mindspace_state(self) -> Dict:
        """Load session state from disk."""
        return self._mindspace.load_mindspace_state()

    def file_dialog_directory(self) -> str:
        """Return the last directory used in a file dialog."""
        return self._directory_tracker.file_dialog_directory()

    def update_file_dialog_directory(self, path: str) -> None:
        """Update and persist the last used file dialog directory."""
        if self._mindspace.has_mindspace():
            self._directory_tracker.update_file_dialog_directory(path)
            self._directory_tracker.save_tracking(self._mindspace.mindspace_path())

    def conversations_directory(self) -> str:
        """Return the last directory used when opening or saving conversations."""
        return self._directory_tracker.conversations_directory()

    def update_conversations_directory(self, path: str) -> None:
        """Update and persist the last used conversations directory."""
        if self._mindspace.has_mindspace():
            self._directory_tracker.update_conversations_directory(path)
            self._directory_tracker.save_tracking(self._mindspace.mindspace_path())

    def _update_home_tracking(self) -> None:
        """Persist the last-opened mindspace path to the home config file."""
        try:
            os.makedirs(os.path.dirname(self._home_config), exist_ok=True)
            with open(self._home_config, 'w', encoding='utf-8') as f:
                json.dump({"lastMindspace": self._mindspace.mindspace_path()}, f, indent=4)

        except OSError as e:
            self._logger.error("Failed to update home tracking: %s", str(e))
