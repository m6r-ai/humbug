"""Shell history manager for managing shell command history within mindspaces."""

import logging
import os
from typing import List

from PySide6.QtCore import QObject, Signal

from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.tabs.shell.shell_history import ShellHistory
from humbug.tabs.shell.shell_message import ShellMessage
from humbug.tabs.shell.shell_message_source import ShellMessageSource


class ShellHistoryManager(QObject):
    """
    Manages shell command history within mindspaces.
    
    This manager handles loading, saving, and managing shell command history
    that is specific to each mindspace.
    """

    # Signal emitted when shell history is updated
    history_updated = Signal()

    SHELL_HISTORY_FILE = "shell_history.json"

    _instance = None

    def __new__(cls) -> 'ShellHistoryManager':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(ShellHistoryManager, cls).__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialize mindspace manager if not already initialized."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._mindspace_manager = MindspaceManager()
            self._history = ShellHistory()
            self._logger = logging.getLogger("ShellHistoryManager")

            # Connect to mindspace changes
            self._mindspace_manager.settings_changed.connect(self._on_mindspace_changed)
            self._initialized = True

    def _on_mindspace_changed(self) -> None:
        """Handle mindspace changes by loading appropriate history."""
        if self._mindspace_manager.has_mindspace():
            self._load_history()

        else:
            self._history.clear()

        self.history_updated.emit()

    def add_message(self, source: ShellMessageSource, content: str) -> ShellMessage:
        """
        Add a new shell message to history.

        Args:
            source: Source of the message (user, success, error)
            content: Content of the message

        Returns:
            The created ShellMessage
        """
        message = ShellMessage.create(source, content)
        self._history.add_message(message)

        # Save to disk if we have a mindspace
        if self._mindspace_manager.has_mindspace():
            self._save_history()

        self.history_updated.emit()
        return message

    def get_messages(self) -> List[ShellMessage]:
        """
        Get all shell messages in chronological order.

        Returns:
            List of ShellMessage objects
        """
        return self._history.get_messages()

    def get_user_commands(self) -> List[str]:
        """
        Get list of user commands from history (newest first).

        Returns:
            List of user command strings
        """
        return self._history.get_user_commands()

    def clear_history(self) -> None:
        """Clear all shell history."""
        self._history.clear()

        # Save empty history to disk if we have a mindspace
        if self._mindspace_manager.has_mindspace():
            self._save_history()

        self.history_updated.emit()

    def _get_history_file_path(self) -> str | None:
        """
        Get the path to the shell history file for the current mindspace.

        Returns:
            Path to history file, or None if no mindspace is open
        """
        if not self._mindspace_manager.has_mindspace():
            return None

        mindspace_path = self._mindspace_manager.mindspace_path()
        return os.path.join(
            mindspace_path,
            self._mindspace_manager.MINDSPACE_DIR,
            self.SHELL_HISTORY_FILE
        )

    def _save_history(self) -> None:
        """Save shell history to disk."""
        history_path = self._get_history_file_path()
        if not history_path:
            return

        try:
            self._history.save(history_path)
            self._logger.debug("Saved shell history to %s", history_path)

        except OSError as e:
            self._logger.error("Failed to save shell history: %s", str(e))
            # Non-critical error, don't raise

    def _load_history(self) -> None:
        """Load shell history from disk."""
        history_path = self._get_history_file_path()
        if not history_path:
            return

        try:
            self._history.load(history_path)
            self._logger.debug("Loaded shell history from %s", history_path)

        except Exception as e:
            self._logger.info("Failed to load shell history: %s", str(e))
            # Non-critical error, start with empty history
