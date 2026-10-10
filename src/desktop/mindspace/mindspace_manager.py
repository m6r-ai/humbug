import logging
import os
from typing import IO

from PySide6.QtCore import QObject, Signal

from mindspace.mindspace import Mindspace
from mindspace.mindspace_error import (
    MindspaceAlreadyOpenError, MindspaceNotFoundError
)
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace_message import MindspaceMessage
from mindspace.mindspace_settings import MindspaceSettings

from desktop import json_store
from desktop.mindspace.mindspace_directory_tracker import MindspaceDirectoryTracker


def _same_mindspace(a: str, b: str) -> bool:
    """
    Return True if two paths refer to the same mindspace.

    Mindspace paths reach the manager in whatever form the user chose them — with or
    without a trailing separator, through a symlink, or with a different case on a
    case-insensitive filesystem.  Comparing the raw strings would treat those as
    different mindspaces, which would list the currently open mindspace among the
    recent ones and list the same mindspace twice.
    """
    return os.path.realpath(a) == os.path.realpath(b)


class MindspaceManager(QObject):
    """
    Qt-facing singleton that owns the Mindspace model.

    Wraps Mindspace with Qt signals and adds the two UI-only concerns that do
    not belong in the pure model: directory tracking (last-used file dialog
    paths) and home-config tracking (last opened mindspace path).

    All mindspace logic lives in Mindspace.  This class only adds the Qt
    observer machinery and the UI-specific state.
    """

    settings_changed = Signal()
    interactions_updated = Signal()
    usage_updated = Signal()

    MAX_RECENT_MINDSPACES = 10

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
            self._claim: IO[str] | None = None
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
        """
        Open an existing mindspace and load its state.

        Args:
            path: Path of the mindspace to open.

        Raises:
            MindspaceAlreadyOpenError: The mindspace is open elsewhere.
            MindspaceError: The mindspace could not be opened.
        """
        # Check the path is a mindspace before claiming it.  Claiming creates the
        # .humbug directory if it is missing, so claiming a path that is not a
        # mindspace would leave a stray .humbug directory behind when the open
        # then fails.
        if not self._mindspace.check_mindspace(path):
            raise MindspaceNotFoundError(f"No mindspace found at {path}")

        # Give up our own claim first if this is the mindspace we already hold.  The
        # lock is exclusive even against this process, so re-opening it while still
        # holding it would otherwise look like another instance owning it.
        if self._claim is not None and _same_mindspace(path, self._mindspace.mindspace_path()):
            self._release_claim()

        claim = json_store.acquire(self._claim_path(path))
        if claim is None:
            raise MindspaceAlreadyOpenError(path)

        try:
            self._mindspace.open_mindspace(path)

        except Exception:
            claim.close()
            raise

        self._release_claim()
        self._claim = claim
        self._directory_tracker.load_tracking(path)
        self._update_home_tracking()

    def close_mindspace(self) -> None:
        """Close the current mindspace and persist directory tracking."""
        if self._mindspace.has_mindspace():
            self._directory_tracker.save_tracking(self._mindspace.mindspace_path())
            self._mindspace.close_mindspace()
            self._directory_tracker.clear_tracking()
            self._update_home_tracking()

        self._release_claim()

    def _release_claim(self) -> None:
        """Give up this process's claim on the current mindspace, if it holds one."""
        if self._claim is not None:
            self._claim.close()
            self._claim = None

    @staticmethod
    def _claim_path(path: str) -> str:
        """
        Return the file whose lock marks a mindspace as open.

        Args:
            path: Path of the mindspace.

        Returns:
            Path used to claim exclusive use of the mindspace.
        """
        return os.path.join(path, Mindspace.MINDSPACE_DIR, "mindspace")

    def update_settings(self, new_settings: MindspaceSettings) -> None:
        """Persist and apply updated mindspace settings."""
        self._mindspace.update_settings(new_settings)

    def get_last_mindspace(self) -> str | None:
        """Return the path of the last opened mindspace, or None."""
        data = self._load_home_config()
        if data is None:
            return None

        mindspace_path = data.get("lastMindspace")
        if mindspace_path and os.path.exists(mindspace_path):
            return mindspace_path

        return None

    def recent_mindspaces(self) -> list[str]:
        """
        Return recently used mindspace paths, most-recent first.

        Stale paths (no longer existing on disk) are silently pruned.
        The currently open mindspace is excluded from the list.
        """
        data = self._load_home_config()
        if data is None:
            return []

        raw = data.get("recentMindspaces", [])
        if not isinstance(raw, list):
            return []

        current = self._mindspace.mindspace_path()
        seen: set[str] = set()
        result: list[str] = []
        for path in raw:
            if not isinstance(path, str):
                continue

            if current and _same_mindspace(path, current):
                continue

            normalised = os.path.realpath(path)
            if normalised in seen:
                continue

            if not os.path.exists(path):
                continue

            seen.add(normalised)
            result.append(path)

        return result[:self.MAX_RECENT_MINDSPACES]

    def _load_home_config(self) -> dict | None:
        """Load and parse the home config file, returning None on failure."""
        return json_store.read_json(self._home_config)

    def can_pin_path(self, abs_path: str) -> bool:
        """Return True if the given absolute path is allowed to be pinned."""
        return self._mindspace.can_pin_path(abs_path)

    def is_path_pinned(self, abs_path: str) -> bool:
        """Return True if the given absolute path is pinned."""
        return self._mindspace.is_path_pinned(abs_path)

    def is_effectively_pinned(self, abs_path: str) -> bool:
        """Return True if the given absolute path is pinned directly or via an ancestor folder."""
        return self._mindspace.is_effectively_pinned(abs_path)

    def folder_has_pinned_content(self, folder_abs_path: str) -> bool:
        """Return True if the folder is pinned, or has any pinned conversation inside it."""
        return self._mindspace.folder_has_pinned_content(folder_abs_path)

    def set_path_pinned(self, abs_path: str, pinned: bool) -> None:
        """Pin or unpin an absolute path, persisting the change."""
        self._mindspace.set_path_pinned(abs_path, pinned)

    def migrate_pinned_path(self, old_abs_path: str, new_abs_path: str) -> None:
        """Update pinned entries after a path is renamed or moved."""
        self._mindspace.migrate_pinned_path(old_abs_path, new_abs_path)

    def unpin_path_tree(self, abs_path: str) -> None:
        """Remove pinned entries for a path and anything nested under it."""
        self._mindspace.unpin_path_tree(abs_path)

    def pinned_root_paths(self) -> list[str]:
        """Return absolute paths for pinned entries to lift into the Pinned section."""
        return self._mindspace.pinned_root_paths()

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

    def get_interactions(self) -> list[MindspaceMessage]:
        """Return all interaction log messages."""
        return self._mindspace.get_interactions()

    def save_mindspace_state(self, state: dict) -> None:
        """Persist session state (open tabs, layout) to disk."""
        self._mindspace.save_mindspace_state(state)

    def load_mindspace_state(self) -> dict:
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
        """Persist the last-opened mindspace path and recent list to the home config."""
        current = self._mindspace.mindspace_path()
        json_store.update_json(self._home_config, lambda previous: self._merge_home_tracking(previous, current))

    def _merge_home_tracking(self, previous_data: dict, current: str) -> dict:
        """
        Fold the current mindspace into the home config's recent list.

        Called with the config as it is on disk at the moment of writing, so entries
        another instance has recorded since this one last read it are preserved rather
        than overwritten.

        Args:
            previous_data: Home config contents as currently stored.
            current: Path of the mindspace now open, or empty if none.

        Returns:
            The home config contents to store.
        """
        # Build the recent list from the existing config, promoting the previous
        # current mindspace to the front.  The mindspace that is now current is kept in
        # the list: this list records what the user has opened, and it is
        # recent_mindspaces() that excludes the currently open one when the menu is
        # built.  Dropping it here would lose it permanently, which matters when a
        # second instance opens a mindspace that the first instance still has open.
        previous_recent: list[str] = []
        raw = previous_data.get("recentMindspaces", [])
        if isinstance(raw, list):
            previous_recent = [p for p in raw if isinstance(p, str)]

        previous_last = previous_data.get("lastMindspace")
        if not isinstance(previous_last, str):
            previous_last = None

        recent: list[str] = []
        seen: set[str] = set()

        # The mindspace we just left goes to the front of the recent list.
        if previous_last and os.path.exists(previous_last):
            recent.append(previous_last)
            seen.add(os.path.realpath(previous_last))

        # Append remaining entries that are not duplicates.
        for path in previous_recent:
            normalised = os.path.realpath(path)
            if normalised in seen:
                continue

            if not os.path.exists(path):
                continue

            seen.add(normalised)
            recent.append(path)

        return {
            "lastMindspace": current,
            "recentMindspaces": recent[:self.MAX_RECENT_MINDSPACES],
        }
