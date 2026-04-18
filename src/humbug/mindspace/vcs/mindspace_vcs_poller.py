"""Background poller for VCS (git) working-tree status."""

import logging
import os

from PySide6.QtCore import QObject, QTimer, Signal

from git import GitCommandError, GitNotFoundError, GitNotRepositoryError
from git import VCSFileStatus, find_repo_root, get_status


_POLL_INTERVAL_MS = 2000


class MindspaceVCSPoller(QObject):
    """
    Singleton background poller that tracks git repository state for a mindspace.

    Emits repo_state_changed when a repository appears or disappears, and
    status_changed whenever the set of modified files changes.

    The poller checks for .git directory existence on every tick (cheap), and
    only runs git status when a repository is confirmed present.
    """

    repo_state_changed = Signal(bool)               # True = repo found, False = no repo
    status_changed = Signal(list)                   # list[VCSFileStatus]

    _instance = None
    _logger = logging.getLogger("MindspaceVCSPoller")

    def __new__(cls) -> 'MindspaceVCSPoller':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialise the poller if not already done."""
        if hasattr(self, '_initialized'):
            return

        super().__init__()
        self._initialized = True

        self._mindspace_path: str = ""
        self._repo_root: str = ""
        self._has_repo: bool = False
        self._last_status: list[VCSFileStatus] = []

        self._timer = QTimer(self)
        self._timer.setInterval(_POLL_INTERVAL_MS)
        self._timer.setSingleShot(False)
        self._timer.timeout.connect(self._poll)

    def set_mindspace(self, path: str) -> None:
        """
        Set the mindspace root to monitor.

        Stops any existing polling, resets state, and restarts if a non-empty
        path is provided.

        Args:
            path: Absolute path to the mindspace root, or empty string to stop.
        """
        self._timer.stop()
        self._mindspace_path = path
        self._repo_root = ""
        self._has_repo = False
        self._last_status = []

        if path:
            self._poll()
            self._timer.start()

    def force_refresh(self) -> None:
        """Trigger an immediate poll cycle outside the normal timer cadence."""
        if self._mindspace_path:
            self._poll()

    def has_vcs_changes(self, path: str) -> bool:
        """
        Return whether the given path has VCS changes in the last poll.

        Args:
            path: Absolute path to the file to check.

        Returns:
            True if the file appears in the current VCS status, False otherwise.
        """
        return any(s.path == path for s in self._last_status)

    def _poll(self) -> None:
        """Single poll cycle: check repo presence then fetch status if present."""
        if not self._mindspace_path:
            return

        repo_root = self._detect_repo_root()

        if repo_root != self._repo_root:
            self._repo_root = repo_root
            has_repo = bool(repo_root)

            if has_repo != self._has_repo:
                self._has_repo = has_repo
                self._last_status = []
                self.repo_state_changed.emit(has_repo)

        if not self._repo_root:
            return

        self._refresh_status()

    def _detect_repo_root(self) -> str:
        """
        Return the git repo root for the mindspace, or empty string if none.

        Uses a cheap filesystem check first (.git directory existence) before
        falling back to the subprocess-based find_repo_root call.

        Returns:
            Absolute repo root path, or empty string.
        """
        # Fast path: if we already have a known repo root, verify .git still exists.
        if self._repo_root:
            git_dir = os.path.join(self._repo_root, ".git")
            if os.path.exists(git_dir):
                return self._repo_root

            # .git has gone — fall through to full scan.

        # Walk up from the mindspace looking for a .git directory.
        candidate = self._mindspace_path
        while True:
            if os.path.exists(os.path.join(candidate, ".git")):
                try:
                    return find_repo_root(candidate)

                except (GitNotFoundError, GitNotRepositoryError, GitCommandError):
                    return ""

            parent = os.path.dirname(candidate)
            if parent == candidate:
                break

            candidate = parent

        return ""

    def _refresh_status(self) -> None:
        """
        Run git status and emit status_changed if the result differs from last time.
        """
        try:
            new_status = get_status(self._repo_root, self._mindspace_path)

        except GitNotFoundError as e:
            self._logger.warning("git not found: %s", e)
            return

        except (GitNotRepositoryError, GitCommandError) as e:
            self._logger.debug("git status failed: %s", e)
            return

        if new_status != self._last_status:
            self._last_status = new_status
            self.status_changed.emit(new_status)
