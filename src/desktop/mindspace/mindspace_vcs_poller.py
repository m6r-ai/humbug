"""Background poller for VCS (git) working-tree status."""

import asyncio
import logging
import os
from dataclasses import dataclass

from PySide6.QtCore import QObject, QTimer, Signal

from git import GitCommandError, GitNotFoundError, GitNotRepositoryError
from git import VCSFileStatus, find_repo_root, get_status

from desktop.file_watcher.file_watcher import FileWatcher


_POLL_INTERVAL_MS = 10000


@dataclass
class _PollResult:
    """Result of a single background poll cycle."""
    repo_root: str
    status: list[VCSFileStatus]
    error: Exception | None


def _poll_worker(mindspace_path: str, current_repo_root: str) -> _PollResult:
    """
    Blocking poll worker executed on a thread-pool thread.

    Performs the .git filesystem walk and git status subprocess call so that
    neither blocks the Qt main thread.  Returns a plain _PollResult — no Qt
    objects are created or touched here.

    Args:
        mindspace_path: Absolute path to the mindspace root.
        current_repo_root: The repo root discovered on the previous poll, or
            empty string if none was known.

    Returns:
        _PollResult with the detected repo root and current status list.
    """
    repo_root = _detect_repo_root(mindspace_path, current_repo_root)

    if not repo_root:
        return _PollResult(repo_root="", status=[], error=None)

    try:
        status = get_status(repo_root, mindspace_path)
        return _PollResult(repo_root=repo_root, status=status, error=None)

    except (GitNotFoundError, GitNotRepositoryError, GitCommandError) as e:
        return _PollResult(repo_root=repo_root, status=[], error=e)


def _detect_repo_root(mindspace_path: str, current_repo_root: str) -> str:
    """
    Return the git repo root for the mindspace, or empty string if none.

    Uses a cheap filesystem check first (.git directory existence) before
    falling back to the subprocess-based find_repo_root call.

    Args:
        mindspace_path: Absolute path to the mindspace root.
        current_repo_root: Previously known repo root, or empty string.

    Returns:
        Absolute repo root path, or empty string.
    """
    if current_repo_root:
        git_dir = os.path.join(current_repo_root, ".git")
        if os.path.exists(git_dir):
            return current_repo_root

    candidate = mindspace_path
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


class MindspaceVCSPoller(QObject):
    """
    Singleton background poller that tracks git repository state for a mindspace.

    Emits repo_state_changed when a repository appears or disappears, and
    status_changed whenever the set of modified files changes.

    Change detection uses two mechanisms:

    - FileWatcher monitors .git/HEAD, .git/index, and the mindspace directory
      itself for changes.  Any change triggers an immediate git status run.
      After each git status completes the watches are re-registered so that the
      post-status mtime becomes the new baseline, preventing the index refresh
      side-effect of git status from re-triggering the watcher.

    - A 10-second periodic timer catches working-tree file modifications that
      the file watcher cannot see (it is not recursive).

    The blocking filesystem walk and git subprocess are executed on a
    thread-pool thread via asyncio's default executor so that neither ever
    stalls the Qt main thread.  A guard flag prevents overlapping poll cycles
    if a previous git call has not yet completed.
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
        self._poll_running: bool = False

        self._file_watcher = FileWatcher()

        self._poll_timer = QTimer(self)
        self._poll_timer.setInterval(_POLL_INTERVAL_MS)
        self._poll_timer.setSingleShot(False)
        self._poll_timer.timeout.connect(self._on_trigger)

    def set_mindspace(self, path: str) -> None:
        """
        Set the mindspace root to monitor.

        Stops any existing polling, resets state, and restarts if a non-empty
        path is provided.

        Args:
            path: Absolute path to the mindspace root, or empty string to stop.
        """
        self._poll_timer.stop()
        self._unregister_watches()

        self._mindspace_path = path
        self._repo_root = ""
        self._last_status = []
        self._poll_running = False

        if self._has_repo:
            self._has_repo = False
            self.repo_state_changed.emit(False)

        if path:
            self._on_trigger()
            self._poll_timer.start()

    def force_refresh(self) -> None:
        """Trigger an immediate poll cycle outside the normal timer cadence."""
        if self._mindspace_path:
            self._on_trigger()

    def has_repo(self) -> bool:
        """Return True if a git repository is currently detected for the mindspace."""
        return self._has_repo

    def has_vcs_changes(self, path: str) -> bool:
        """
        Return whether the given path has VCS changes in the last poll.

        Args:
            path: Absolute path to the file to check.

        Returns:
            True if the file appears in the current VCS status, False otherwise.
        """
        return any(s.path == path for s in self._last_status)

    def _on_trigger(self) -> None:
        """Dispatch a poll cycle to the thread pool if none is running."""
        if not self._mindspace_path:
            return

        if self._poll_running:
            return

        self._poll_running = True
        loop = asyncio.get_event_loop()
        loop.create_task(self._run_poll())

    def _on_watch_changed(self, _path: str) -> None:
        """File watcher callback: a watched path changed, trigger a poll."""
        self._on_trigger()

    def _register_watches(self) -> None:
        """Register FileWatcher callbacks for the repo's key git files and the mindspace dir."""
        if not self._repo_root:
            return

        self._file_watcher.watch_file(self._mindspace_path, self._on_watch_changed)
        self._file_watcher.watch_file(
            os.path.join(self._repo_root, ".git", "HEAD"), self._on_watch_changed
        )
        self._file_watcher.watch_file(
            os.path.join(self._repo_root, ".git", "index"), self._on_watch_changed
        )

    def _unregister_watches(self) -> None:
        """Remove all FileWatcher callbacks registered by this poller."""
        if not self._mindspace_path:
            return

        self._file_watcher.unwatch_file(self._mindspace_path, self._on_watch_changed)

        if self._repo_root:
            self._file_watcher.unwatch_file(
                os.path.join(self._repo_root, ".git", "HEAD"), self._on_watch_changed
            )
            self._file_watcher.unwatch_file(
                os.path.join(self._repo_root, ".git", "index"), self._on_watch_changed
            )

    async def _run_poll(self) -> None:
        """
        Coroutine that runs the blocking poll worker on the executor and then
        processes the result back on the main thread.
        """
        mindspace_path = self._mindspace_path
        current_repo_root = self._repo_root

        try:
            loop = asyncio.get_event_loop()
            result: _PollResult = await loop.run_in_executor(
                None,
                _poll_worker,
                mindspace_path,
                current_repo_root,
            )

        finally:
            self._poll_running = False

        if self._mindspace_path != mindspace_path:
            return

        self._apply_result(result)

    def _apply_result(self, result: _PollResult) -> None:
        """
        Apply a completed poll result on the main thread.

        Updates internal state, re-registers file watches with the post-poll
        mtime as the new baseline, and emits signals as needed.

        Args:
            result: The _PollResult returned by the worker.
        """
        repo_changed = result.repo_root != self._repo_root

        if repo_changed:
            self._unregister_watches()
            self._repo_root = result.repo_root
            has_repo = bool(result.repo_root)

            if has_repo != self._has_repo:
                self._has_repo = has_repo
                self._last_status = []
                self.repo_state_changed.emit(has_repo)

        if not self._repo_root:
            return

        # Re-register watches after every poll so that the post-poll mtime of
        # .git/index becomes the new baseline.  This prevents the index refresh
        # side-effect of git status from immediately re-triggering the watcher.
        self._register_watches()

        if result.error is not None:
            if isinstance(result.error, GitNotFoundError):
                self._logger.warning("git not found: %s", result.error)

            else:
                self._logger.debug("git status failed: %s", result.error)

            return

        if result.status != self._last_status:
            self._last_status = result.status
            self.status_changed.emit(result.status)
