"""Background poller for VCS (git) working-tree status."""

import asyncio
import logging
import os
from dataclasses import dataclass

from PySide6.QtCore import QObject, QTimer, Signal

from git import GitCommandError, GitNotFoundError, GitNotRepositoryError
from git import (
    MergeState, VCSFileStatus, find_repositories, get_merge_state, get_status
)

from desktop.file_watcher.file_watcher import FileWatcher


_POLL_INTERVAL_MS = 10000


@dataclass
class _PollResult:
    """Result of a single background poll cycle."""
    repos: list[str]
    status_by_repo: dict[str, list[VCSFileStatus]]
    merge_states: dict[str, MergeState]
    error: Exception | None = None


def _poll_worker(mindspace_path: str) -> _PollResult:
    """
    Blocking poll worker executed on a thread-pool thread.

    Discovers every repository within the mindspace and runs ``git status`` for
    each.  Falls back to walking upward from the mindspace when no repository is
    found inside it (the mindspace itself lives within a repo).  No Qt objects
    are touched here.

    Args:
        mindspace_path: Absolute path to the mindspace root.

    Returns:
        _PollResult with the discovered repos and their status lists.
    """
    repos = _discover_repos(mindspace_path)

    if not repos:
        return _PollResult(repos=[], status_by_repo={}, merge_states={})

    status_by_repo: dict[str, list[VCSFileStatus]] = {}
    merge_states: dict[str, MergeState] = {}
    error: Exception | None = None

    for repo in repos:
        # Repositories nested inside this one (e.g. a cloned project) are tracked
        # separately; their files must not appear in this repo's status, where
        # git would otherwise list the nested repo as a single untracked entry.
        nested = [r for r in repos if r != repo and r.startswith(repo + os.sep)]

        try:
            status = get_status(repo, _status_subtree(repo, mindspace_path))
            status_by_repo[repo] = _exclude_nested(status, nested)

        except (GitNotFoundError, GitNotRepositoryError, GitCommandError) as e:
            status_by_repo[repo] = []
            error = e

        # Merge/rebase state is a cheap filesystem check, computed here on the
        # worker thread so the sidebar never probes it on the Qt main thread.
        merge_states[repo] = get_merge_state(repo)

    return _PollResult(
        repos=repos, status_by_repo=status_by_repo, merge_states=merge_states, error=error
    )


def _exclude_nested(status: list[VCSFileStatus], nested_repos: list[str]) -> list[VCSFileStatus]:
    """
    Drop status entries that live inside a nested repository.

    Args:
        status: Status entries for the parent repository.
        nested_repos: Absolute roots of repositories nested inside the parent.

    Returns:
        Status entries excluding anything within a nested repository.
    """
    if not nested_repos:
        return status

    kept: list[VCSFileStatus] = []
    for entry in status:
        path = os.path.normpath(entry.path)
        if any(path == r or path.startswith(r + os.sep) for r in nested_repos):
            continue

        kept.append(entry)

    return kept


def _status_subtree(repo: str, mindspace_path: str) -> str:
    """
    Return the subtree that a repo's status should be scoped to.

    A project repo living inside the mindspace reports its whole tree.  When the
    mindspace instead lives *inside* a larger repo (the fallback case), status is
    scoped to the mindspace so files outside it are not surfaced.

    Args:
        repo: Absolute repository root.
        mindspace_path: Absolute mindspace root.

    Returns:
        The subtree path to pass to ``get_status``.
    """
    repo_n = os.path.normpath(repo)
    ms = os.path.normpath(mindspace_path)
    if repo_n == ms or repo_n.startswith(ms + os.sep):
        return repo_n

    return ms


def _is_within_mindspace(path: str, mindspace_path: str) -> bool:
    """Return True if *path* is the mindspace root or a descendant of it."""
    if not path or not mindspace_path:
        return False

    path_norm = os.path.normpath(path)
    root_norm = os.path.normpath(mindspace_path)
    return path_norm == root_norm or path_norm.startswith(root_norm + os.sep)


def _discover_repos(mindspace_path: str) -> list[str]:
    """
    Return the repositories located inside the mindspace.

    Enforces a hard mindspace boundary: only repositories at or below the
    mindspace root are surfaced.  We deliberately never climb up to an enclosing
    repository, so the panel can never operate on files outside the mindspace.

    Args:
        mindspace_path: Absolute path to the mindspace root.

    Returns:
        Sorted list of absolute repository root paths (empty if none inside).
    """
    if not mindspace_path:
        return []

    return find_repositories(mindspace_path)


class MindspaceVCSPoller(QObject):
    """
    Singleton background poller tracking git state for every repo in a mindspace.

    Emits:
    - ``repositories_changed`` when the set of discovered repositories changes.
    - ``repo_state_changed`` when the mindspace gains or loses all repositories.
    - ``status_changed`` with the *active* repository's changed-file list.

    Change detection combines a FileWatcher on each repo's ``.git/HEAD`` and
    ``.git/index`` (plus the mindspace directory) with a 10-second periodic
    timer.  Blocking work runs on asyncio's default executor so the Qt main
    thread never stalls; a guard flag prevents overlapping poll cycles.
    """

    repositories_changed = Signal(list)             # list[str] repo roots
    repo_state_changed = Signal(bool)               # True = at least one repo present
    status_changed = Signal(list)                   # list[VCSFileStatus] for active repo

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
        self._repos: list[str] = []
        self._active_repo: str = ""
        self._status_by_repo: dict[str, list[VCSFileStatus]] = {}
        self._merge_states: dict[str, MergeState] = {}
        self._changed_paths: set[str] = set()
        self._has_repo: bool = False
        self._poll_running: bool = False
        # (active_repo, status) last emitted via status_changed, so we only emit
        # when the active repo or its file list actually changed.
        self._last_emit: tuple[str, list[VCSFileStatus]] | None = None

        self._file_watcher = FileWatcher()

        self._poll_timer = QTimer(self)
        self._poll_timer.setInterval(_POLL_INTERVAL_MS)
        self._poll_timer.setSingleShot(False)
        self._poll_timer.timeout.connect(self._on_trigger)

    def set_mindspace(self, path: str) -> None:
        """
        Set the mindspace root to monitor.

        Args:
            path: Absolute path to the mindspace root, or empty string to stop.
        """
        self._poll_timer.stop()
        self._unregister_watches()

        self._mindspace_path = path
        self._repos = []
        self._active_repo = ""
        self._status_by_repo = {}
        self._merge_states = {}
        self._changed_paths = set()
        self._poll_running = False
        self._last_emit = None

        if self._has_repo:
            self._has_repo = False
            self.repo_state_changed.emit(False)

        self.repositories_changed.emit([])

        if path:
            self._on_trigger()
            self._poll_timer.start()

    def force_refresh(self) -> None:
        """Trigger an immediate poll cycle outside the normal timer cadence."""
        if self._mindspace_path:
            self._on_trigger()

    def repositories(self) -> list[str]:
        """Return the list of currently discovered repository roots."""
        return list(self._repos)

    def set_active_repo(self, repo_root: str) -> None:
        """
        Select the active repository whose status feeds ``status_changed``.

        Args:
            repo_root: Absolute path to a repository root.  May be a repo that
                has not been discovered yet (e.g. just selected from the file
                tree); a refresh is triggered to pick it up.  A repo outside the
                mindspace is rejected to preserve the mindspace boundary.
        """
        repo_root = os.path.normpath(repo_root) if repo_root else ""
        if repo_root and not _is_within_mindspace(repo_root, self._mindspace_path):
            return

        if repo_root == self._active_repo:
            return

        self._active_repo = repo_root

        if repo_root and repo_root not in self._repos:
            # Not polled yet: don't flash an empty tree — wait for the refresh
            # to load its status, then emit.
            self.force_refresh()

        else:
            self._emit_status_if_changed()

    def _emit_status_if_changed(self) -> None:
        """Emit status_changed only when the active repo or its status changed."""
        status = self._status_by_repo.get(self._active_repo, [])
        key = (self._active_repo, status)
        if key != self._last_emit:
            self._last_emit = key
            self.status_changed.emit(status)

    def active_repo(self) -> str:
        """Return the active repository root, or "" if none."""
        return self._active_repo

    def merge_state(self, repo_root: str) -> MergeState:
        """Return the cached merge/rebase state for a repository."""
        return self._merge_states.get(repo_root, MergeState.NONE)

    def repo_root(self) -> str:
        """Return the active repository root (alias used by the sidebar/mediator)."""
        return self._active_repo

    def has_repo(self) -> bool:
        """Return True if the mindspace contains at least one git repository."""
        return self._has_repo

    def has_vcs_changes(self, path: str) -> bool:
        """
        Return whether the given path has VCS changes in any discovered repo.

        Args:
            path: Absolute path to the file to check.

        Returns:
            True if the file appears in any repository's current status.
        """
        return os.path.normpath(path) in self._changed_paths

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
        """Watch the mindspace dir and each repo's key git files."""
        if not self._mindspace_path:
            return

        self._file_watcher.watch_file(self._mindspace_path, self._on_watch_changed)

        for repo in self._repos:
            self._file_watcher.watch_file(
                os.path.join(repo, ".git", "HEAD"), self._on_watch_changed
            )
            self._file_watcher.watch_file(
                os.path.join(repo, ".git", "index"), self._on_watch_changed
            )

    def _unregister_watches(self) -> None:
        """Remove all FileWatcher callbacks registered by this poller."""
        if not self._mindspace_path:
            return

        self._file_watcher.unwatch_file(self._mindspace_path, self._on_watch_changed)

        for repo in self._repos:
            self._file_watcher.unwatch_file(
                os.path.join(repo, ".git", "HEAD"), self._on_watch_changed
            )
            self._file_watcher.unwatch_file(
                os.path.join(repo, ".git", "index"), self._on_watch_changed
            )

    async def _run_poll(self) -> None:
        """Run the blocking poll worker on the executor, then process the result."""
        mindspace_path = self._mindspace_path

        try:
            loop = asyncio.get_event_loop()
            result: _PollResult = await loop.run_in_executor(
                None, _poll_worker, mindspace_path
            )

        finally:
            self._poll_running = False

        if self._mindspace_path != mindspace_path:
            return

        self._apply_result(result)

    def _apply_result(self, result: _PollResult) -> None:
        """
        Apply a completed poll result on the main thread.

        Args:
            result: The _PollResult returned by the worker.
        """
        repos_changed = result.repos != self._repos

        if repos_changed:
            self._unregister_watches()
            self._repos = result.repos
            self._register_watches()
            self.repositories_changed.emit(list(self._repos))

        else:
            # Re-register watches every poll so the post-poll .git/index mtime
            # becomes the new baseline (git status refreshes the index).
            self._register_watches()

        has_repo = bool(self._repos)
        if has_repo != self._has_repo:
            self._has_repo = has_repo
            self.repo_state_changed.emit(has_repo)

        if result.error is not None and not result.status_by_repo:
            if isinstance(result.error, GitNotFoundError):
                self._logger.warning("git not found: %s", result.error)

            else:
                self._logger.debug("git status failed: %s", result.error)

            return

        self._status_by_repo = result.status_by_repo
        self._merge_states = result.merge_states
        self._changed_paths = {
            os.path.normpath(entry.path)
            for status in result.status_by_repo.values()
            for entry in status
        }

        # Default the active repo to the first discovered one when unset or when
        # the previously active repo has disappeared.
        if self._active_repo not in self._repos:
            self._active_repo = self._repos[0] if self._repos else ""

        self._emit_status_if_changed()
