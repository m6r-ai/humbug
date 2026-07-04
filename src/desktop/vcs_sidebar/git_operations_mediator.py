"""Async mediator between the VCS sidebar and the git backend."""

import asyncio
import logging
from collections.abc import Callable

from PySide6.QtCore import QObject, Signal

from git import (
    BranchInfo,
    GitCommandError,
    GitError,
    GitNotFoundError,
    UpstreamStatus,
    abort_merge,
    abort_rebase,
    accept_ours,
    accept_theirs,
    add_remote,
    add_to_gitignore,
    apply_patch,
    checkout_remote_branch,
    cherry_pick,
    clean_untracked,
    clone_repository,
    commit,
    create_branch,
    create_tag,
    delete_branch,
    delete_tag,
    discard_all_changes,
    discard_changes,
    fetch,
    init_repository,
    mark_resolved,
    push_tags,
    rebase,
    rebase_continue,
    merge_branch,
    remove_remote,
    rename_branch,
    set_credential,
    set_identity,
    set_remote_url,
    get_branch_info,
    get_upstream_status,
    pull,
    push,
    reset,
    revert_commit,
    stage_files,
    stash_apply,
    stash_drop,
    stash_pop,
    stash_push,
    switch_branch,
    undo_last_commit,
    unstage_files,
)


class GitOperationsMediator(QObject):
    """Runs git write/remote operations off the main thread and reports results."""

    # op_name is a stable identifier ("stage", "commit", "push", ...) so the UI
    # can tailor status messages without parsing human-readable text.
    operation_started = Signal(str)                     # op_name
    operation_succeeded = Signal(str)                   # op_name
    operation_failed = Signal(str, str)                 # op_name, error_message

    # Emitted when refresh_info() completes.  Payloads are the backend
    # dataclasses (BranchInfo, UpstreamStatus | None); None means no repo.
    repo_info_changed = Signal(object, object)          # BranchInfo, UpstreamStatus

    _logger = logging.getLogger("GitOperationsMediator")

    # Repos with an operation in flight, shared across ALL mediator instances
    # (the sidebar and the history tab each have their own mediator), so two
    # instances never run git against the same repo at once.
    _active_repos: set[str] = set()

    def __init__(self, parent: QObject | None = None) -> None:
        """Initialise the mediator with no repository selected."""
        super().__init__(parent)
        self._repo_root: str = ""
        self._busy: bool = False
        self._op_repo: str = ""  # repo captured for the in-flight op

    def set_repo_root(self, repo_root: str) -> None:
        """
        Set the repository the mediator operates on.

        Args:
            repo_root: Absolute path to the repository root, or "" to clear.
        """
        self._repo_root = repo_root

    def is_busy(self) -> bool:
        """Return True while an operation is in flight."""
        return self._busy

    # -- Mutating operations -------------------------------------------------

    def stage(self, paths: list[str]) -> None:
        """Stage the given paths."""
        self._dispatch("stage", lambda: stage_files(self._op_repo, paths))

    def unstage(self, paths: list[str]) -> None:
        """Unstage the given paths."""
        self._dispatch("unstage", lambda: unstage_files(self._op_repo, paths))

    def discard(self, paths: list[str]) -> None:
        """Discard working-tree changes to the given tracked paths."""
        self._dispatch("discard", lambda: discard_changes(self._op_repo, paths))

    def commit(self, message: str, amend: bool = False) -> None:
        """Commit the currently staged changes (optionally amending HEAD)."""
        self._dispatch("commit", lambda: commit(self._op_repo, message, amend=amend))

    def commit_and_push(self, message: str, amend: bool = False) -> None:
        """Commit the staged changes and then push to the upstream."""
        def _commit_then_push() -> None:
            commit(self._op_repo, message, amend=amend)
            push(self._op_repo)

        self._dispatch("commit_push", _commit_then_push)

    def create_branch(self, name: str) -> None:
        """Create and check out a new branch."""
        self._dispatch("create_branch", lambda: create_branch(self._op_repo, name))

    def switch_branch(self, name: str) -> None:
        """Check out an existing branch."""
        self._dispatch("switch_branch", lambda: switch_branch(self._op_repo, name))

    def accept_ours(self, paths: list[str]) -> None:
        """Resolve conflicts by keeping our version."""
        self._dispatch("accept_ours", lambda: accept_ours(self._op_repo, paths))

    def accept_theirs(self, paths: list[str]) -> None:
        """Resolve conflicts by taking the incoming version."""
        self._dispatch("accept_theirs", lambda: accept_theirs(self._op_repo, paths))

    def mark_resolved(self, paths: list[str]) -> None:
        """Mark conflicted files as resolved (stage them)."""
        self._dispatch("mark_resolved", lambda: mark_resolved(self._op_repo, paths))

    def abort_merge(self) -> None:
        """Abort an in-progress merge."""
        self._dispatch("abort_merge", lambda: abort_merge(self._op_repo))

    def abort_rebase(self) -> None:
        """Abort an in-progress rebase."""
        self._dispatch("abort_rebase", lambda: abort_rebase(self._op_repo))

    def revert(self, commit_hash: str) -> None:
        """Create a commit that undoes the given commit."""
        self._dispatch("revert", lambda: revert_commit(self._op_repo, commit_hash))

    def reset(self, mode: str, target: str) -> None:
        """Reset the current branch to a target commit."""
        self._dispatch("reset", lambda: reset(self._op_repo, mode, target))

    def undo_last_commit(self) -> None:
        """Undo the most recent commit, keeping its changes staged."""
        self._dispatch("undo", lambda: undo_last_commit(self._op_repo))

    def stash_push(self, message: str = "", include_untracked: bool = True) -> None:
        """Stash the working-tree changes."""
        self._dispatch(
            "stash_push",
            lambda: stash_push(self._op_repo, message, include_untracked=include_untracked)
        )

    def stash_pop(self, ref: str = "stash@{0}") -> None:
        """Apply and remove a stash entry."""
        self._dispatch("stash_pop", lambda: stash_pop(self._op_repo, ref))

    def stash_apply(self, ref: str = "stash@{0}") -> None:
        """Apply a stash entry without removing it."""
        self._dispatch("stash_apply", lambda: stash_apply(self._op_repo, ref))

    def stash_drop(self, ref: str) -> None:
        """Delete a stash entry."""
        self._dispatch("stash_drop", lambda: stash_drop(self._op_repo, ref))

    def delete_branch(self, name: str, force: bool = False) -> None:
        """Delete a local branch."""
        self._dispatch("delete_branch", lambda: delete_branch(self._op_repo, name, force=force))

    def rename_branch(self, old_name: str, new_name: str) -> None:
        """Rename a branch."""
        self._dispatch("rename_branch", lambda: rename_branch(self._op_repo, old_name, new_name))

    def merge_branch(self, name: str) -> None:
        """Merge a branch into the current branch."""
        self._dispatch("merge_branch", lambda: merge_branch(self._op_repo, name))

    def create_tag(self, name: str, target: str = "HEAD", message: str = "") -> None:
        """Create a tag at the given target."""
        self._dispatch("create_tag", lambda: create_tag(self._op_repo, name, target, message))

    def delete_tag(self, name: str) -> None:
        """Delete a tag."""
        self._dispatch("delete_tag", lambda: delete_tag(self._op_repo, name))

    def add_remote(self, name: str, url: str) -> None:
        """Add a remote."""
        self._dispatch("add_remote", lambda: add_remote(self._op_repo, name, url))

    def remove_remote(self, name: str) -> None:
        """Remove a remote."""
        self._dispatch("remove_remote", lambda: remove_remote(self._op_repo, name))

    def set_remote_url(self, name: str, url: str) -> None:
        """Change a remote's URL."""
        self._dispatch("set_remote_url", lambda: set_remote_url(self._op_repo, name, url))

    def set_identity(self, name: str, email: str) -> None:
        """Set the repository-local commit identity."""
        self._dispatch("set_identity", lambda: set_identity(self._op_repo, name, email))

    def set_credential(self, username: str, token: str) -> None:
        """Securely store an access token for the repository's HTTPS remote."""
        self._dispatch("set_credential", lambda: set_credential(self._op_repo, username, token))

    def stage_hunk(self, patch: str) -> None:
        """Stage a single hunk (apply the patch to the index)."""
        self._dispatch("stage_hunk", lambda: apply_patch(self._op_repo, patch, cached=True))

    def unstage_hunk(self, patch: str) -> None:
        """Unstage a single hunk (reverse-apply the patch to the index)."""
        self._dispatch(
            "unstage_hunk", lambda: apply_patch(self._op_repo, patch, cached=True, reverse=True)
        )

    def discard_hunk(self, patch: str) -> None:
        """Discard a single hunk from the working tree."""
        self._dispatch(
            "discard_hunk", lambda: apply_patch(self._op_repo, patch, cached=False, reverse=True)
        )

    def fetch(self) -> None:
        """Fetch from all remotes."""
        self._dispatch("fetch", lambda: fetch(self._op_repo))

    def pull(self, use_rebase: bool = False) -> None:
        """Pull from the current branch's upstream (optionally with rebase)."""
        self._dispatch("pull", lambda: pull(self._op_repo, use_rebase=use_rebase))

    def push(self, set_upstream: bool = False, force: bool = False) -> None:
        """Push the current branch to its upstream (optionally force-with-lease)."""
        self._dispatch("push", lambda: push(self._op_repo, set_upstream=set_upstream, force=force))

    def push_tags(self) -> None:
        """Push all tags to origin."""
        self._dispatch("push_tags", lambda: push_tags(self._op_repo))

    def cherry_pick(self, commit_hash: str) -> None:
        """Apply a commit onto the current branch."""
        self._dispatch("cherry_pick", lambda: cherry_pick(self._op_repo, commit_hash))

    def checkout_remote_branch(self, remote_branch: str) -> None:
        """Check out a remote branch as a local tracking branch."""
        self._dispatch(
            "checkout_remote", lambda: checkout_remote_branch(self._op_repo, remote_branch)
        )

    def rebase(self, onto: str) -> None:
        """Rebase the current branch onto another ref."""
        self._dispatch("rebase", lambda: rebase(self._op_repo, onto))

    def rebase_continue(self) -> None:
        """Continue an in-progress rebase."""
        self._dispatch("rebase_continue", lambda: rebase_continue(self._op_repo))

    def add_to_gitignore(self, pattern: str) -> None:
        """Append a pattern to the repository's .gitignore."""
        self._dispatch("gitignore", lambda: add_to_gitignore(self._op_repo, pattern))

    def discard_all_changes(self) -> None:
        """Discard all tracked changes back to HEAD."""
        self._dispatch("discard_all", lambda: discard_all_changes(self._op_repo))

    def clean_untracked(self) -> None:
        """Remove all untracked files and directories."""
        self._dispatch("clean_untracked", lambda: clean_untracked(self._op_repo))

    # -- Repository lifecycle (no active repo required) ----------------------

    def init_repository(self, path: str) -> None:
        """Initialise a new git repository at *path* (a mindspace folder)."""
        self._dispatch_keyed("init", path, lambda: init_repository(path))

    def clone_repository(self, url: str, parent_dir: str) -> None:
        """Clone *url* into *parent_dir* (a mindspace folder)."""
        self._dispatch_keyed("clone", parent_dir, lambda: clone_repository(url, parent_dir))

    # -- Read-only info refresh ----------------------------------------------

    def refresh_info(self) -> None:
        """
        Query branch and upstream state in the background and emit
        ``repo_info_changed`` with the results.

        Failures are logged but not surfaced as operation errors — stale info
        is preferable to an error dialog during routine refreshes.
        """
        repo_root = self._repo_root
        if not repo_root:
            self.repo_info_changed.emit(BranchInfo(current="", branches=[]), None)
            return

        loop = asyncio.get_event_loop()
        loop.create_task(self._run_refresh(repo_root))

    async def _run_refresh(self, repo_root: str) -> None:
        """Coroutine that loads branch/upstream info on the executor."""
        loop = asyncio.get_event_loop()

        try:
            branch_info, upstream = await loop.run_in_executor(
                None, self._query_info, repo_root
            )

        except GitError as e:
            self._logger.debug("Failed to refresh git info: %s", e)
            return

        # Ignore results for a repo we've since navigated away from.
        if self._repo_root != repo_root:
            return

        self.repo_info_changed.emit(branch_info, upstream)

    @staticmethod
    def _query_info(repo_root: str) -> tuple[BranchInfo, UpstreamStatus]:
        """Blocking worker: fetch branch listing and upstream status."""
        branch_info = get_branch_info(repo_root)
        upstream = get_upstream_status(repo_root)
        return branch_info, upstream

    # -- Internal dispatch ---------------------------------------------------

    def _dispatch(self, op_name: str, func: Callable[[], None]) -> None:
        """
        Run a repository operation on the executor against the active repo.

        The active repository is snapshotted into ``self._op_repo`` for the
        duration of the operation; operation closures read ``self._op_repo``
        (not ``self._repo_root``) so a repo switch that lands mid-flight cannot
        redirect an in-progress command to the wrong repository.

        Args:
            op_name: Stable operation identifier.
            func: Zero-argument callable performing the blocking git work.
        """
        self._dispatch_keyed(op_name, self._repo_root, func)

    def _dispatch_keyed(self, op_name: str, key: str, func: Callable[[], None]) -> None:
        """
        Run *func* on the executor, emitting start/success/failure signals.

        A second operation requested while one is in flight is rejected with an
        ``operation_failed`` so the UI can surface a "please wait" message rather
        than silently interleaving git commands.

        Args:
            op_name: Stable operation identifier.
            key: The path used for the cross-instance busy lock and snapshotted
                into ``self._op_repo`` (a repo root, or a target directory for
                init/clone where no repo exists yet).
            func: Zero-argument callable performing the blocking git work.
        """
        if not key:
            self.operation_failed.emit(op_name, "No repository is selected")
            return

        if self._busy or key in GitOperationsMediator._active_repos:
            self.operation_failed.emit(op_name, "Another git operation is already running")
            return

        self._busy = True
        self._op_repo = key
        GitOperationsMediator._active_repos.add(self._op_repo)
        self.operation_started.emit(op_name)
        loop = asyncio.get_event_loop()
        loop.create_task(self._run_operation(op_name, func))

    async def _run_operation(self, op_name: str, func: Callable[[], None]) -> None:
        """Coroutine that runs a mutating op on the executor and reports the result."""
        loop = asyncio.get_event_loop()

        try:
            try:
                await loop.run_in_executor(None, func)

            finally:
                self._busy = False
                GitOperationsMediator._active_repos.discard(self._op_repo)

        except GitNotFoundError:
            self.operation_failed.emit(op_name, "git executable not found on PATH")

        except GitCommandError as e:
            message = e.stderr.strip() if e.stderr else str(e)
            self.operation_failed.emit(op_name, message)

        except (GitError, ValueError, OSError) as e:
            self.operation_failed.emit(op_name, str(e))

        else:
            self.operation_succeeded.emit(op_name)
