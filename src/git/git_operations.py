"""Git working-tree mutation and remote operations."""

import os
import sys
from dataclasses import dataclass
from enum import Enum, auto
from urllib.parse import urlparse

from git.git_error import GitCommandError
from git.git_repository import _GIT_NETWORK_TIMEOUT, _run_git


class MergeState(Enum):
    """Whether the repository is mid-merge, mid-rebase, or neither."""
    NONE = auto()
    MERGE = auto()
    REBASE = auto()


@dataclass(frozen=True)
class StashEntry:
    """A single entry in the stash list."""
    ref: str            # Stash ref, e.g. "stash@{0}"
    message: str        # Human-readable stash description


@dataclass(frozen=True)
class RemoteInfo:
    """A configured remote and its fetch URL."""
    name: str
    url: str


@dataclass(frozen=True)
class BranchInfo:
    """Local branch listing plus the currently checked-out branch."""
    current: str            # Current branch name, or "" when in detached HEAD
    branches: list[str]     # All local branch names


@dataclass(frozen=True)
class UpstreamStatus:
    """Tracking relationship between the current branch and its upstream."""
    upstream: str | None    # Upstream ref name (e.g. "origin/main"), or None if unset
    ahead: int              # Commits the local branch is ahead of upstream
    behind: int             # Commits the local branch is behind upstream


def stage_files(repo_root: str, paths: list[str]) -> None:
    """
    Stage the given paths (``git add``).

    Args:
        repo_root: Absolute path to the repository root.
        paths: Absolute paths to stage.  Deleted files are handled correctly.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails.
    """
    if not paths:
        return

    _run_git(["add", "--"] + paths, cwd=repo_root)


def unstage_files(repo_root: str, paths: list[str]) -> None:
    """
    Remove the given paths from the index, keeping working-tree changes
    (``git reset -q HEAD -- <paths>``).

    Args:
        repo_root: Absolute path to the repository root.
        paths: Absolute paths to unstage.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails.
    """
    if not paths:
        return

    _run_git(["reset", "-q", "HEAD", "--"] + paths, cwd=repo_root)


def discard_changes(repo_root: str, paths: list[str]) -> None:
    """
    Discard working-tree modifications to tracked files, restoring them to
    their staged/HEAD content (``git checkout -- <paths>``).

    This does not remove untracked files; callers should delete those directly.

    Args:
        repo_root: Absolute path to the repository root.
        paths: Absolute paths of tracked files to restore.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails.
    """
    if not paths:
        return

    _run_git(["checkout", "--"] + paths, cwd=repo_root)


def commit(repo_root: str, message: str, amend: bool = False) -> None:
    """
    Create a commit from the currently staged changes.

    Args:
        repo_root: Absolute path to the repository root.
        message: Commit message.  Must be non-empty.
        amend: If True, replace the most recent commit (``git commit --amend``)
            with the staged changes and the new message.

    Raises:
        ValueError: If the message is empty or whitespace only.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the commit fails (e.g. nothing staged).
    """
    if not message.strip():
        raise ValueError("Commit message must not be empty")

    args = ["commit", "-m", message]
    if amend:
        args.insert(1, "--amend")

    _run_git(args, cwd=repo_root)


def get_merge_state(repo_root: str) -> MergeState:
    """
    Return whether the repository is mid-merge, mid-rebase, or clean.

    Uses a cheap filesystem check of the ``.git`` directory (no subprocess).

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        The current MergeState.
    """
    git_dir = _resolve_git_dir(repo_root)
    if os.path.exists(os.path.join(git_dir, "MERGE_HEAD")):
        return MergeState.MERGE

    if any(os.path.isdir(os.path.join(git_dir, d)) for d in ("rebase-merge", "rebase-apply")):
        return MergeState.REBASE

    return MergeState.NONE


def _resolve_git_dir(repo_root: str) -> str:
    """
    Return the repository's real git directory.

    Handles linked worktrees and submodules, where ``<repo>/.git`` is a file
    containing a ``gitdir: <path>`` pointer rather than a directory.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        Absolute path to the git directory (best-effort; falls back to
        ``<repo>/.git`` if the pointer cannot be read).
    """
    git_path = os.path.join(repo_root, ".git")
    if os.path.isdir(git_path):
        return git_path

    if os.path.isfile(git_path):
        try:
            with open(git_path, encoding="utf-8") as f:
                content = f.read().strip()

        except OSError:
            return git_path

        prefix = "gitdir:"
        if content.startswith(prefix):
            target = content[len(prefix):].strip()
            if not os.path.isabs(target):
                target = os.path.normpath(os.path.join(repo_root, target))

            return target

    return git_path


def accept_ours(repo_root: str, paths: list[str]) -> None:
    """
    Resolve conflicts by keeping our version and staging the result.

    Args:
        repo_root: Absolute path to the repository root.
        paths: Absolute paths of conflicted files.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the operation fails.
    """
    if not paths:
        return

    _run_git(["checkout", "--ours", "--"] + paths, cwd=repo_root)
    _run_git(["add", "--"] + paths, cwd=repo_root)


def accept_theirs(repo_root: str, paths: list[str]) -> None:
    """
    Resolve conflicts by taking the incoming version and staging the result.

    Args:
        repo_root: Absolute path to the repository root.
        paths: Absolute paths of conflicted files.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the operation fails.
    """
    if not paths:
        return

    _run_git(["checkout", "--theirs", "--"] + paths, cwd=repo_root)
    _run_git(["add", "--"] + paths, cwd=repo_root)


def mark_resolved(repo_root: str, paths: list[str]) -> None:
    """
    Mark conflicted files as resolved by staging them (``git add``).

    Use after editing the files to remove conflict markers manually.

    Args:
        repo_root: Absolute path to the repository root.
        paths: Absolute paths of the resolved files.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the operation fails.
    """
    if not paths:
        return

    _run_git(["add", "--"] + paths, cwd=repo_root)


def abort_merge(repo_root: str) -> None:
    """
    Abort an in-progress merge (``git merge --abort``).

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If there is no merge to abort.
    """
    _run_git(["merge", "--abort"], cwd=repo_root)


def abort_rebase(repo_root: str) -> None:
    """
    Abort an in-progress rebase (``git rebase --abort``).

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If there is no rebase to abort.
    """
    _run_git(["rebase", "--abort"], cwd=repo_root)


def revert_commit(repo_root: str, commit_hash: str) -> None:
    """
    Create a new commit that undoes *commit_hash* (``git revert --no-edit``).

    Args:
        repo_root: Absolute path to the repository root.
        commit_hash: The commit to revert.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the revert fails (e.g. conflicts).
    """
    _run_git(["revert", "--no-edit", commit_hash], cwd=repo_root)


def reset(repo_root: str, mode: str, target: str = "HEAD") -> None:
    """
    Reset the current branch to *target* (``git reset --<mode> <target>``).

    Args:
        repo_root: Absolute path to the repository root.
        mode: One of "soft" (keep index + worktree), "mixed" (keep worktree,
            reset index) or "hard" (discard index + worktree changes).
        target: Commit-ish to reset to (default "HEAD").

    Raises:
        ValueError: If *mode* is not a recognised reset mode.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the reset fails.
    """
    flags = {"soft": "--soft", "mixed": "--mixed", "hard": "--hard"}
    if mode not in flags:
        raise ValueError(f"Unknown reset mode: {mode!r}")

    _run_git(["reset", flags[mode], target], cwd=repo_root)


def undo_last_commit(repo_root: str) -> None:
    """
    Undo the most recent commit, keeping its changes staged
    (``git reset --soft HEAD~1``).

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If there is no prior commit to undo.
    """
    _run_git(["reset", "--soft", "HEAD~1"], cwd=repo_root)


def stash_push(repo_root: str, message: str = "", include_untracked: bool = True) -> None:
    """
    Stash the working-tree (and optionally untracked) changes.

    Args:
        repo_root: Absolute path to the repository root.
        message: Optional stash description.
        include_untracked: If True, also stash untracked files.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the stash fails.
    """
    args = ["stash", "push"]
    if include_untracked:
        args.append("--include-untracked")

    if message.strip():
        args += ["-m", message]

    _run_git(args, cwd=repo_root)


def stash_list(repo_root: str) -> list[StashEntry]:
    """
    Return the list of stash entries, most recent first.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        List of StashEntry.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    raw = _run_git(["stash", "list", "--format=%gd\x1f%s"], cwd=repo_root)

    entries: list[StashEntry] = []
    for line in raw.splitlines():
        if not line.strip():
            continue

        ref, _, message = line.partition("\x1f")
        entries.append(StashEntry(ref=ref, message=message))

    return entries


def stash_pop(repo_root: str, ref: str = "stash@{0}") -> None:
    """
    Apply and remove a stash entry (``git stash pop``).

    Args:
        repo_root: Absolute path to the repository root.
        ref: The stash ref to pop (default the most recent).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the pop fails (e.g. conflicts, no stash).
    """
    _run_git(["stash", "pop", ref], cwd=repo_root)


def stash_apply(repo_root: str, ref: str = "stash@{0}") -> None:
    """
    Apply a stash entry without removing it (``git stash apply``).

    Args:
        repo_root: Absolute path to the repository root.
        ref: The stash ref to apply (default the most recent).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the apply fails.
    """
    _run_git(["stash", "apply", ref], cwd=repo_root)


def stash_drop(repo_root: str, ref: str) -> None:
    """
    Delete a stash entry (``git stash drop``).

    Args:
        repo_root: Absolute path to the repository root.
        ref: The stash ref to drop.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the drop fails.
    """
    _run_git(["stash", "drop", ref], cwd=repo_root)


def get_current_branch(repo_root: str) -> str:
    """
    Return the name of the current branch, or "" when in detached HEAD.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        Branch name, or empty string if HEAD is detached.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    name = _run_git(["rev-parse", "--abbrev-ref", "HEAD"], cwd=repo_root).strip()
    return "" if name == "HEAD" else name


def get_branch_info(repo_root: str) -> BranchInfo:
    """
    Return the local branch list and the current branch.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        BranchInfo with the current branch and all local branch names.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    current = get_current_branch(repo_root)
    output = _run_git(["branch", "--format=%(refname:short)"], cwd=repo_root)
    branches = [line.strip() for line in output.splitlines() if line.strip()]
    return BranchInfo(current=current, branches=branches)


def create_branch(repo_root: str, name: str, checkout: bool = True) -> None:
    """
    Create a new branch, optionally switching to it.

    Args:
        repo_root: Absolute path to the repository root.
        name: Name of the branch to create.
        checkout: If True (default), check out the new branch immediately.

    Raises:
        ValueError: If the branch name is empty or whitespace only.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the branch already exists or the name is invalid.
    """
    name = name.strip()
    if not name:
        raise ValueError("Branch name must not be empty")

    if checkout:
        _run_git(["checkout", "-b", name], cwd=repo_root)

    else:
        _run_git(["branch", name], cwd=repo_root)


def switch_branch(repo_root: str, name: str) -> None:
    """
    Check out an existing branch (``git checkout <name>``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Branch name to switch to.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the switch fails (e.g. conflicting local changes).
    """
    _run_git(["checkout", name], cwd=repo_root)


def delete_branch(repo_root: str, name: str, force: bool = False) -> None:
    """
    Delete a local branch (``git branch -d/-D``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Branch to delete.
        force: If True, delete even if not fully merged (``-D``).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the delete fails (e.g. unmerged without force).
    """
    _run_git(["branch", "-D" if force else "-d", name], cwd=repo_root)


def rename_branch(repo_root: str, old_name: str, new_name: str) -> None:
    """
    Rename a branch (``git branch -m <old> <new>``).

    Args:
        repo_root: Absolute path to the repository root.
        old_name: Current branch name.
        new_name: New branch name.

    Raises:
        ValueError: If the new name is empty.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the rename fails.
    """
    new_name = new_name.strip()
    if not new_name:
        raise ValueError("New branch name must not be empty")

    _run_git(["branch", "-m", old_name, new_name], cwd=repo_root)


def merge_branch(repo_root: str, name: str) -> None:
    """
    Merge a branch into the current branch (``git merge <name>``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Branch to merge into the current one.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the merge fails or conflicts.
    """
    _run_git(["merge", name], cwd=repo_root)


def list_tags(repo_root: str) -> list[str]:
    """
    Return tag names, newest first.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        List of tag names.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    output = _run_git(["tag", "--list", "--sort=-creatordate"], cwd=repo_root)
    return [line.strip() for line in output.splitlines() if line.strip()]


def create_tag(repo_root: str, name: str, target: str = "HEAD", message: str = "") -> None:
    """
    Create a tag at *target* (annotated if a message is given).

    Args:
        repo_root: Absolute path to the repository root.
        name: Tag name.
        target: Commit-ish to tag (default HEAD).
        message: Annotation message; if empty a lightweight tag is created.

    Raises:
        ValueError: If the tag name is empty.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the tag already exists or the name is invalid.
    """
    name = name.strip()
    if not name:
        raise ValueError("Tag name must not be empty")

    if message.strip():
        _run_git(["tag", "-a", name, "-m", message, target], cwd=repo_root)

    else:
        _run_git(["tag", name, target], cwd=repo_root)


def delete_tag(repo_root: str, name: str) -> None:
    """
    Delete a tag (``git tag -d``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Tag to delete.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the tag does not exist.
    """
    _run_git(["tag", "-d", name], cwd=repo_root)


def list_remotes(repo_root: str) -> list[RemoteInfo]:
    """
    Return configured remotes with their fetch URLs.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        List of RemoteInfo (one per remote, using the fetch URL).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    output = _run_git(["remote", "-v"], cwd=repo_root)
    seen: dict[str, str] = {}
    for line in output.splitlines():
        if "(fetch)" not in line:
            continue

        parts = line.split()
        if len(parts) >= 2:
            seen.setdefault(parts[0], parts[1])

    return [RemoteInfo(name=name, url=url) for name, url in seen.items()]


def _get_config(repo_root: str, key: str) -> str:
    """Return a git config value, or "" if it is not set."""
    try:
        return _run_git(["config", key], cwd=repo_root).strip()

    except GitCommandError as e:
        # Return code 1 means the key is not set.
        if e.returncode == 1:
            return ""

        raise


def get_identity(repo_root: str) -> tuple[str, str]:
    """
    Return the effective commit identity (name, email) for the repository.

    Reflects what commits will use (repo-local value, falling back to global).

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        A (name, email) tuple; either may be "" if unset.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    return _get_config(repo_root, "user.name"), _get_config(repo_root, "user.email")


def set_identity(repo_root: str, name: str, email: str) -> None:
    """
    Set the repository-local commit identity (``git config --local``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Author name.
        email: Author email.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails.
    """
    _run_git(["config", "--local", "user.name", name], cwd=repo_root)
    _run_git(["config", "--local", "user.email", email], cwd=repo_root)


def _default_credential_helper() -> str:
    """Return the platform's recommended (secure) credential helper."""
    if sys.platform == "darwin":
        return "osxkeychain"

    if sys.platform == "win32":
        return "manager"

    # Linux/other: an in-memory cache avoids writing tokens to plaintext.
    return "cache --timeout=3600"


def _primary_remote_url(repo_root: str) -> str:
    """Return the URL of 'origin', or the first configured remote."""
    try:
        return _run_git(["remote", "get-url", "origin"], cwd=repo_root).strip()

    except GitCommandError as exc:
        remotes = list_remotes(repo_root)
        if remotes:
            return remotes[0].url

        raise ValueError("No remote is configured for this repository.") from exc


def set_credential(repo_root: str, username: str, token: str) -> None:
    """
    Store an access token for the repository's HTTPS remote, securely.

    The token is saved through git's configured credential helper (the OS
    keychain on macOS / Windows), never in plaintext.  The repository is set to
    key credentials by URL path (``credential.useHttpPath``) so different
    projects on the same host (e.g. two GitHub accounts) keep separate tokens.

    If no credential helper is configured anywhere, a secure per-platform helper
    is set in the global git config as a sensible default.

    Args:
        repo_root: Absolute path to the repository root.
        username: The remote account username (e.g. GitHub login).
        token: The personal access token (used as the password).

    Raises:
        ValueError: If the remote is missing or not an HTTP(S) remote.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If storing the credential fails.
    """
    if not token.strip():
        raise ValueError("Token must not be empty")

    parsed = urlparse(_primary_remote_url(repo_root))
    if parsed.scheme not in ("http", "https"):
        raise ValueError(
            "Token authentication requires an HTTPS remote. This repository's "
            "remote is not HTTPS (it may use SSH)."
        )

    host = parsed.hostname or ""
    if not host:
        raise ValueError("Could not determine the remote host from its URL.")

    # Ensure some credential helper is configured; only add a default if none.
    if not _get_config(repo_root, "credential.helper"):
        _run_git(
            ["config", "--global", "credential.helper", _default_credential_helper()],
            cwd=repo_root,
        )

    # Key credentials by path so multiple accounts on one host stay separate.
    _run_git(["config", "--local", "credential.useHttpPath", "true"], cwd=repo_root)

    spec = f"protocol={parsed.scheme}\nhost={host}\n"
    if parsed.port:
        spec += f"port={parsed.port}\n"

    spec += f"path={parsed.path.lstrip('/')}\nusername={username}\npassword={token}\n\n"
    _run_git(["credential", "approve"], cwd=repo_root, input_text=spec)


def add_remote(repo_root: str, name: str, url: str) -> None:
    """
    Add a remote (``git remote add <name> <url>``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Remote name.
        url: Remote URL.

    Raises:
        ValueError: If the name or url is empty.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the remote already exists.
    """
    name = name.strip()
    url = url.strip()
    if not name or not url:
        raise ValueError("Remote name and URL must not be empty")

    _run_git(["remote", "add", name, url], cwd=repo_root)


def set_remote_url(repo_root: str, name: str, url: str) -> None:
    """
    Change a remote's URL (``git remote set-url <name> <url>``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Remote name.
        url: New remote URL.

    Raises:
        ValueError: If the URL is empty.
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the remote does not exist.
    """
    url = url.strip()
    if not url:
        raise ValueError("Remote URL must not be empty")

    _run_git(["remote", "set-url", name, url], cwd=repo_root)


def remove_remote(repo_root: str, name: str) -> None:
    """
    Remove a remote (``git remote remove <name>``).

    Args:
        repo_root: Absolute path to the repository root.
        name: Remote to remove.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the remote does not exist.
    """
    _run_git(["remote", "remove", name], cwd=repo_root)


def apply_patch(repo_root: str, patch_text: str, cached: bool = True, reverse: bool = False) -> None:
    """
    Apply a unified-diff patch, typically a single hunk (``git apply``).

    Used for per-hunk staging: with ``cached=True`` the patch is applied to the
    index; ``reverse=True`` un-applies it (e.g. to unstage or discard a hunk).

    Args:
        repo_root: Absolute path to the repository root.
        patch_text: The patch text (header + one or more hunks).
        cached: If True, apply to the index (staging); otherwise the worktree.
        reverse: If True, apply the patch in reverse.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the patch does not apply cleanly.
    """
    args = ["apply", "--whitespace=nowarn"]
    if cached:
        args.append("--cached")

    if reverse:
        args.append("--reverse")

    args.append("-")
    _run_git(args, cwd=repo_root, input_text=patch_text)


def fetch(repo_root: str) -> None:
    """
    Fetch updates from all configured remotes (``git fetch --all``).

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the fetch fails.
    """
    _run_git(["fetch", "--all"], cwd=repo_root, timeout=_GIT_NETWORK_TIMEOUT)


def pull(repo_root: str, use_rebase: bool = False) -> None:
    """
    Pull from the current branch's upstream (``git pull``).

    Args:
        repo_root: Absolute path to the repository root.
        use_rebase: If True, rebase local commits onto the upstream
            (``--rebase``) instead of merging.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the pull fails (e.g. no upstream, conflicts).
    """
    args = ["pull", "--rebase"] if use_rebase else ["pull"]
    _run_git(args, cwd=repo_root, timeout=_GIT_NETWORK_TIMEOUT)


def push(repo_root: str, set_upstream: bool = False, force: bool = False) -> None:
    """
    Push the current branch to its upstream (``git push``).

    Args:
        repo_root: Absolute path to the repository root.
        set_upstream: If True, push with ``--set-upstream origin <branch>`` to
            establish tracking for a branch that has no upstream yet.
        force: If True, force-push with ``--force-with-lease`` (safe force,
            used after amend/rebase).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the push fails (e.g. rejected, no upstream).
    """
    force_args = ["--force-with-lease"] if force else []

    if set_upstream:
        branch = get_current_branch(repo_root)
        if not branch:
            raise GitCommandError(
                "Cannot set upstream while in detached HEAD",
                returncode=-1,
                stderr=""
            )

        _run_git(
            ["push"] + force_args + ["--set-upstream", "origin", branch],
            cwd=repo_root,
            timeout=_GIT_NETWORK_TIMEOUT
        )
        return

    _run_git(["push"] + force_args, cwd=repo_root, timeout=_GIT_NETWORK_TIMEOUT)


def push_tags(repo_root: str) -> None:
    """
    Push all tags to the 'origin' remote (``git push --tags``).

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the push fails.
    """
    _run_git(["push", "--tags"], cwd=repo_root, timeout=_GIT_NETWORK_TIMEOUT)


def cherry_pick(repo_root: str, commit_hash: str) -> None:
    """
    Apply a commit onto the current branch (``git cherry-pick``).

    Args:
        repo_root: Absolute path to the repository root.
        commit_hash: The commit to apply.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the cherry-pick fails or conflicts.
    """
    _run_git(["cherry-pick", commit_hash], cwd=repo_root, env={"GIT_EDITOR": "true"})


def list_remote_branches(repo_root: str) -> list[str]:
    """
    Return remote-tracking branch names (e.g. ``origin/main``).

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        Sorted list of remote-tracking branch names, excluding symbolic refs
        such as ``origin/HEAD``.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    output = _run_git(["branch", "-r", "--format=%(refname:short)"], cwd=repo_root)
    return sorted(
        line.strip() for line in output.splitlines()
        if line.strip() and "->" not in line and not line.strip().endswith("/HEAD")
    )


def checkout_remote_branch(repo_root: str, remote_branch: str) -> None:
    """
    Check out a remote branch, creating a local tracking branch.

    Args:
        repo_root: Absolute path to the repository root.
        remote_branch: The remote-tracking branch (e.g. ``origin/feature``).

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the checkout fails.
    """
    # Local branch name = remote branch without the remote prefix.  Checking out
    # this name lets git's DWIM create a tracking branch (or switch if it exists).
    local = remote_branch.split("/", 1)[-1]
    _run_git(["checkout", local], cwd=repo_root)


def rebase(repo_root: str, onto: str) -> None:
    """
    Rebase the current branch onto another ref (``git rebase <onto>``).

    Args:
        repo_root: Absolute path to the repository root.
        onto: The branch/commit to rebase onto.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the rebase fails or conflicts.
    """
    _run_git(["rebase", onto], cwd=repo_root, env={"GIT_EDITOR": "true"})


def rebase_continue(repo_root: str) -> None:
    """
    Continue an in-progress rebase after resolving conflicts.

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If there are still unresolved conflicts.
    """
    _run_git(
        ["rebase", "--continue"], cwd=repo_root,
        env={"GIT_EDITOR": "true", "GIT_SEQUENCE_EDITOR": "true"}
    )


def add_to_gitignore(repo_root: str, pattern: str) -> None:
    """
    Append a pattern to the repository's ``.gitignore``.

    Args:
        repo_root: Absolute path to the repository root.
        pattern: The ignore pattern (e.g. a repo-relative path).

    Raises:
        ValueError: If the pattern is empty.
        OSError: If the .gitignore file cannot be written.
    """
    pattern = pattern.strip()
    if not pattern:
        raise ValueError("Ignore pattern must not be empty")

    gitignore = os.path.join(repo_root, ".gitignore")
    existing = ""
    if os.path.exists(gitignore):
        with open(gitignore, encoding="utf-8") as f:
            existing = f.read()

    if pattern in existing.splitlines():
        return

    prefix = "" if (not existing or existing.endswith("\n")) else "\n"
    with open(gitignore, "a", encoding="utf-8") as f:
        f.write(f"{prefix}{pattern}\n")


def discard_all_changes(repo_root: str) -> None:
    """
    Discard all tracked changes (staged and unstaged) back to HEAD
    (``git reset --hard HEAD``).

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the reset fails.
    """
    _run_git(["reset", "--hard", "HEAD"], cwd=repo_root)


def clean_untracked(repo_root: str) -> None:
    """
    Remove all untracked files and directories (``git clean -fd``).

    Args:
        repo_root: Absolute path to the repository root.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the clean fails.
    """
    _run_git(["clean", "-f", "-d"], cwd=repo_root)


def get_upstream_status(repo_root: str) -> UpstreamStatus:
    """
    Return how far the current branch is ahead of/behind its upstream.

    Args:
        repo_root: Absolute path to the repository root.

    Returns:
        UpstreamStatus.  ``upstream`` is None (and both counts 0) when the
        current branch has no configured upstream.

    Raises:
        GitNotFoundError: If git is not installed or not on PATH.
        GitCommandError: If the command fails unexpectedly.
    """
    try:
        upstream = _run_git(
            ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"],
            cwd=repo_root
        ).strip()

    except GitCommandError as e:
        # Return code 128 means no upstream is configured for this branch.
        if e.returncode == 128:
            return UpstreamStatus(upstream=None, ahead=0, behind=0)

        raise

    counts = _run_git(
        ["rev-list", "--left-right", "--count", "@{upstream}...HEAD"],
        cwd=repo_root
    ).strip()

    # Output is "<behind>\t<ahead>" — left side is upstream-only commits.
    parts = counts.split()
    behind = int(parts[0]) if len(parts) == 2 else 0
    ahead = int(parts[1]) if len(parts) == 2 else 0

    return UpstreamStatus(upstream=upstream, ahead=ahead, behind=behind)
