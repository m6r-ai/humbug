"""
Git repository inspection.

This package provides a lightweight interface for querying git repository
state and retrieving file diffs without any GUI dependencies.
"""

from git.git_commit_info import GitCommitInfo
from git.git_error import GitCommandError, GitError, GitNotFoundError, GitNotRepositoryError
from git.git_repository import GitRepository, find_repo_root
from git.git_status import GitFileStatus, GitStatusCode

__all__ = [
    # Commit info
    "GitCommitInfo",

    # Exceptions
    "GitError",
    "GitNotFoundError",
    "GitNotRepositoryError",
    "GitCommandError",

    # Repository
    "GitRepository",
    "find_repo_root",

    # Status
    "GitFileStatus",
    "GitStatusCode",
]
