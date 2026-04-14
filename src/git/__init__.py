"""
Git repository inspection.

This package provides a lightweight interface for querying git repository
state and retrieving file diffs without any GUI dependencies.
"""

from git.git_error import GitCommandError, GitError, GitNotFoundError, GitNotRepositoryError
from git.git_repository import find_repo_root, get_file_diff, is_file_tracked

__all__ = [
    # Exceptions
    "GitError",
    "GitNotFoundError",
    "GitNotRepositoryError",
    "GitCommandError",
    # Functions
    "find_repo_root",
    "is_file_tracked",
    "get_file_diff",
]
