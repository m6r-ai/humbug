"""Exceptions for git operations."""


class GitError(Exception):
    """Base exception for git operations."""


class GitNotFoundError(GitError):
    """Raised when the git executable cannot be found."""


class GitNotRepositoryError(GitError):
    """Raised when a path is not within a git repository."""


class GitCommandError(GitError):
    """Raised when a git command fails unexpectedly."""

    def __init__(self, message: str, returncode: int, stderr: str) -> None:
        """
        Initialize the exception.

        Args:
            message: Error message
            returncode: Git process return code
            stderr: Stderr output from git
        """
        super().__init__(message)
        self.returncode = returncode
        self.stderr = stderr
