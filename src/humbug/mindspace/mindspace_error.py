class MindspaceError(Exception):
    """Base exception for mindspace-related errors."""


class MindspaceNotFoundError(MindspaceError):
    """Raised when attempting to access a non-existent mindspace."""


class MindspaceExistsError(MindspaceError):
    """Raised when attempting to create a mindspace that already exists."""
