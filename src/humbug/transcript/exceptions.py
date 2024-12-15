"""Exceptions for transcript-related operations."""


class TranscriptWriteError(Exception):
    """Errors that occur during transcript writing.

    Raised when the application fails to write conversation data, including:
    - Atomic write operation failures (IOError, OSError)
    - Permission errors during write (PermissionError)
    - Missing parent directory (FileNotFoundError)
    """
    pass


class TranscriptReadError(Exception):
    """Errors that occur during transcript reading.

    Raised when the application fails to read transcript data, including:
    - JSON parsing failures (json.JSONDecodeError)
    - Missing transcript file (FileNotFoundError)
    - Permission denied reading file (PermissionError)
    """
    pass


class TranscriptRotationError(Exception):
    """Errors that occur during transcript file rotation.

    Raised when transcript rotation fails, including:
    - File system errors during rotation (OSError)
    - Cannot delete old files (PermissionError)
    - Cannot create new transcript file (IOError)
    """
    pass
