"""Exception notifier singleton for communicating uncaught exceptions to the UI."""

import logging
from typing import ClassVar

from PySide6.QtCore import QObject, Signal


class ExceptionNotifier(QObject):
    """Singleton class for notifying the UI of uncaught exceptions."""

    _instance: ClassVar['ExceptionNotifier | None'] = None

    # Signal emitted when an uncaught exception occurs
    exception_occurred = Signal()

    def __new__(cls) -> 'ExceptionNotifier':
        """Ensure only one instance exists."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self) -> None:
        """Initialize the exception notifier."""
        # Only initialize once
        if hasattr(self, '_initialized'):
            return

        super().__init__()
        self._logger = logging.getLogger("ExceptionNotifier")
        self._initialized = True

    def notify_exception(self) -> None:
        """
        Notify the UI that an uncaught exception has occurred.

        This method is thread-safe and can be called from any thread.
        """
        self._logger.debug("Notifying UI of uncaught exception")
        self.exception_occurred.emit()


def get_exception_notifier() -> ExceptionNotifier:
    """
    Get the global exception notifier instance.

    Returns:
        ExceptionNotifier: The singleton instance
    """
    return ExceptionNotifier()
