"""Abstract base class for platform-specific terminal implementations."""

from abc import ABC, abstractmethod
import logging
from typing import Optional, Tuple


class TerminalBase(ABC):
    """Abstract base class defining terminal interface."""

    def __init__(self):
        """Initialize terminal base."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._process_id: Optional[int] = None
        self._running = True

    @abstractmethod
    async def start(self, command: Optional[str] = None) -> Tuple[int, int]:
        """
        Start terminal process.

        Args:
            command: Optional command to run instead of shell

        Returns:
            Tuple of (process_id, master_fd)

        Raises:
            OSError: If process creation fails
        """

    @abstractmethod
    async def terminate(self) -> None:
        """Terminate the terminal process."""

    @abstractmethod
    def is_running(self) -> bool:
        """Check if process is still running."""

    @abstractmethod
    def update_window_size(self, rows: int, cols: int) -> None:
        """
        Update terminal window size.

        Args:
            rows: Number of rows
            cols: Number of columns
        """

    @abstractmethod
    async def read_data(self, size: int) -> bytes:
        """
        Read data from terminal.

        Args:
            size: Maximum number of bytes to read

        Returns:
            bytes read from terminal

        Raises:
            OSError: If read operation fails
        """

    @abstractmethod
    async def write_data(self, data: bytes) -> None:
        """
        Write data to terminal.

        Args:
            data: Bytes to write

        Raises:
            OSError: If write operation fails
        """

    def get_process_id(self) -> Optional[int]:
        """Get process ID."""
        return self._process_id

    def transfer_to(self, other: 'TerminalBase') -> None:
        """
        Transfer terminal ownership to another terminal instance.

        Args:
            other: Terminal instance to transfer to
        """
