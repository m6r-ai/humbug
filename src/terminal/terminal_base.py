"""Abstract base class for platform-specific terminal implementations."""

from abc import ABC, abstractmethod
import logging
import sys
from typing import Tuple


class TerminalBase(ABC):
    """Abstract base class defining terminal interface."""

    def __init__(self, working_directory: str) -> None:
        """
        Initialize terminal base.

        Args:
            working_directory: Directory where the terminal process should start
        """
        self._logger = logging.getLogger(self.__class__.__name__)
        self._working_directory = working_directory
        self._process_id: int | None = None
        self._process_name: str = ""
        self._running = True

    @abstractmethod
    async def start(self, command: str | None = None) -> Tuple[int, int]:
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

    def get_process_id(self) -> int | None:
        """Get process ID."""
        return self._process_id

    def get_process_name(self) -> str:
        """Get process name."""
        return self._process_name

    def get_platform(self) -> str:
        """
        Get platform identifier.

        Returns:
            Platform string (e.g., 'linux', 'darwin', 'win32')
        """
        return sys.platform

    def transfer_to(self, other: 'TerminalBase') -> None:
        """
        Transfer terminal ownership to another terminal instance.

        Args:
            other: Terminal instance to transfer to
        """
