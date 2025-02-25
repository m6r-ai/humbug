# File: unix_terminal.py
"""Unix-specific terminal implementation."""

import asyncio
import os
import pty
import select
import signal
import struct
import termios
import tty
import fcntl
from typing import Optional, Tuple

from humbug.gui.terminal.terminal_base import TerminalBase


class UnixTerminal(TerminalBase):
    """Unix-specific terminal implementation."""

    def __init__(self):
        """Initialize Unix terminal."""
        super().__init__()
        self._main_fd: Optional[int] = None

    def _set_nonblocking(self, fd: int) -> None:
        """Set file descriptor to non-blocking mode."""
        flags = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)

    async def start(self, command: Optional[str] = None) -> Tuple[int, int]:
        """Start Unix terminal process with proper PTY setup."""
        main_fd, secondary_fd = pty.openpty()

        shell = command if command else os.environ.get('SHELL', '/bin/sh')

        # Fork process
        pid = os.fork()

        if pid == 0:  # Child process
            try:
                # Close master
                os.close(main_fd)

                # Create new session
                os.setsid()

                # Configure terminal
                slave_attr = termios.tcgetattr(secondary_fd)
                slave_attr[tty.IFLAG] = termios.ICRNL | termios.IXON
                slave_attr[tty.OFLAG] = termios.OPOST | termios.ONLCR
                slave_attr[tty.LFLAG] = (
                    termios.ISIG | termios.ICANON | termios.ECHO | termios.ECHOE |
                    termios.ECHOK | termios.IEXTEN | termios.ECHOCTL | termios.ECHOKE
                )
                termios.tcsetattr(secondary_fd, termios.TCSANOW, slave_attr)

                # Set controlling terminal
                fcntl.ioctl(secondary_fd, termios.TIOCSCTTY, 0)

                # Set up standard streams
                os.dup2(secondary_fd, 0)
                os.dup2(secondary_fd, 1)
                os.dup2(secondary_fd, 2)

                if secondary_fd > 2:
                    os.close(secondary_fd)

                # Reset all signals to their default handlers
                for i in range(1, signal.NSIG):
                    try:
                        signal.signal(i, signal.SIG_DFL)
                    except OSError:
                        # Some signals can't be modified (like SIGKILL)
                        pass

                # Execute shell/command
                os.execvp(shell.split()[0], shell.split())

            except Exception as e:
                self._logger.exception(f"Child process failed: %s", str(e))

        # Parent process
        os.close(secondary_fd)

        # Configure master terminal
        mode = termios.tcgetattr(main_fd)
        mode[tty.IFLAG] &= ~(
            termios.ICRNL | termios.IXON | termios.IXOFF | termios.ISTRIP
        )
        mode[tty.OFLAG] &= ~(termios.OPOST)
        mode[tty.CFLAG] |= (termios.CS8)
        mode[tty.LFLAG] &= ~(
            termios.ECHO | termios.ICANON | termios.IEXTEN | termios.ISIG
        )
        mode[tty.CC][termios.VMIN] = 0
        mode[tty.CC][termios.VTIME] = 0
        termios.tcsetattr(main_fd, termios.TCSANOW, mode)

        self._set_nonblocking(main_fd)

        self._process_name = shell
        self._process_id = pid
        self._main_fd = main_fd

        return pid, main_fd

    async def terminate(self) -> None:
        """Terminate Unix terminal process."""
        self._running = False

        if self._process_id:
            try:
                # Try graceful termination
                os.killpg(os.getpgid(self._process_id), signal.SIGHUP)

                # Wait for process
                try:
                    await asyncio.get_event_loop().run_in_executor(
                        None, os.waitpid, self._process_id, 0
                    )
                except ChildProcessError:
                    pass  # Already terminated

            except ProcessLookupError:
                pass  # Already terminated

            self._process_id = None

        if self._main_fd is not None:
            try:
                os.close(self._main_fd)
            except OSError:
                pass
            self._main_fd = None

    def is_running(self) -> bool:
        """Check if Unix process is running."""
        if not self._process_id:
            return False

        try:
            os.kill(self._process_id, 0)
            return True
        except ProcessLookupError:
            return False

    def update_window_size(self, rows: int, cols: int) -> None:
        """Update PTY window size."""
        if self._main_fd is not None:
            # Create window size structure
            win_size = struct.pack('HHHH', rows, cols, 0, 0)
            try:
                fcntl.ioctl(self._main_fd, termios.TIOCSWINSZ, win_size)
                # Signal process group
                if self._process_id:
                    os.killpg(os.getpgid(self._process_id), signal.SIGWINCH)
            except OSError as e:
                self._logger.warning(f"Failed to update window size: {e}")

    async def read_data(self, size: int) -> bytes:
        """
        Read data from Unix terminal.

        Returns:
            bytes: Data read from terminal, or empty bytes if no data available.

        Raises:
            EOFError: If pipe is closed/EOF reached
            OSError: On actual errors
        """
        loop = asyncio.get_event_loop()
        try:
            # Wait for data with select
            r, _w, _e = await loop.run_in_executor(
                None,
                select.select,
                [self._main_fd],
                [],
                [],
                0.1
            )

            if not r:
                return b''  # No data available within timeout

            # Read now that select indicated data is available
            data = os.read(self._main_fd, size)
            if not data:  # Empty read after select indicated data means EOF
                raise EOFError("Terminal pipe closed")

            return data

        except BlockingIOError:  # EAGAIN/EWOULDBLOCK
            return b''  # No data available right now

        except OSError as e:
            if not self._running:
                return b''

            raise

    async def write_data(self, data: bytes) -> None:
        """Write data to Unix terminal."""
        if self._main_fd is not None and self._running:
            loop = asyncio.get_event_loop()
            await loop.run_in_executor(None, lambda: os.write(self._main_fd, data))

    def transfer_to(self, other: 'TerminalBase') -> None:
        """Transfer Unix terminal ownership."""
        other._process_id = self._process_id
        other._process_name = self._process_name
        other._main_fd = self._main_fd
        other._running = True

        # Clear our state without closing fd
        self._process_id = None
        self._process_name = ""
        self._main_fd = None
        self._running = False
